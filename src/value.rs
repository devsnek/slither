use crate::agent::Agent;
use crate::vm::{evaluate_at, Compiled, ExecutionContext, LexicalEnvironment};
use gc::{Gc, GcCell};
use indexmap::IndexMap;
use num::BigInt;
use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicUsize, Ordering};

macro_rules! custom_trace {
    ($this:ident, $body:expr) => {
        #[inline]
        unsafe fn trace(&self) {
            #[inline]
            unsafe fn mark<T: gc::Trace>(it: &T) {
                gc::Trace::trace(it);
            }
            let $this = self;
            $body
        }
        #[inline]
        unsafe fn root(&self) {
            #[inline]
            unsafe fn mark<T: gc::Trace>(it: &T) {
                gc::Trace::root(it);
            }
            let $this = self;
            $body
        }
        #[inline]
        unsafe fn unroot(&self) {
            #[inline]
            unsafe fn mark<T: gc::Trace>(it: &T) {
                gc::Trace::unroot(it);
            }
            let $this = self;
            $body
        }
        #[inline]
        fn finalize_glue(&self) {
            gc::Finalize::finalize(self);
            #[inline]
            fn mark<T: gc::Trace>(it: &T) {
                gc::Trace::finalize_glue(it);
            }
            let $this = self;
            $body
        }
    }
}

pub fn new_error(message: &str) -> Value {
    let mut m = IndexMap::new();
    m.insert(
        ObjectKey::from("message"),
        Value::String(message.to_string()),
    );
    Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::Ordinary,
        properties: GcCell::new(m),
        prototype: Value::Null,
    }))
}

type BuiltinFunction = fn(&Agent, &ExecutionContext, Vec<Value>) -> Result<Value, Value>;

#[derive(Finalize)]
pub enum ObjectKind {
    Ordinary,
    Array,
    Boolean(bool),
    String(String),
    Float(f64),
    Integer(BigInt),
    Custom(Gc<GcCell<HashMap<String, Value>>>), // internal slots
    CompiledFunction(
        u8,
        usize,
        *const Compiled,
        bool,
        Gc<GcCell<LexicalEnvironment>>,
    ), // paramc, index, compiled, inherits this, env
    BuiltinFunction(BuiltinFunction, Gc<GcCell<HashMap<String, Value>>>),
}

impl std::fmt::Debug for ObjectKind {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        let r = match self {
            ObjectKind::Ordinary => "Ordinary".to_string(),
            ObjectKind::Array => "Array".to_string(),
            ObjectKind::Boolean(b) => format!("Boolean({})", b),
            ObjectKind::String(s) => format!("String({})", s),
            ObjectKind::Float(f) => format!("Float({})", f),
            ObjectKind::Integer(i) => format!("Integer({})", i),
            ObjectKind::Custom(..) => "Custom".to_string(),
            ObjectKind::CompiledFunction(_, index, ..) => format!("CompiledFunction @ {}", index),
            ObjectKind::BuiltinFunction(f, ..) => format!("BuiltinFunction @ {:p}", f),
        };
        write!(fmt, "{}", r)
    }
}

// empty impl
unsafe impl gc::Trace for ObjectKind {
    custom_trace!(this, {
        match this {
            ObjectKind::CompiledFunction(_, _, _, _, env) => mark(env),
            ObjectKind::Custom(slots) => mark(slots),
            ObjectKind::BuiltinFunction(_, slots) => mark(slots),
            _ => {}
        }
    });
}

#[derive(Trace, Finalize, Debug)]
pub struct ObjectInfo {
    pub kind: ObjectKind,
    pub properties: GcCell<IndexMap<ObjectKey, Value>>,
    pub prototype: Value,
}

impl ObjectInfo {
    pub fn get(&self, property: ObjectKey) -> Result<Value, Value> {
        match self.properties.borrow().get(&property) {
            Some(v) => Ok(v.clone()),
            _ => {
                if let ObjectKey::Symbol(Symbol(_, true, _)) = property {
                    // don't traverse for private symbol
                    Ok(Value::Null)
                } else {
                    match &self.prototype {
                        Value::Object(oo) => oo.get(property),
                        Value::Null => Ok(Value::Null),
                        _ => unreachable!(),
                    }
                }
            }
        }
    }

    pub fn set(
        &self,
        property: ObjectKey,
        value: Value,
        receiver: Gc<ObjectInfo>,
    ) -> Result<Value, Value> {
        match self {
            ObjectInfo {
                kind: ObjectKind::Array,
                ..
            } if property == ObjectKey::from("length") => {
                if let Value::Integer(int_len) = value {
                    let old_len = self.get(ObjectKey::from("length"))?;
                    let mut old_len = match old_len {
                        Value::Integer(n) => n,
                        Value::Null => BigInt::from(0),
                        _ => unreachable!(),
                    };
                    if int_len >= old_len {
                        self.properties
                            .borrow_mut()
                            .insert(property, Value::Integer(int_len.clone()));
                    } else if int_len < old_len {
                        while int_len < old_len {
                            old_len -= 1;
                            self.properties
                                .borrow_mut()
                                .remove(&ObjectKey::from(old_len.clone()));
                        }
                    } else {
                        // nothing!
                    }
                    Ok(Value::Integer(int_len))
                } else {
                    Err(new_error("invalid array length"))
                }
            }
            _ => {
                let mut own = false;
                if let ObjectKey::Symbol(Symbol(_, true, _)) = property {
                    own = true;
                }
                if own || self.properties.borrow().contains_key(&property) {
                    receiver
                        .properties
                        .borrow_mut()
                        .insert(property, value.clone());
                    Ok(value)
                } else {
                    match &self.prototype {
                        Value::Object(oo) => oo.set(property, value, receiver),
                        Value::Null => {
                            receiver
                                .properties
                                .borrow_mut()
                                .insert(property, value.clone());
                            Ok(value)
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }
    }

    fn keys(&self) -> Vec<ObjectKey> {
        let mut keys = Vec::new();
        let entries = self.properties.borrow();
        for key in entries.keys() {
            keys.push(key.clone());
        }
        keys
    }
}

static SYMBOL_COUNTER: AtomicUsize = AtomicUsize::new(0);
#[derive(Debug, Clone, Trace, Finalize, Hash, PartialEq, Eq)]
pub struct Symbol(pub usize, pub bool, pub Option<String>); // id, private

impl Symbol {
    pub fn new(private: bool, desc: Option<String>) -> Symbol {
        let s = Symbol(SYMBOL_COUNTER.load(Ordering::Relaxed), private, desc);
        SYMBOL_COUNTER.fetch_add(1, Ordering::Relaxed);
        s
    }
}

#[derive(Trace, Finalize, Hash, Debug, PartialEq, Eq, Clone)]
pub enum ObjectKey {
    String(String),
    Symbol(Symbol),
}

impl std::fmt::Display for ObjectKey {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ObjectKey::String(s) => write!(fmt, "{}", s),
            ObjectKey::Symbol(Symbol(_, _, d)) => {
                if let Some(s) = d {
                    write!(fmt, "[Symbol({})]", s)
                } else {
                    write!(fmt, "[Symbol()]")
                }
            }
        }
    }
}

impl From<String> for ObjectKey {
    fn from(s: String) -> Self {
        ObjectKey::String(s)
    }
}

impl From<&str> for ObjectKey {
    fn from(s: &str) -> Self {
        ObjectKey::String(s.to_string())
    }
}

impl From<u32> for ObjectKey {
    fn from(n: u32) -> Self {
        ObjectKey::String(n.to_string())
    }
}

impl From<i32> for ObjectKey {
    fn from(n: i32) -> Self {
        ObjectKey::String(n.to_string())
    }
}

impl From<BigInt> for ObjectKey {
    fn from(n: BigInt) -> Self {
        ObjectKey::String(n.to_string())
    }
}

#[derive(Debug, Clone, Finalize)]
pub enum Value {
    Empty, // used to replace default params
    Null,
    True,
    False,
    String(String),
    Float(f64),
    Integer(BigInt),
    Symbol(Symbol),
    Object(Gc<ObjectInfo>),
    List(Gc<GcCell<VecDeque<Value>>>),
    EnvironmentReference(Gc<GcCell<LexicalEnvironment>>, String),
    ValueReference(Box<Value>, ObjectKey),
}

unsafe impl gc::Trace for Value {
    custom_trace!(this, {
        match this {
            Value::Empty
            | Value::Null
            | Value::True
            | Value::False
            | Value::String(_)
            | Value::Float(_)
            | Value::Integer(_)
            | Value::Symbol(_) => {}
            Value::Object(o) => mark(o),
            Value::List(list) => mark(list),
            Value::EnvironmentReference(env, ..) => mark(env),
            Value::ValueReference(v, ..) => mark(v),
        }
    });
}

impl Value {
    pub fn new_symbol(desc: Option<String>) -> Value {
        Value::Symbol(Symbol::new(false, desc))
    }

    pub fn new_list() -> Value {
        Value::List(Gc::new(GcCell::new(VecDeque::new())))
    }

    pub fn type_of(&self) -> &str {
        match &self {
            Value::Null => "null",
            Value::Float(_) => "float",
            Value::Integer(_) => "integer",
            Value::String(_) => "string",
            Value::Object(o) => match o.kind {
                ObjectKind::CompiledFunction(..) | ObjectKind::BuiltinFunction(..) => "function",
                _ => "object",
            },
            _ => unreachable!(),
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Null => false,
            Value::True => true,
            Value::False => false,
            Value::String(s) => s.chars().count() > 0,
            Value::Float(n) => *n != 0.0f64,
            Value::Integer(n) => *n != BigInt::from(0),
            Value::Object(_) => true,
            _ => unreachable!(),
        }
    }

    pub fn to_object_key(&self) -> Result<ObjectKey, Value> {
        match self {
            Value::Symbol(s) => Ok(ObjectKey::Symbol(s.clone())),
            Value::String(s) => Ok(ObjectKey::String(s.clone())),
            Value::Float(n) => Ok(ObjectKey::String(n.to_string())),
            Value::Integer(n) => Ok(ObjectKey::String(n.to_string())),
            _ => Err(new_error("cannot convert to object key")),
        }
    }

    pub fn to_object(&self, a: &Agent) -> Result<Value, Value> {
        match self {
            Value::Null => Err(new_error("cannot convert null to object")),
            Value::True => Ok(new_boolean_object(a, true)),
            Value::False => Ok(new_boolean_object(a, false)),
            Value::Object(_) => Ok(self.clone()),
            Value::Float(n) => Ok(new_float_object(a, *n)),
            Value::Integer(i) => Ok(new_integer_object(a, i.clone())),
            Value::String(s) => Ok(new_string_object(a, s.clone())),
            _ => unreachable!(),
        }
    }

    pub fn has_slot(&self, property: &str) -> bool {
        if let Value::Object(o) = self {
            match &o.kind {
                ObjectKind::Custom(cell) | ObjectKind::BuiltinFunction(_, cell) => {
                    cell.borrow().contains_key(property)
                }
                _ => false,
            }
        } else {
            false
        }
    }

    pub fn get_slot(&self, property: &str) -> Value {
        if let Value::Object(o) = self {
            match &o.kind {
                ObjectKind::Custom(cell) | ObjectKind::BuiltinFunction(_, cell) => {
                    match cell.borrow().get(property) {
                        Some(v) => v.clone(),
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        } else {
            panic!()
        }
    }

    pub fn set_slot(&self, property: &str, value: Value) {
        if let Value::Object(o) = self {
            match &o.kind {
                ObjectKind::Custom(cell) | ObjectKind::BuiltinFunction(_, cell) => {
                    cell.borrow_mut().insert(property.to_string(), value);
                }
                _ => panic!(),
            }
        } else {
            panic!()
        }
    }

    pub fn set(&self, property: &ObjectKey, value: Value) -> Result<Value, Value> {
        match self {
            Value::Object(o) => o.set(property.clone(), value, o.clone()),
            _ => Err(new_error("base must be an object")),
        }
    }

    pub fn get(&self, property: &ObjectKey) -> Result<Value, Value> {
        match self {
            Value::Object(o) => o.get(property.clone()),
            _ => Err(new_error("base must be an object")),
        }
    }

    pub fn keys(&self) -> Result<Vec<ObjectKey>, Value> {
        match self {
            Value::Object(o) => Ok(o.keys()),
            _ => Err(new_error("base must be an object")),
        }
    }

    pub fn call(&self, agent: &Agent, this: Value, args: Vec<Value>) -> Result<Value, Value> {
        match self {
            Value::Object(o) => match &o.kind {
                ObjectKind::CompiledFunction(paramc, index, compiled, inherits_this, env) => {
                    let paramc = *paramc;
                    let index = *index;
                    let ctx = ExecutionContext::new(LexicalEnvironment::new(Some(env.clone())));
                    if !inherits_this {
                        ctx.borrow().environment.borrow_mut().this = Some(this);
                    }
                    ctx.borrow_mut().function = Some(self.clone());
                    let mut stack = Vec::new();
                    for i in (0..paramc).rev() {
                        stack.push(args.get(i as usize).unwrap_or(&Value::Empty).clone());
                    }
                    unsafe {
                        let compiled = &**compiled;
                        Ok(evaluate_at(
                            agent,
                            compiled,
                            index,
                            &mut stack,
                            &mut vec![ctx],
                            &mut vec![compiled.code.len()],
                        )?
                        .unwrap_or(Value::Null))
                    }
                }
                ObjectKind::BuiltinFunction(f, _) => {
                    let ctx = ExecutionContext::new(LexicalEnvironment::new(None));
                    let mut ctx = ctx.borrow_mut();
                    ctx.environment.borrow_mut().this = Some(this);
                    ctx.function = Some(self.clone());
                    f(agent, &ctx, args)
                }
                _ => Err(new_error("not a function")),
            },
            _ => Err(new_error("not a function")),
        }
        // Err(new_error("unimplemented"))
    }

    pub fn construct(&self, agent: &Agent, args: Vec<Value>) -> Result<Value, Value> {
        match self {
            Value::Object(o) => {
                let mut prototype = o.get(ObjectKey::from("prototype"))?;
                if prototype.type_of() != "object" {
                    prototype = agent.intrinsics.object_prototype.clone();
                }
                let this = new_object(prototype);
                match &o.kind {
                    ObjectKind::CompiledFunction(..) | ObjectKind::BuiltinFunction(..) => {
                        let r = self.call(agent, this.clone(), args)?;
                        if r.type_of() == "object" {
                            Ok(r)
                        } else {
                            Ok(this)
                        }
                    }
                    _ => Err(new_error("not a function")),
                }
            }
            _ => Err(new_error("not a function")),
        }
    }
}

fn inspect(value: &Value, indent: usize) -> Result<String, Value> {
    match value {
        Value::Null => Ok("null".to_string()),
        Value::True => Ok("true".to_string()),
        Value::False => Ok("false".to_string()),
        Value::Float(n) => Ok(format!("{}f", n)),
        Value::Integer(n) => Ok(format!("{}i", n)),
        Value::String(s) => Ok(format!("'{}'", s)),
        Value::Symbol(Symbol(_, _, d)) => {
            if let Some(s) = d {
                Ok(format!("Symbol({})", s))
            } else {
                Ok("Symbol()".to_string())
            }
        }
        Value::Object(o) => {
            let keys = value.keys()?;
            let array = match o.kind {
                ObjectKind::Array => true,
                _ => false,
            };
            if keys.is_empty() {
                return Ok(if array { "[]" } else { "{}" }.to_string());
            }
            let mut out = String::from(if array { "[" } else { "{" });
            for key in keys {
                out += &format!(
                    "\n{}{}: {},",
                    "  ".repeat(indent + 1),
                    key,
                    inspect(&value.get(&key)?, indent + 1)?
                )
            }
            out += &format!("\n{}{}", "  ".repeat(indent), if array { "]" } else { "}" });
            Ok(out)
        }
        _ => unreachable!(),
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match inspect(self, 0) {
            Ok(s) => write!(fmt, "{}", s),
            Err(_e) => Err(std::fmt::Error),
        }
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::String(s)
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Value::String(s.to_string())
    }
}

#[inline]
pub fn ref_eq<T>(thing: &T, other: &T) -> bool {
    (thing as *const T) == (other as *const T)
}

impl PartialEq for Value {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match self {
            Value::Empty => match other {
                Value::Empty => true,
                _ => false,
            },
            Value::Null => match other {
                Value::Null => true,
                _ => false,
            },
            Value::True => match other {
                Value::True => true,
                _ => false,
            },
            Value::False => match other {
                Value::False => true,
                _ => false,
            },
            Value::String(s) => match &other {
                Value::String(vs) => s == vs,
                _ => false,
            },
            Value::Float(n) => match &other {
                Value::Float(vn) => n == vn,
                _ => false,
            },
            Value::Integer(n) => match &other {
                Value::Integer(vn) => n == vn,
                _ => false,
            },
            Value::Object(o) => match &other {
                Value::Object(vo) => ref_eq(&*o.properties.borrow(), &*vo.properties.borrow()),
                _ => false,
            },
            _ => unreachable!(),
        }
    }
}

impl std::convert::From<crate::parser::Error> for Value {
    fn from(_e: crate::parser::Error) -> Self {
        new_error("parsing error")
    }
}

pub fn new_object(proto: Value) -> Value {
    Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::Ordinary,
        properties: GcCell::new(IndexMap::new()),
        prototype: proto,
    }))
}

pub fn new_array(agent: &Agent) -> Value {
    Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::Array,
        properties: GcCell::new(IndexMap::new()),
        prototype: agent.intrinsics.array_prototype.clone(),
    }))
}

pub fn new_custom_object(proto: Value) -> Value {
    Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::Custom(Gc::new(GcCell::new(HashMap::new()))),
        properties: GcCell::new(IndexMap::new()),
        prototype: proto,
    }))
}

pub fn new_compiled_function(
    agent: &Agent,
    argc: u8,
    pc_index: usize,
    compiled: *const Compiled,
    inherits_this: bool,
    env: Gc<GcCell<LexicalEnvironment>>,
) -> Value {
    Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::CompiledFunction(argc, pc_index, compiled, inherits_this, env),
        properties: GcCell::new(IndexMap::new()),
        prototype: agent.intrinsics.function_prototype.clone(),
    }))
}

pub fn new_builtin_function(agent: &Agent, f: BuiltinFunction) -> Value {
    Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::BuiltinFunction(f, Gc::new(GcCell::new(HashMap::new()))),
        properties: GcCell::new(IndexMap::new()),
        prototype: agent.intrinsics.function_prototype.clone(),
    }))
}

pub fn new_boolean_object(agent: &Agent, v: bool) -> Value {
    Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::Boolean(v),
        properties: GcCell::new(IndexMap::new()),
        prototype: agent.intrinsics.boolean_prototype.clone(),
    }))
}

pub fn new_string_object(agent: &Agent, v: String) -> Value {
    Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::String(v),
        properties: GcCell::new(IndexMap::new()),
        prototype: agent.intrinsics.string_prototype.clone(),
    }))
}

pub fn new_float_object(agent: &Agent, v: f64) -> Value {
    Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::Float(v),
        properties: GcCell::new(IndexMap::new()),
        prototype: agent.intrinsics.float_prototype.clone(),
    }))
}

pub fn new_integer_object(agent: &Agent, v: BigInt) -> Value {
    Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::Integer(v),
        properties: GcCell::new(IndexMap::new()),
        prototype: agent.intrinsics.float_prototype.clone(),
    }))
}
