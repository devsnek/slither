use crate::module::{Agent, ExecutionContext, LexicalEnvironment};
use crate::parser::Node;
use gc::{Gc, GcCell};
use num::BigInt;
use std::collections::{HashMap, VecDeque};
pub use std::sync::atomic::{AtomicUsize, Ordering};

pub fn new_error(message: &str) -> Value {
    let mut m = HashMap::new();
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

type BuiltinFunction = fn(&Agent, &mut ExecutionContext, Vec<Value>) -> Result<Value, Value>;
#[derive(Finalize)]
pub struct BuiltinFunctionWrap(pub BuiltinFunction);

impl std::fmt::Debug for BuiltinFunctionWrap {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "builtin function")
    }
}

#[derive(Finalize, Debug)]
pub enum ObjectKind {
    Ordinary,
    Array,
    Boolean(bool),
    String(String),
    Float(f64),
    Function(Vec<String>, Box<Node>, Gc<GcCell<LexicalEnvironment>>), // args, body, parent_env
    Custom(Gc<GcCell<HashMap<String, Value>>>),                       // internal slots
    BuiltinFunction(BuiltinFunctionWrap, Gc<GcCell<HashMap<String, Value>>>), // fn, internal slots
}

// empty impl
unsafe impl gc::Trace for ObjectKind {
    #[inline]
    unsafe fn trace(&self) {
        match self {
            ObjectKind::Function(_, _, v) => gc::Trace::trace(v),
            ObjectKind::Custom(v) => gc::Trace::trace(v),
            ObjectKind::BuiltinFunction(_, v) => gc::Trace::trace(v),
            _ => {}
        }
    }
    #[inline]
    unsafe fn root(&self) {
        match self {
            ObjectKind::Function(_, _, v) => gc::Trace::root(v),
            ObjectKind::Custom(v) => gc::Trace::root(v),
            ObjectKind::BuiltinFunction(_, v) => gc::Trace::root(v),
            _ => {}
        }
    }
    #[inline]
    unsafe fn unroot(&self) {
        match self {
            ObjectKind::Function(_, _, v) => gc::Trace::unroot(v),
            ObjectKind::Custom(v) => gc::Trace::unroot(v),
            ObjectKind::BuiltinFunction(_, v) => gc::Trace::unroot(v),
            _ => {}
        }
    }
    #[inline]
    fn finalize_glue(&self) {
        gc::Trace::finalize(self);
        match self {
            ObjectKind::Function(_, _, v) => gc::Trace::finalize_glue(v),
            ObjectKind::Custom(v) => gc::Trace::finalize_glue(v),
            ObjectKind::BuiltinFunction(_, v) => gc::Trace::finalize_glue(v),
            _ => {}
        }
    }
}

#[derive(Trace, Finalize, Debug)]
pub struct ObjectInfo {
    pub kind: ObjectKind,
    pub properties: GcCell<HashMap<ObjectKey, Value>>,
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
                    if int_len > old_len {
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

impl From<BigInt> for ObjectKey {
    fn from(n: BigInt) -> Self {
        ObjectKey::String(n.to_string())
    }
}

#[derive(Debug, Clone, Finalize)]
pub enum Value {
    Null,
    True,
    False,
    String(String),
    Float(f64),
    Integer(BigInt),
    Symbol(Symbol),
    Object(Gc<ObjectInfo>),
    ReturnCompletion(Box<Value>),
    List(Gc<GcCell<VecDeque<Value>>>),
}

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

unsafe impl gc::Trace for Value {
    custom_trace!(this, {
        match this {
            Value::Null
            | Value::True
            | Value::False
            | Value::String(_)
            | Value::Float(_)
            | Value::Integer(_)
            | Value::Symbol(_) => {}
            Value::Object(o) => mark(o),
            Value::ReturnCompletion(b) => mark(b),
            Value::List(list) => mark(list),
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
                ObjectKind::Function(_, _, _) | ObjectKind::BuiltinFunction(_, _) => "function",
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
            _ => Err(new_error("cannot convert to object key")),
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
        properties: GcCell::new(HashMap::new()),
        prototype: proto,
    }))
}

pub fn new_custom_object(proto: Value) -> Value {
    Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::Custom(Gc::new(GcCell::new(HashMap::new()))),
        properties: GcCell::new(HashMap::new()),
        prototype: proto,
    }))
}

pub fn new_array(agent: &Agent) -> Value {
    Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::Array,
        properties: GcCell::new(HashMap::new()),
        prototype: agent.intrinsics.array_prototype.clone(),
    }))
}

pub fn new_boolean_object(agent: &Agent, v: bool) -> Value {
    Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::Boolean(v),
        properties: GcCell::new(HashMap::new()),
        prototype: agent.intrinsics.boolean_prototype.clone(),
    }))
}

pub fn new_string_object(agent: &Agent, v: String) -> Value {
    Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::String(v),
        properties: GcCell::new(HashMap::new()),
        prototype: agent.intrinsics.string_prototype.clone(),
    }))
}

pub fn new_float_object(agent: &Agent, v: f64) -> Value {
    Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::Float(v),
        properties: GcCell::new(HashMap::new()),
        prototype: agent.intrinsics.float_prototype.clone(),
    }))
}

pub fn new_function(
    agent: &Agent,
    args: Vec<String>,
    body: Box<Node>,
    env: Gc<GcCell<LexicalEnvironment>>,
) -> Value {
    Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::Function(args, body, env),
        properties: GcCell::new(HashMap::new()),
        prototype: agent.intrinsics.function_prototype.clone(),
    }))
}

pub fn new_builtin_function(agent: &Agent, bfn: BuiltinFunction) -> Value {
    Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::BuiltinFunction(
            BuiltinFunctionWrap(bfn),
            Gc::new(GcCell::new(HashMap::new())),
        ),
        properties: GcCell::new(HashMap::new()),
        prototype: agent.intrinsics.function_prototype.clone(),
    }))
}
