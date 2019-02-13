use crate::intrinsics::promise::{PromiseReaction, PromiseState};
use crate::module::{Agent, ExecutionContext, LexicalEnvironment};
use crate::parser::Node;
use gc::{Gc, GcCell};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
pub use std::sync::atomic::{AtomicU64, Ordering};

pub fn new_error(message: &str) -> Value {
    let mut m = HashMap::new();
    m.insert(ObjectKey::from("message"), Value::String(message.to_string()));
    Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::Ordinary,
        properties: GcCell::new(m),
        prototype: Value::Null,
    }))
}

type BuiltinFunction = fn(&Agent, &mut ExecutionContext, Vec<Value>) -> Result<Value, Value>;
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
    Number(f64),
    Function(Vec<String>, Box<Node>, Rc<RefCell<LexicalEnvironment>>), // args, body
    BuiltinFunction(BuiltinFunctionWrap),
    Promise(
        PromiseState,
        Value,
        Vec<PromiseReaction>,
        Vec<PromiseReaction>,
    ), // state, result, fulfill reactions, reject reactions
}

// empty impl
unsafe impl gc::Trace for ObjectKind {
    #[inline]
    unsafe fn trace(&self) {}
    #[inline]
    unsafe fn root(&self) {}
    #[inline]
    unsafe fn unroot(&self) {}
    #[inline]
    fn finalize_glue(&self) {}
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
                if let ObjectKey::Symbol(Symbol(_, true)) = property { // don't traverse for private symbol
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
                if let Value::Number(number_len) = value {
                    let new_len = f64::from(number_len as u32);
                    if new_len != number_len {
                        Err(new_error("invalid array length"))
                    } else {
                        let new_len = new_len as u32;
                        let old_len = self.get(ObjectKey::from("length"))?;
                        let mut old_len = match old_len {
                            Value::Number(n) => n as u32,
                            Value::Null => 0u32,
                            _ => unreachable!(),
                        };
                        if new_len > old_len {
                            self.properties
                                .borrow_mut()
                                .insert(property, Value::Number(f64::from(new_len)));
                        } else if new_len < old_len {
                            while new_len < old_len {
                                old_len -= 1;
                                self.properties.borrow_mut().remove(&ObjectKey::from(old_len));
                            }
                        } else {
                            // nothing!
                        }
                        Ok(Value::Number(number_len))
                    }
                } else {
                    Err(new_error("invalid array length"))
                }
            }
            _ => {
                let mut own = false;
                if let ObjectKey::Symbol(Symbol(_, true)) = property {
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

static SYMBOL_COUNTER: AtomicU64 = AtomicU64::new(0);
#[derive(Debug, Clone, Trace, Finalize, Hash, PartialEq, Eq)]
pub struct Symbol(u64, bool); // id, private

#[derive(Trace, Finalize, Hash, Debug, PartialEq, Eq)]
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

#[derive(Debug, Clone, Trace, Finalize)]
pub enum Value {
    Null,
    True,
    False,
    String(String),
    Number(f64),
    Symbol(Symbol),
    Object(Gc<ObjectInfo>),
    ReturnCompletion(Box<Value>),
}

impl Value {
    pub fn new_symbol(private: bool) -> Value {
        let s = Symbol(SYMBOL_COUNTER.load(Ordering::Relaxed), private);
        SYMBOL_COUNTER.fetch_add(1, Ordering::Relaxed);
        Value::Symbol(s)
    }

    pub fn type_of(&self) -> &str {
        match &self {
            Value::Null => "null",
            Value::Number(_) => "number",
            Value::String(_) => "string",
            Value::Object(o) => match o.kind {
                ObjectKind::Function(_, _, _) | ObjectKind::BuiltinFunction(_) => "function",
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
            Value::Number(n) => *n != 0.0f64,
            Value::Object(_) => true,
            _ => unreachable!(),
        }
    }

    pub fn to_object_key(&self) -> Result<ObjectKey, Value> {
        match self {
            Value::Symbol(s) => Ok(ObjectKey::Symbol(s.clone())),
            Value::String(s) => Ok(ObjectKey::String(s.clone())),
            Value::Number(n) => Ok(ObjectKey::String(n.to_string())),
            _ => Err(new_error("cannot convert to object key")),
        }
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
            Value::Number(n) => match &other {
                Value::Number(vn) => n == vn,
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

pub fn new_number_object(agent: &Agent, v: f64) -> Value {
    Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::Number(v),
        properties: GcCell::new(HashMap::new()),
        prototype: agent.intrinsics.number_prototype.clone(),
    }))
}

pub fn new_function(
    agent: &Agent,
    args: Vec<String>,
    body: Box<Node>,
    env: Rc<RefCell<LexicalEnvironment>>,
) -> Value {
    Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::Function(args, body, env),
        properties: GcCell::new(HashMap::new()),
        prototype: agent.intrinsics.function_prototype.clone(),
    }))
}

pub fn new_builtin_function(agent: &Agent, bfn: BuiltinFunction) -> Value {
    Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::BuiltinFunction(BuiltinFunctionWrap(bfn)),
        properties: GcCell::new(HashMap::new()),
        prototype: agent.intrinsics.function_prototype.clone(),
    }))
}
