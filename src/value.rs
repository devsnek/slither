use crate::module::Agent;
use crate::parser::Node;
use gc::{Gc, GcCell};
use std::collections::HashMap;

pub fn new_error(message: &str) -> Value {
    let mut m = HashMap::new();
    m.insert("message".to_string(), Value::String(message.to_string()));
    Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::Ordinary,
        properties: GcCell::new(m),
        prototype: Value::Null,
    }))
}

pub struct BuiltinFunction(pub fn(&Agent, Vec<Value>) -> Result<Value, Value>);

impl std::fmt::Debug for BuiltinFunction {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "builtin function")
    }
}

#[derive(Finalize, Debug)]
pub enum ObjectKind {
    Ordinary,
    Boolean(bool),
    String(String),
    Number(f64),
    Function(Vec<String>, Box<Node>), // args, body
    BuiltinFunction(BuiltinFunction),
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
    pub properties: GcCell<HashMap<String, Value>>,
    pub prototype: Value,
}

impl ObjectInfo {
    pub fn get(&self, property: String) -> Result<Value, Value> {
        match self.properties.borrow().get(&property) {
            Some(v) => Ok(v.clone()),
            _ => match &self.prototype {
                Value::Object(oo) => oo.get(property),
                Value::Null => Ok(Value::Null),
                _ => unreachable!(),
            },
        }
    }

    pub fn set(
        &self,
        property: String,
        value: Value,
        receiver: Gc<ObjectInfo>,
    ) -> Result<Value, Value> {
        if self.properties.borrow().contains_key(&property) {
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

#[derive(Debug, Clone, Trace, Finalize)]
pub enum Value {
    Null,
    True,
    False,
    String(String),
    Number(f64),
    Object(Gc<ObjectInfo>),
    ReturnCompletion(Box<Value>),
}

impl Value {
    pub fn type_of(&self) -> &str {
        match &self {
            Value::Null => "null",
            Value::Number(_) => "number",
            Value::String(_) => "string",
            Value::Object(o) => match o.kind {
                ObjectKind::Ordinary
                | ObjectKind::Boolean(_)
                | ObjectKind::String(_)
                | ObjectKind::Number(_) => "object",
                ObjectKind::Function(_, _) | ObjectKind::BuiltinFunction(_) => "function",
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

    pub fn to_string(&self) -> Result<String, Value> {
        match self {
            Value::Null => Ok("null".to_string()),
            Value::True => Ok("true".to_string()),
            Value::False => Ok("false".to_string()),
            Value::String(s) => Ok(s.clone()),
            Value::Number(n) => Ok(n.to_string()),
            Value::Object(_) => Ok("[object Object]".to_string()),
            _ => Err(new_error("cannot convert to string")),
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

pub fn new_function(args: Vec<String>, body: Box<Node>, proto: Value) -> Value {
    Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::Function(args, body),
        properties: GcCell::new(HashMap::new()),
        prototype: proto,
    }))
}
