use crate::value::{new_object, Value};

pub fn create_object_prototype() -> Value {
    new_object(Value::Null)
}
