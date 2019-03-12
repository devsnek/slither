use crate::value::Value;

pub fn create_object_prototype() -> Value {
    Value::new_object(Value::Null)
}
