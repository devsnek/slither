use crate::value::Value;

pub fn create_string_prototype(object_prototype: Value) -> Value {
    Value::new_object(object_prototype)
}
