use crate::value::Value;

pub fn create_function_prototype(object_prototype: Value) -> Value {
    Value::new_object(object_prototype)
}
