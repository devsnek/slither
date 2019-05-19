use crate::value::Value;

pub(crate) fn create_object_prototype() -> Value {
    Value::new_object(Value::Null)
}
