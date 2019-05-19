use crate::value::Value;

pub(crate) fn create_symbol_prototype(object_prototype: Value) -> Value {
    Value::new_object(object_prototype)
}
