use crate::value::{new_object, Value};

pub fn create_promise_prototype(object_prototype: Value) -> Value {
    new_object(object_prototype)
}
