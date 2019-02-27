use crate::agent::Agent;
use crate::value::{new_builtin_function, new_error, new_object, ObjectKey, ObjectKind, Value};
use crate::vm::ExecutionContext;

fn to_string(_: &Agent, ctx: &ExecutionContext, _: Vec<Value>) -> Result<Value, Value> {
    let this = ctx.environment.borrow().get_this()?;

    match this {
        Value::Object(o) => match o.kind {
            ObjectKind::Number(d) => Ok(Value::String(d.to_string())),
            _ => Err(new_error("invalid receiver")),
        },
        _ => Err(new_error("invalid receiver")),
    }
}

pub fn create_number_prototype(agent: &Agent, object_prototype: Value) -> Value {
    let proto = new_object(object_prototype);

    proto
        .set(
            &ObjectKey::from("toString"),
            new_builtin_function(agent, to_string),
        )
        .unwrap();

    proto
}
