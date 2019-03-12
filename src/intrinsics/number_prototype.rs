use crate::agent::Agent;
use crate::value::{ObjectKey, ObjectKind, Value};
use crate::vm::ExecutionContext;

fn to_string(_: &Agent, ctx: &ExecutionContext, _: Vec<Value>) -> Result<Value, Value> {
    let this = ctx.environment.borrow().get_this()?;

    match this {
        Value::Object(o) => match o.kind {
            ObjectKind::Number(d) => Ok(Value::String(d.to_string())),
            _ => Err(Value::new_error("invalid receiver")),
        },
        _ => Err(Value::new_error("invalid receiver")),
    }
}

pub fn create_number_prototype(agent: &Agent) -> Value {
    let proto = Value::new_object(agent.intrinsics.object_prototype.clone());

    proto
        .set(
            &ObjectKey::from("toString"),
            Value::new_builtin_function(agent, to_string),
        )
        .unwrap();

    proto
}
