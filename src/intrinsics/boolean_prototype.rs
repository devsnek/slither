use crate::agent::Agent;
use crate::value::{ObjectKey, ObjectKind, Value};
use crate::vm::ExecutionContext;

fn to_string(_: &Agent, ctx: &ExecutionContext, _: Vec<Value>) -> Result<Value, Value> {
    match ctx.environment.borrow().get_this()? {
        Value::Object(o) => match o.kind {
            ObjectKind::Boolean(b) => Ok(Value::String(b.to_string())),
            _ => Err(Value::new_error("invalid receiver")),
        },
        _ => Err(Value::new_error("invalid receiver")),
    }
}

pub fn create_boolean_prototype(agent: &Agent) -> Value {
    let proto = Value::new_object(agent.intrinsics.object_prototype.clone());

    proto
        .set(
            &ObjectKey::from("toString"),
            Value::new_builtin_function(agent, to_string),
        )
        .unwrap();

    proto
}
