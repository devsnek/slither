use crate::agent::Agent;
use crate::interpreter::Context;
use crate::value::{ObjectKey, ObjectKind, Value};

fn to_string(agent: &Agent, _: Vec<Value>, ctx: &Context) -> Result<Value, Value> {
    let this = ctx.get_this(agent)?;

    match this {
        Value::Object(o) => match o.kind {
            ObjectKind::Number(d) => Ok(Value::String(d.to_string())),
            _ => Err(Value::new_error(agent, "invalid receiver")),
        },
        _ => Err(Value::new_error(agent, "invalid receiver")),
    }
}

pub fn create_number_prototype(agent: &Agent) -> Value {
    let proto = Value::new_object(agent.intrinsics.object_prototype.clone());

    proto
        .set(
            agent,
            ObjectKey::from("toString"),
            Value::new_builtin_function(agent, to_string),
        )
        .unwrap();

    proto
}
