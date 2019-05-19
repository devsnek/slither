use crate::agent::Agent;
use crate::value::{Args, ObjectKey, ObjectKind, Value};

fn to_string(args: Args) -> Result<Value, Value> {
    match args.this() {
        Value::Object(o) => match o.kind {
            ObjectKind::Boolean(b) => Ok(Value::from(b.to_string())),
            _ => Err(Value::new_error(args.agent(), "invalid receiver")),
        },
        _ => Err(Value::new_error(args.agent(), "invalid receiver")),
    }
}

pub(crate) fn create_boolean_prototype(agent: &Agent) -> Value {
    let proto = Value::new_object(agent.intrinsics.object_prototype.clone());

    proto
        .set(
            agent,
            ObjectKey::well_known_symbol("toString"),
            Value::new_builtin_function(agent, to_string),
        )
        .unwrap();

    proto
}
