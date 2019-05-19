use crate::agent::Agent;
use crate::value::{Args, ObjectKey, Value};

fn to_string(args: Args) -> Result<Value, Value> {
    let this = args.this();

    let name = match this.get(args.agent(), ObjectKey::from("name"))? {
        Value::String(s) => s,
        _ => return Err(Value::new_error(args.agent(), "Invalid error object")),
    };
    let message = match this.get(args.agent(), ObjectKey::from("message"))? {
        Value::String(s) => format!(": {}", s),
        Value::Null => "".to_string(),
        _ => return Err(Value::new_error(args.agent(), "Invalid error object")),
    };

    Ok(Value::from(format!("{}{}", name, message)))
}

pub fn create_error_prototype(agent: &Agent) -> Value {
    let proto = Value::new_object(agent.intrinsics.object_prototype.clone());

    proto
        .set(
            agent,
            ObjectKey::well_known_symbol("toString"),
            Value::new_builtin_function(agent, to_string),
        )
        .unwrap();

    proto
        .set(agent, ObjectKey::from("name"), Value::from("Error"))
        .unwrap();

    proto
}
