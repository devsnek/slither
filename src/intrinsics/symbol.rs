use crate::agent::Agent;
use crate::value::{Args, ObjectKey, Value};

fn symbol(args: Args) -> Result<Value, Value> {
    let desc = match &args[0] {
        Value::String(s) => Ok(Some(s.clone())),
        Value::Null => Ok(None),
        _ => Err(Value::new_error(args.agent(), "invalid description")),
    }?;
    Ok(Value::new_symbol(desc))
}

fn private(args: Args) -> Result<Value, Value> {
    let desc = match &args[0] {
        Value::String(s) => Ok(Some(s.clone())),
        Value::Null => Ok(None),
        _ => Err(Value::new_error(args.agent(), "invalid description")),
    }?;
    Ok(Value::new_private_symbol(desc))
}

pub(crate) fn create_symbol(agent: &Agent) -> Value {
    let s = Value::new_builtin_function(agent, symbol, true);

    s.set(
        agent,
        ObjectKey::from("prototype"),
        agent.intrinsics.symbol_prototype.clone(),
    )
    .expect("failed to set prototype on symbol constructor");
    agent
        .intrinsics
        .symbol_prototype
        .set(agent, ObjectKey::from("constructor"), s.clone())
        .expect("failed to set constructor on symbol prototype");

    s.set(
        agent,
        ObjectKey::from("private"),
        Value::new_builtin_function(agent, private, false),
    )
    .expect("failed to set private on symbol constructor");

    s
}
