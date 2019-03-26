use crate::agent::Agent;
use crate::interpreter::Context;
use crate::value::{ObjectKey, Value};

fn symbol(agent: &Agent, args: Vec<Value>, _ctx: &Context) -> Result<Value, Value> {
    let desc = match args.get(0) {
        Some(Value::String(s)) => Ok(Some(s.clone())),
        Some(_) => Err(Value::new_error(agent, "invalid description")),
        None => Ok(None),
    }?;
    Ok(Value::new_symbol(desc))
}

fn private(agent: &Agent, args: Vec<Value>, _ctx: &Context) -> Result<Value, Value> {
    let desc = match args.get(0) {
        Some(Value::String(s)) => Ok(Some(s.clone())),
        Some(_) => Err(Value::new_error(agent, "invalid description")),
        None => Ok(None),
    }?;
    Ok(Value::new_private_symbol(desc))
}

pub fn create_symbol(agent: &Agent) -> Value {
    let s = Value::new_builtin_function(agent, symbol);

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
        Value::new_builtin_function(agent, private),
    )
    .expect("failed to set private on symbol constructor");

    s
}
