use crate::agent::Agent;
use crate::value::{ObjectKey, Value};
use crate::vm::ExecutionContext;

fn symbol(agent: &Agent, _ctx: &ExecutionContext, args: Vec<Value>) -> Result<Value, Value> {
    let desc = match args.get(0) {
        Some(Value::String(s)) => Ok(Some(s.clone())),
        Some(_) => Err(Value::new_error(agent, "invalid description")),
        None => Ok(None),
    }?;
    Ok(Value::new_symbol(desc))
}

pub fn create_symbol(agent: &Agent) -> Value {
    let s = Value::new_builtin_function(agent, symbol);

    s.set(
        agent,
        &ObjectKey::from("prototype"),
        agent.intrinsics.symbol_prototype.clone(),
    )
    .expect("failed to set prototype on promise constructor");
    agent
        .intrinsics
        .symbol_prototype
        .set(agent, &ObjectKey::from("constructor"), s.clone())
        .expect("failed to set constructor on promise prototype");

    s
}
