use crate::agent::Agent;
use crate::value::Value;
use crate::vm::ExecutionContext;
use std::collections::HashMap;

fn print(agent: &Agent, _: &ExecutionContext, args: Vec<Value>) -> Result<Value, Value> {
    let mut inspected = Vec::with_capacity(args.len());
    for v in args {
        inspected.push(Value::inspect(agent, &v));
    }
    println!("{}", inspected.join(" "));
    Ok(Value::Null)
}

pub fn create(agent: &Agent) -> HashMap<String, Value> {
    let mut module = HashMap::new();
    module.insert(
        "print".to_string(),
        Value::new_builtin_function(agent, print),
    );

    module
}
