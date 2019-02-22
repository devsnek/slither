use crate::agent::Agent;
use crate::value::{new_builtin_function, Value};
use crate::vm::ExecutionContext;
use std::collections::HashMap;

fn print(_: &Agent, _: &ExecutionContext, args: Vec<Value>) -> Result<Value, Value> {
    println!(
        "{}",
        args.iter()
            .map(|v| format!("{}", v))
            .collect::<Vec<String>>()
            .join(" ")
    );
    Ok(Value::Null)
}

pub fn create_debug(agent: &Agent) -> HashMap<String, Value> {
    let mut module = HashMap::new();
    module.insert("print".to_string(), new_builtin_function(agent, print));

    module
}
