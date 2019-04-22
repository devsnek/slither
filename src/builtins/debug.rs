use crate::interpreter::Context;
use crate::{Agent, Value};
use std::collections::HashMap;

fn print(agent: &Agent, args: Vec<Value>, _: &Context) -> Result<Value, Value> {
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
