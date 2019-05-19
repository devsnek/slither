use crate::value::Args;
use crate::{Agent, Value};
use std::collections::HashMap;

fn print(args: Args) -> Result<Value, Value> {
    let mut inspected = Vec::with_capacity(args.args().len());
    for v in args.args() {
        inspected.push(Value::inspect(args.agent(), &v));
    }
    println!("{}", inspected.join(" "));
    Ok(Value::Null)
}

pub(crate) fn create(agent: &Agent) -> HashMap<String, Value> {
    let mut module = HashMap::new();
    module.insert(
        "print".to_string(),
        Value::new_builtin_function(agent, print),
    );

    module
}
