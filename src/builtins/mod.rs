use crate::agent::Agent;
use crate::value::Value;
use std::collections::HashMap;

mod debug;
mod timers;

pub fn create(agent: &Agent) -> HashMap<String, HashMap<String, Value>> {
    let mut builtins = HashMap::new();

    builtins.insert("debug".to_string(), debug::create(agent));
    builtins.insert("timers".to_string(), timers::create(agent));

    builtins
}
