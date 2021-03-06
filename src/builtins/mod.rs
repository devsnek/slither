use crate::agent::Agent;
use crate::value::Value;
use std::collections::HashMap;

mod r#async;
pub(crate) mod fs;
mod json;
mod math;
pub(crate) mod net;
pub(crate) mod timers;

pub(crate) fn create(agent: &Agent) -> HashMap<String, HashMap<String, Value>> {
    let mut builtins = HashMap::new();

    builtins.insert("timers".to_string(), timers::create(agent));
    builtins.insert("fs".to_string(), fs::create(agent));
    builtins.insert("net".to_string(), net::create(agent));
    builtins.insert("math".to_string(), math::create(agent));
    builtins.insert("async".to_string(), r#async::create(agent));
    builtins.insert("json".to_string(), json::create(agent));

    builtins
}
