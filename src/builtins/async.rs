use crate::{Agent, Value};
use std::collections::HashMap;

pub(crate) fn create(agent: &Agent) -> HashMap<String, Value> {
    let mut module = HashMap::new();
    module.insert("Promise".to_string(), agent.intrinsics.promise.clone());

    module
}
