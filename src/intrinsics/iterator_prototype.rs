use crate::agent::Agent;
use crate::value::{new_builtin_function, new_object, Value};
use crate::vm::ExecutionContext;

fn iterator(_: &Agent, ctx: &ExecutionContext, _: Vec<Value>) -> Result<Value, Value> {
    ctx.environment.borrow().get_this()
}

pub fn create_iterator_prototype(agent: &Agent) -> Value {
    let proto = new_object(agent.intrinsics.object_prototype.clone());

    proto
        .set(
            &agent.well_known_symbol("iterator").to_object_key().unwrap(),
            new_builtin_function(agent, iterator),
        )
        .unwrap();

    proto
}
