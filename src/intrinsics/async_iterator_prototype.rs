use crate::agent::Agent;
use crate::value::Value;
use crate::vm::ExecutionContext;

fn iterator(_: &Agent, ctx: &ExecutionContext, _: Vec<Value>) -> Result<Value, Value> {
    ctx.environment.borrow().get_this()
}

pub fn create_async_iterator_prototype(agent: &Agent) -> Value {
    let proto = Value::new_object(agent.intrinsics.object_prototype.clone());

    proto
        .set(
            &agent
                .well_known_symbol("asyncIterator")
                .to_object_key()
                .unwrap(),
            Value::new_builtin_function(agent, iterator),
        )
        .unwrap();

    proto
}
