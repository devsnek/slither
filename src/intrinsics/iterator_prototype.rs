use crate::agent::Agent;
use crate::interpreter::Context;
use crate::value::Value;

fn iterator(agent: &Agent, _: Vec<Value>, ctx: &Context) -> Result<Value, Value> {
    ctx.scope.borrow().get_this(agent)
}

pub fn create_iterator_prototype(agent: &Agent) -> Value {
    let proto = Value::new_object(agent.intrinsics.object_prototype.clone());

    proto
        .set(
            agent,
            agent
                .well_known_symbol("iterator")
                .to_object_key(agent)
                .unwrap(),
            Value::new_builtin_function(agent, iterator),
        )
        .unwrap();

    proto
}
