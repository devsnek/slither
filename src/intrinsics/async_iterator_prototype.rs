use crate::agent::Agent;
use crate::value::{Args, ObjectKey, Value};

fn iterator(args: Args) -> Result<Value, Value> {
    Ok(args.this())
}

pub fn create_async_iterator_prototype(agent: &Agent) -> Value {
    let proto = Value::new_object(agent.intrinsics.object_prototype.clone());

    proto
        .set(
            agent,
            ObjectKey::well_known_symbol("asyncIterator"),
            Value::new_builtin_function(agent, iterator),
        )
        .unwrap();

    proto
}
