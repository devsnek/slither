use crate::agent::Agent;
use crate::interpreter::Context;
use crate::value::{ObjectKey, Value};

fn iterator(agent: &Agent, _: Vec<Value>, ctx: &Context) -> Result<Value, Value> {
    ctx.scope.borrow().get_this(agent)
}

fn map(agent: &Agent, mut args: Vec<Value>, ctx: &Context) -> Result<Value, Value> {
    let iterated = ctx.scope.borrow().get_this(agent)?.to_iterator(agent)?;
    let mapper = args.pop().unwrap_or(Value::Null);
    if mapper.type_of() != "function" {
        return Err(Value::new_error(agent, "mapper is not a function"));
    }
    let iterator = Value::new_custom_object(agent.intrinsics.iterator_map_prototype.clone());
    iterator.set_slot("mapper", mapper);
    iterator.set_slot("iterated", iterated);
    Ok(iterator)
}

pub fn create_iterator_prototype(agent: &Agent) -> Value {
    let proto = Value::new_object(agent.intrinsics.object_prototype.clone());

    proto
        .set(
            agent,
            Value::new_well_known_symbol("iterator")
                .to_object_key(agent)
                .unwrap(),
            Value::new_builtin_function(agent, iterator),
        )
        .unwrap();

    proto
        .set(
            agent,
            ObjectKey::from("map"),
            Value::new_builtin_function(agent, map),
        )
        .unwrap();

    proto
}
