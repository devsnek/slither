use crate::agent::Agent;
use crate::value::{Args, ObjectKey, Value};

fn iterator(args: Args) -> Result<Value, Value> {
    Ok(args.this())
}

fn map(args: Args) -> Result<Value, Value> {
    let iterated = args.this().to_iterator(args.agent())?;
    if args[0].type_of() != "function" {
        return Err(Value::new_error(args.agent(), "mapper is not a function"));
    }
    let iterator = Value::new_custom_object(args.agent().intrinsics.iterator_map_prototype.clone());
    iterator.set_slot("mapper", args[0].clone());
    iterator.set_slot("iterated", iterated);
    Ok(iterator)
}

pub fn create_iterator_prototype(agent: &Agent) -> Value {
    let proto = Value::new_object(agent.intrinsics.object_prototype.clone());

    proto
        .set(
            agent,
            ObjectKey::well_known_symbol("iterator"),
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
