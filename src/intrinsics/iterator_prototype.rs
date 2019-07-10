use crate::agent::Agent;
use crate::value::{Args, ObjectKey, Value, ValueType};

fn iterator(args: Args) -> Result<Value, Value> {
    Ok(args.this())
}

fn map(args: Args) -> Result<Value, Value> {
    let iterated = args.this().to_iterator(args.agent())?;
    if args[0].type_of() != ValueType::Function {
        return Err(Value::new_error(args.agent(), "mapper is not a function"));
    }
    let iterator = Value::new_custom_object(args.agent().intrinsics.iterator_map_prototype.clone());
    iterator.set_slot("mapper", args[0].clone());
    iterator.set_slot("iterated", iterated);
    Ok(iterator)
}

fn foreach(args: Args) -> Result<Value, Value> {
    let iterated = args.this().to_iterator(args.agent())?;
    if args[0].type_of() != ValueType::Function {
        return Err(Value::new_error(args.agent(), "cb is not a function"));
    }
    if let Value::Iterator(iterator, next) = iterated {
        loop {
            let result = next.call(args.agent(), (*iterator).clone(), vec![])?;
            let done = result.get(args.agent(), ObjectKey::from("done"))?;
            if done == Value::from(true) {
                return Ok(Value::Null);
            }
            let value = result.get(args.agent(), ObjectKey::from("value"))?;
            args[0].call(args.agent(), Value::Null, vec![value])?;
        }
    } else {
        unreachable!();
    }
}

pub(crate) fn create_iterator_prototype(agent: &Agent) -> Value {
    let proto = Value::new_object(agent.intrinsics.object_prototype.clone());

    proto
        .set(
            agent,
            ObjectKey::well_known_symbol("iterator"),
            Value::new_builtin_function(agent, iterator, false),
        )
        .unwrap();

    proto
        .set(
            agent,
            ObjectKey::from("map"),
            Value::new_builtin_function(agent, map, false),
        )
        .unwrap();
    proto
        .set(
            agent,
            ObjectKey::from("forEach"),
            Value::new_builtin_function(agent, foreach, false),
        )
        .unwrap();

    proto
}
