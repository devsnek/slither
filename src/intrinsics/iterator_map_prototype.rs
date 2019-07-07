use crate::agent::Agent;
use crate::value::{Args, ObjectKey, Value, ValueType};

fn next(args: Args) -> Result<Value, Value> {
    let o = args.this();
    if o.type_of() != ValueType::Object {
        return Err(Value::new_error(args.agent(), "invalid receiver"));
    }
    let mapper = o.get_slot("mapper");
    let iterated = o.get_slot("iterated");
    let result = if let Value::Iterator(iterator, next) = iterated {
        next.call(args.agent(), *iterator, vec![])?
    } else {
        unreachable!();
    };
    if result.get(args.agent(), ObjectKey::from("done"))? == Value::from(true) {
        return Value::new_iter_result(args.agent(), Value::Null, true);
    }
    let value = result.get(args.agent(), ObjectKey::from("value"))?;
    let mapped = mapper.call(args.agent(), Value::Null, vec![value])?;
    Value::new_iter_result(args.agent(), mapped, false)
}

pub(crate) fn create_iterator_map_prototype(agent: &Agent) -> Value {
    let proto = Value::new_object(agent.intrinsics.iterator_prototype.clone());

    proto
        .set(
            agent,
            ObjectKey::from("next"),
            Value::new_builtin_function(agent, next, false),
        )
        .unwrap();

    proto
}
