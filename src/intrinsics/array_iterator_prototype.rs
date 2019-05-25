use crate::agent::Agent;
use crate::value::{Args, ObjectKey, Value, ValueType};

fn next(args: Args) -> Result<Value, Value> {
    let o = args.this();
    if o.type_of() != ValueType::Object {
        return Err(Value::new_error(args.agent(), "invalid receiver"));
    }
    let a = o.get_slot("iterated object");
    if a == Value::Null {
        return Value::new_iter_result(args.agent(), Value::Null, true);
    }
    let index = if let Value::Number(n) = o.get_slot("array iterator next index") {
        n
    } else {
        unreachable!();
    };
    let len = if let Value::Number(n) = a.get(
        args.agent(),
        Value::from("length").to_object_key(args.agent())?,
    )? {
        n
    } else {
        return Err(Value::new_error(args.agent(), "invalid array length"));
    };
    if index >= len {
        o.set_slot("iterated object", Value::Null);
        return Value::new_iter_result(args.agent(), Value::Null, true);
    }
    o.set_slot("array iterator next index", Value::from(index + 1.0));
    let value = a.get(
        args.agent(),
        Value::from(index).to_object_key(args.agent())?,
    )?;
    Value::new_iter_result(args.agent(), value, false)
}

pub(crate) fn create_array_iterator_prototype(agent: &Agent) -> Value {
    let proto = Value::new_object(agent.intrinsics.iterator_prototype.clone());

    proto
        .set(
            agent,
            ObjectKey::from("next"),
            Value::new_builtin_function(agent, next),
        )
        .unwrap();

    proto
}
