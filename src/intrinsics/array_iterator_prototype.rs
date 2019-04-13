use crate::agent::Agent;
use crate::interpreter::Context;
use crate::value::{ObjectKey, Value};

fn next(agent: &Agent, _args: Vec<Value>, ctx: &Context) -> Result<Value, Value> {
    let o = ctx.scope.borrow().get_this(agent)?;
    if o.type_of() != "object" {
        return Err(Value::new_error(agent, "invalid receiver"));
    }
    let a = o.get_slot("iterated object");
    if a == Value::Null {
        return Value::new_iter_result(agent, Value::Null, true);
    }
    let index = if let Value::Number(n) = o.get_slot("array iterator next index") {
        n
    } else {
        unreachable!();
    };
    let len = if let Value::Number(n) = a.get(agent, Value::from("length").to_object_key(agent)?)? {
        n
    } else {
        return Err(Value::new_error(agent, "invalid array length"));
    };
    if index >= len {
        o.set_slot("iterated object", Value::Null);
        return Value::new_iter_result(agent, Value::Null, true);
    }
    o.set_slot("array iterator next index", Value::from(index + 1.0));
    let value = a.get(agent, Value::from(index).to_object_key(agent)?)?;
    Value::new_iter_result(agent, value, false)
}

pub fn create_array_iterator_prototype(agent: &Agent) -> Value {
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
