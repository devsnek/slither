use crate::agent::Agent;
use crate::interpreter::Context;
use crate::value::{ObjectKey, Value};

fn next(agent: &Agent, _args: Vec<Value>, ctx: &Context) -> Result<Value, Value> {
    let o = ctx.scope.borrow().get_this(agent)?;
    if o.type_of() != "object" {
        return Err(Value::new_error(agent, "invalid receiver"));
    }
    let mapper = o.get_slot("mapper");
    let iterated = o.get_slot("iterated");
    let result = if let Value::Iterator(iterator, next) = iterated {
        next.call(agent, *iterator, vec![])?
    } else {
        unreachable!();
    };
    if result.get(agent, ObjectKey::from("done"))? == Value::from(true) {
        return Value::new_iter_result(agent, Value::Null, true);
    }
    let value = result.get(agent, ObjectKey::from("value"))?;
    let mapped = mapper.call(agent, Value::Null, vec![value])?;
    Value::new_iter_result(agent, mapped, false)
}

pub fn create_iterator_map_prototype(agent: &Agent) -> Value {
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
