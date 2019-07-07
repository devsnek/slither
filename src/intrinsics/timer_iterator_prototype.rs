use crate::intrinsics::promise::new_promise_capability;
use crate::value::{Args, ObjectKey};
use crate::{Agent, Value};
use num::ToPrimitive;

fn next(args: Args) -> Result<Value, Value> {
    let this = args.this();
    if !this.has_slot("timer queue") {
        return Err(Value::new_error(args.agent(), "invalid receiver"));
    }

    if let Value::List(buffer) = this.get_slot("timer buffer") {
        if let Some(promise) = buffer.borrow_mut().pop_front() {
            return Ok(promise);
        }
    }

    if let Value::List(queue) = this.get_slot("timer queue") {
        let promise =
            new_promise_capability(args.agent(), args.agent().intrinsics.promise.clone())?;
        queue.borrow_mut().push_back(promise.clone());
        Ok(promise)
    } else {
        unreachable!();
    }
}

fn stop(args: Args) -> Result<Value, Value> {
    let this = args.this();
    if !this.has_slot("timer token") {
        return Err(Value::new_error(args.agent(), "invalid receiver"));
    }

    if let Value::Number(t) = this.get_slot("timer token") {
        let token = mio::Token(t.to_usize().unwrap());
        args.agent().mio_map.borrow_mut().remove(&token);
        Ok(Value::Null)
    } else {
        unreachable!();
    }
}

pub(crate) fn create_timer_iterator_prototype(agent: &Agent) -> Value {
    let proto = Value::new_object(agent.intrinsics.async_iterator_prototype.clone());

    proto
        .set(
            agent,
            ObjectKey::from("next"),
            Value::new_builtin_function(agent, next, false),
        )
        .unwrap();

    proto
        .set(
            agent,
            ObjectKey::from("stop"),
            Value::new_builtin_function(agent, stop, false),
        )
        .unwrap();

    proto
        .set(
            agent,
            ObjectKey::from("return"),
            proto.get(agent, ObjectKey::from("stop")).unwrap(),
        )
        .unwrap();

    proto
}
