use crate::agent::Agent;
use crate::intrinsics::promise::{new_promise_capability, promise_resolve_i};
use crate::value::{ObjectKey, Value};
use crate::vm::ExecutionContext;
use num::ToPrimitive;

fn next(agent: &Agent, ctx: &ExecutionContext, _: Vec<Value>) -> Result<Value, Value> {
    let this = ctx.environment.borrow().get_this()?;
    if !this.has_slot("net client queue") {
        return Err(Value::new_error("invalid receiver"));
    }

    if let Value::List(buffer) = this.get_slot("net client buffer") {
        let promise = buffer.borrow_mut().pop_front();
        if promise.is_some() {
            return Ok(promise.unwrap());
        }
    }

    if let Value::List(queue) = this.get_slot("net client queue") {
        let promise = new_promise_capability(agent, agent.intrinsics.promise.clone())?;
        queue.borrow_mut().push_back(promise.clone());
        Ok(promise)
    } else {
        unreachable!();
    }
}

pub fn get_or_create_resolve(agent: &Agent, target: Value, value: Value, done: bool) {
    if let Value::List(queue) = target.get_slot("net client queue") {
        let value = Value::new_iter_result(agent, value, done).unwrap();
        if let Some(promise) = queue.borrow_mut().pop_front() {
            promise
                .get_slot("resolve")
                .call(agent, Value::Null, vec![value])
                .unwrap();
        } else if let Value::List(buffer) = target.get_slot("net client buffer") {
            buffer.borrow_mut().push_back(
                promise_resolve_i(agent, agent.intrinsics.promise_prototype.clone(), value)
                    .unwrap(),
            );
        } else {
            unreachable!();
        }
    } else {
        unreachable!();
    }
}

pub fn get_or_create_reject(agent: &Agent, target: Value, value: Value) {
    if let Value::List(queue) = target.get_slot("net client queue") {
        if let Some(promise) = queue.borrow_mut().pop_front() {
            promise
                .get_slot("reject")
                .call(agent, Value::Null, vec![value])
                .unwrap();
        } else if let Value::List(buffer) = target.get_slot("net client buffer") {
            let p = new_promise_capability(agent, agent.intrinsics.promise.clone()).unwrap();
            p.get_slot("reject")
                .call(agent, Value::Null, vec![value])
                .unwrap();
            buffer.borrow_mut().push_back(p);
        } else {
            unreachable!();
        }
    } else {
        unreachable!();
    }
}

fn close(agent: &Agent, ctx: &ExecutionContext, _: Vec<Value>) -> Result<Value, Value> {
    let this = ctx.environment.borrow().get_this()?;
    if !this.has_slot("net client token") {
        return Err(Value::new_error("invalid receiver"));
    }

    if let Value::Number(t) = this.get_slot("net client token") {
        let token = mio::Token(t.to_usize().unwrap());
        agent.mio_map.borrow_mut().remove(&token);
        Ok(Value::Null)
    } else {
        unreachable!();
    }
}

pub fn create_net_client_prototype(agent: &Agent) -> Value {
    let proto = Value::new_object(agent.intrinsics.async_iterator_prototype.clone());

    proto
        .set(
            &ObjectKey::from("next"),
            Value::new_builtin_function(agent, next),
        )
        .unwrap();

    proto
        .set(
            &ObjectKey::from("close"),
            Value::new_builtin_function(agent, close),
        )
        .unwrap();

    proto
}
