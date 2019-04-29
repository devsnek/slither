use crate::agent::MioMapType;
use crate::interpreter::Context;
use crate::intrinsics::promise::new_promise_capability;
use crate::value::{ObjectKey, ObjectKind};
use crate::IntoValue;
use crate::{Agent, Value};
use num::ToPrimitive;
use std::io::prelude::*;

fn next(agent: &Agent, _: Vec<Value>, ctx: &Context) -> Result<Value, Value> {
    let this = ctx.scope.borrow().get_this(agent)?;
    if !this.has_slot("net client queue") {
        return Err(Value::new_error(agent, "invalid receiver"));
    }

    if let Value::List(buffer) = this.get_slot("net client buffer") {
        if let Some(promise) = buffer.borrow_mut().pop_front() {
            return Ok(promise);
        }
    }

    if let Value::List(queue) = this.get_slot("net client queue") {
        let promise = new_promise_capability(agent, agent.intrinsics.promise.clone())?;
        queue.borrow_mut().push_back(promise.clone());
        println!("queue.len() = {:?}", queue.borrow().len());
        Ok(promise)
    } else {
        unreachable!();
    }
}

fn write(agent: &Agent, args: Vec<Value>, ctx: &Context) -> Result<Value, Value> {
    let this = ctx.scope.borrow().get_this(agent)?;
    if !this.has_slot("net client token") {
        return Err(Value::new_error(agent, "invalid receiver"));
    }
    if let Value::Number(t) = this.get_slot("net client token") {
        let token = mio::Token(t.to_usize().unwrap());
        let map = agent.mio_map.borrow_mut();
        if let MioMapType::Net(crate::builtins::net::Net::Client(s, ..)) =
            map.get(&token).expect("socket missing in mio_map")
        {
            let mut s = s;
            match args.get(0) {
                Some(Value::String(str)) => {
                    match s.write_all(str.as_bytes()) {
                        Ok(_) => Ok(Value::Null),
                        Err(e) => Err(e.into_value(agent)),
                    }
                    // s.write_all(str.as_bytes())?;
                    // Ok(Value::Null)
                }
                Some(Value::Object(o)) => {
                    if let ObjectKind::Buffer(b) = &o.kind {
                        match s.write_all(&b.borrow()) {
                            Ok(_) => Ok(Value::Null),
                            Err(e) => Err(e.into_value(agent)),
                        }
                    // s.write_all(&b.borrow())?;
                    // Ok(Value::Null)
                    } else {
                        Err(Value::new_error(agent, "data must be a string or buffer"))
                    }
                }
                _ => Err(Value::new_error(agent, "data must be a string or buffer")),
            }
        } else {
            unreachable!();
        }
    } else {
        unreachable!();
    }
}

fn close(agent: &Agent, _: Vec<Value>, ctx: &Context) -> Result<Value, Value> {
    let this = ctx.scope.borrow().get_this(agent)?;
    if !this.has_slot("net client token") {
        return Err(Value::new_error(agent, "invalid receiver"));
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
            agent,
            ObjectKey::from("next"),
            Value::new_builtin_function(agent, next),
        )
        .unwrap();

    proto
        .set(
            agent,
            ObjectKey::from("write"),
            Value::new_builtin_function(agent, write),
        )
        .unwrap();

    proto
        .set(
            agent,
            ObjectKey::from("close"),
            Value::new_builtin_function(agent, close),
        )
        .unwrap();

    proto
}
