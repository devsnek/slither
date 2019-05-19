use crate::agent::MioMapType;
use crate::intrinsics::promise::new_promise_capability;
use crate::value::{Args, ObjectKey, ObjectKind};
use crate::IntoValue;
use crate::{Agent, Value};
use num::ToPrimitive;
use std::io::prelude::*;

fn next(args: Args) -> Result<Value, Value> {
    let this = args.this();
    if !this.has_slot("net client queue") {
        return Err(Value::new_error(args.agent(), "invalid receiver"));
    }

    if let Value::List(buffer) = this.get_slot("net client buffer") {
        if let Some(promise) = buffer.borrow_mut().pop_front() {
            return Ok(promise);
        }
    }

    if let Value::List(queue) = this.get_slot("net client queue") {
        let promise =
            new_promise_capability(args.agent(), args.agent().intrinsics.promise.clone())?;
        queue.borrow_mut().push_back(promise.clone());
        Ok(promise)
    } else {
        unreachable!();
    }
}

fn write(args: Args) -> Result<Value, Value> {
    let this = args.this();
    if !this.has_slot("net client token") {
        return Err(Value::new_error(args.agent(), "invalid receiver"));
    }
    if let Value::Number(t) = this.get_slot("net client token") {
        let token = mio::Token(t.to_usize().unwrap());
        let map = args.agent().mio_map.borrow_mut();
        if let MioMapType::Net(crate::builtins::net::Net::Client(s, ..)) =
            map.get(&token).expect("socket missing in mio_map")
        {
            let mut s = s;
            match &args[0] {
                Value::String(str) => match s.write_all(str.as_bytes()) {
                    Ok(_) => Ok(Value::Null),
                    Err(e) => Err(e.into_value(args.agent())),
                },
                Value::Object(o) => {
                    if let ObjectKind::Buffer(b) = &o.kind {
                        match s.write_all(&b.borrow()) {
                            Ok(_) => Ok(Value::Null),
                            Err(e) => Err(e.into_value(args.agent())),
                        }
                    } else {
                        Err(Value::new_error(
                            args.agent(),
                            "data must be a string or buffer",
                        ))
                    }
                }
                _ => Err(Value::new_error(
                    args.agent(),
                    "data must be a string or buffer",
                )),
            }
        } else {
            unreachable!();
        }
    } else {
        unreachable!();
    }
}

fn close(args: Args) -> Result<Value, Value> {
    let this = args.this();
    if !this.has_slot("net client token") {
        return Err(Value::new_error(args.agent(), "invalid receiver"));
    }

    if let Value::Number(t) = this.get_slot("net client token") {
        let token = mio::Token(t.to_usize().unwrap());
        args.agent().mio_map.borrow_mut().remove(&token);
        Ok(Value::Null)
    } else {
        unreachable!();
    }
}

pub(crate) fn create_net_client_prototype(agent: &Agent) -> Value {
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
