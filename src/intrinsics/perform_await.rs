use crate::intrinsics::promise::promise_resolve_i;
use crate::value::{Args, ObjectKey};
use crate::{Agent, Value};

pub(crate) fn on_fulfilled(args: Args) -> Result<Value, Value> {
    let f = args.function();
    if let Value::WrappedContext(context, promise) = f.get_slot("async context") {
        let mut interpreter = context.borrow_mut().interpreter.take().unwrap();
        interpreter.accumulator = args[0].clone();
        match interpreter.run(args.agent()) {
            Ok(r) => match r {
                Ok(v) => {
                    promise.unwrap().get_slot("resolve").call(
                        args.agent(),
                        Value::Null,
                        vec![v],
                    )?;
                }
                Err(e) => {
                    promise
                        .unwrap()
                        .get_slot("reject")
                        .call(args.agent(), Value::Null, vec![e])?;
                }
            },
            Err(c) => {
                context.borrow_mut().interpreter = Some(interpreter);
                let mut c = c;
                let value = std::mem::replace(&mut c.0, Value::Null);
                perform_await(args.agent(), Value::WrappedContext(context, promise), value)?;
            }
        }
        Ok(Value::Null)
    } else {
        unreachable!();
    }
}

pub(crate) fn on_rejected(args: Args) -> Result<Value, Value> {
    let f = args.function();
    if let Value::WrappedContext(context, promise) = f.get_slot("async context") {
        let mut interpreter = context.borrow_mut().interpreter.take().unwrap();
        interpreter.exception = Some(args[0].clone());
        match interpreter.run(args.agent()) {
            Ok(r) => match r {
                Ok(v) => {
                    promise.unwrap().get_slot("resolve").call(
                        args.agent(),
                        Value::Null,
                        vec![v],
                    )?;
                }
                Err(e) => {
                    promise
                        .unwrap()
                        .get_slot("reject")
                        .call(args.agent(), Value::Null, vec![e])?;
                }
            },
            Err(c) => {
                context.borrow_mut().interpreter = Some(interpreter);
                let mut c = c;
                let value = std::mem::replace(&mut c.0, Value::Null);
                perform_await(args.agent(), Value::WrappedContext(context, promise), value)?;
            }
        }
        Ok(Value::Null)
    } else {
        unreachable!();
    }
}

pub(crate) fn perform_await(agent: &Agent, ctx: Value, value: Value) -> Result<(), Value> {
    let promise = promise_resolve_i(agent, agent.intrinsics.promise.clone(), value)?;

    let on_fulfilled = Value::new_builtin_function(agent, on_fulfilled, false);
    on_fulfilled.set_slot("async context", ctx.clone());
    let on_rejected = Value::new_builtin_function(agent, on_rejected, false);
    on_rejected.set_slot("async context", ctx);

    promise.get(agent, ObjectKey::from("then"))?.call(
        agent,
        promise,
        vec![on_fulfilled, on_rejected],
    )?;

    Ok(())
}
