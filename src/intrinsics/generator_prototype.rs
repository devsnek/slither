use crate::agent::Agent;
use crate::interpreter::Context;
use crate::value::{ObjectKey, Value};

fn next(agent: &Agent, args: Vec<Value>, ctx: &Context) -> Result<Value, Value> {
    let this = ctx.get_this(agent)?;
    if let Value::WrappedContext(context, _) = this.get_slot("generator context") {
        let mut args = args;
        if context.borrow_mut().interpreter.is_none() {
            Value::new_iter_result(agent, Value::Null, true)
        } else {
            let mut interpreter = context.borrow_mut().interpreter.take().unwrap();
            interpreter.accumulator = args.pop().unwrap_or(Value::Null);
            match interpreter.run(agent) {
                Ok(r) => match r {
                    Ok(v) => Value::new_iter_result(agent, v, true),
                    Err(e) => Err(e),
                },
                Err(c) => {
                    context.borrow_mut().interpreter = Some(interpreter);
                    let mut c = c;
                    let value = std::mem::replace(&mut c.0, Value::Null);
                    Value::new_iter_result(agent, value, false)
                }
            }
        }
    } else {
        unreachable!();
    }
}

fn throw(agent: &Agent, args: Vec<Value>, ctx: &Context) -> Result<Value, Value> {
    let this = ctx.get_this(agent)?;
    if let Value::WrappedContext(context, _) = this.get_slot("generator context") {
        let mut args = args;
        if context.borrow_mut().interpreter.is_none() {
            Value::new_iter_result(agent, Value::Null, true)
        } else {
            let mut interpreter = context.borrow_mut().interpreter.take().unwrap();
            interpreter.exception = Some(args.pop().unwrap_or(Value::Null));
            match interpreter.run(agent) {
                Ok(r) => match r {
                    Ok(v) => Value::new_iter_result(agent, v, true),
                    Err(e) => Err(e),
                },
                Err(c) => {
                    context.borrow_mut().interpreter = Some(interpreter);
                    let mut c = c;
                    let value = std::mem::replace(&mut c.0, Value::Null);
                    Value::new_iter_result(agent, value, false)
                }
            }
        }
    } else {
        unreachable!();
    }
}

pub fn create_generator_prototype(agent: &Agent) -> Value {
    let proto = Value::new_object(agent.intrinsics.iterator_prototype.clone());

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
            ObjectKey::from("throw"),
            Value::new_builtin_function(agent, throw),
        )
        .unwrap();

    proto
}
