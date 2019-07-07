use crate::agent::Agent;
use crate::value::{Args, ObjectKey, Value};

fn next(args: Args) -> Result<Value, Value> {
    let this = args.this();
    if let Value::WrappedContext(context, _) = this.get_slot("generator context") {
        if context.borrow_mut().interpreter.is_none() {
            Value::new_iter_result(args.agent(), Value::Null, true)
        } else {
            let mut interpreter = context.borrow_mut().interpreter.take().unwrap();
            interpreter.accumulator = args[0].clone();
            match interpreter.run(args.agent()) {
                Ok(r) => match r {
                    Ok(v) => Value::new_iter_result(args.agent(), v, true),
                    Err(e) => Err(e),
                },
                Err(c) => {
                    context.borrow_mut().interpreter = Some(interpreter);
                    let mut c = c;
                    let value = std::mem::replace(&mut c.0, Value::Null);
                    Value::new_iter_result(args.agent(), value, false)
                }
            }
        }
    } else {
        unreachable!();
    }
}

fn throw(args: Args) -> Result<Value, Value> {
    let this = args.this();
    if let Value::WrappedContext(context, _) = this.get_slot("generator context") {
        if context.borrow_mut().interpreter.is_none() {
            Value::new_iter_result(args.agent(), Value::Null, true)
        } else {
            let mut interpreter = context.borrow_mut().interpreter.take().unwrap();
            interpreter.exception = Some(args[0].clone());
            match interpreter.run(args.agent()) {
                Ok(r) => match r {
                    Ok(v) => Value::new_iter_result(args.agent(), v, true),
                    Err(e) => Err(e),
                },
                Err(c) => {
                    context.borrow_mut().interpreter = Some(interpreter);
                    let mut c = c;
                    let value = std::mem::replace(&mut c.0, Value::Null);
                    Value::new_iter_result(args.agent(), value, false)
                }
            }
        }
    } else {
        unreachable!();
    }
}

pub(crate) fn create_generator_prototype(agent: &Agent) -> Value {
    let proto = Value::new_object(agent.intrinsics.iterator_prototype.clone());

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
            ObjectKey::from("throw"),
            Value::new_builtin_function(agent, throw, false),
        )
        .unwrap();

    proto
}
