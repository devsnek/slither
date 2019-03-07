use crate::agent::Agent;
use crate::value::{new_builtin_function, new_object, ObjectKey, Value};
use crate::vm::ExecutionContext;

fn iter_result(agent: &Agent, value: Value, done: bool) -> Result<Value, Value> {
    let o = new_object(agent.intrinsics.object_prototype.clone());
    o.set(&ObjectKey::from("value"), value)?;
    o.set(
        &ObjectKey::from("done"),
        if done { Value::True } else { Value::False },
    )?;
    Ok(o)
}

fn next(agent: &Agent, ctx: &ExecutionContext, args: Vec<Value>) -> Result<Value, Value> {
    let this = ctx.environment.borrow().get_this()?;
    if let Value::WrappedContext(context, _) = this.get_slot("generator context") {
        let mut args = args;
        if context.borrow_mut().evaluator.is_none() {
            iter_result(agent, Value::Null, true)
        } else {
            let mut evaluator = context.borrow_mut().evaluator.take().unwrap();
            evaluator.stack.push(args.pop().unwrap_or(Value::Null));
            match evaluator.run(agent) {
                Ok(r) => match r {
                    Ok(v) => iter_result(agent, v, true),
                    Err(e) => Err(e),
                },
                Err(c) => {
                    context.borrow_mut().evaluator = Some(evaluator);
                    let mut c = c;
                    let value = std::mem::replace(&mut c.0, Value::Null);
                    iter_result(agent, value, false)
                }
            }
        }
    } else {
        unreachable!();
    }
}

fn throw(agent: &Agent, ctx: &ExecutionContext, args: Vec<Value>) -> Result<Value, Value> {
    let this = ctx.environment.borrow().get_this()?;
    if let Value::WrappedContext(context, _) = this.get_slot("generator context") {
        let mut args = args;
        if context.borrow_mut().evaluator.is_none() {
            iter_result(agent, Value::Null, true)
        } else {
            let mut evaluator = context.borrow_mut().evaluator.take().unwrap();
            evaluator.exception = Some(args.pop().unwrap_or(Value::Null));
            match evaluator.run(agent) {
                Ok(r) => match r {
                    Ok(v) => iter_result(agent, v, true),
                    Err(e) => Err(e),
                },
                Err(c) => {
                    context.borrow_mut().evaluator = Some(evaluator);
                    let mut c = c;
                    let value = std::mem::replace(&mut c.0, Value::Null);
                    iter_result(agent, value, false)
                }
            }
        }
    } else {
        unreachable!();
    }
}

pub fn create_generator_prototype(agent: &Agent) -> Value {
    let proto = new_object(agent.intrinsics.iterator_prototype.clone());

    proto
        .set(&ObjectKey::from("next"), new_builtin_function(agent, next))
        .unwrap();

    proto
        .set(
            &ObjectKey::from("throw"),
            new_builtin_function(agent, throw),
        )
        .unwrap();

    proto
}
