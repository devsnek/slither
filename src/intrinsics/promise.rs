use crate::module::{call, get, set, Agent, ExecutionContext};
use crate::value::{
    new_builtin_function, new_custom_object, new_error, new_object, ObjectKey, Value,
};

fn trigger_promise_reactions(
    agent: &Agent,
    reactions: Value,
    argument: Value,
) -> Result<Value, Value> {
    if let Value::List(list) = &reactions {
        let list = list.borrow();
        for reaction in list.iter() {
            agent.enqueue_job(promise_reaction_job, vec![reaction.clone(), argument.clone()]);
        }
    } else {
        unreachable!();
    }
    Ok(Value::Null)
}

pub fn promise_reaction_job(agent: &Agent, args: Vec<Value>) {
    let reaction = args[0].clone();
    let argument = args[1].clone();

    let promise = reaction.get_slot("promise").unwrap();
    let kind = reaction.get_slot("kind").unwrap();
    let handler = reaction.get_slot("handler").unwrap();

    let mut handler_result;
    if handler == Value::Null {
        if kind == Value::from("fulfill") {
            handler_result = Ok(argument);
        } else {
            handler_result = Err(argument);
        }
    } else {
        handler_result = call(agent, handler, Value::Null, vec![argument]);
    }

    if promise != Value::Null {
        match handler_result {
            Ok(v) => call(
                agent,
                promise.get_slot("resolve").unwrap(),
                Value::Null,
                vec![v],
            ),
            Err(v) => call(
                agent,
                promise.get_slot("reject").unwrap(),
                Value::Null,
                vec![v],
            ),
        };
    }
}

fn reject_promise(agent: &Agent, promise: Value, reason: Value) -> Result<Value, Value> {
    let reactions = promise.get_slot("reject reactions")?;
    promise.set_slot("result", reason.clone())?;
    promise.set_slot("state", Value::from("rejected"))?;
    promise.set_slot("fulfill reactions", Value::Null)?;
    promise.set_slot("reject reactions", Value::Null)?;
    trigger_promise_reactions(agent, reactions, reason)
}

fn fulfill_promise(agent: &Agent, promise: Value, value: Value) -> Result<Value, Value> {
    let reactions = promise.get_slot("fulfill reactions")?;
    promise.set_slot("result", value.clone())?;
    promise.set_slot("state", Value::from("fulfilled"))?;
    promise.set_slot("fulfill reactions", Value::Null)?;
    promise.set_slot("reject reactions", Value::Null)?;
    trigger_promise_reactions(agent, reactions, value)
}

fn promise_resolve_function(
    agent: &Agent,
    ctx: &mut ExecutionContext,
    args: Vec<Value>,
) -> Result<Value, Value> {
    let f = ctx.function.clone().unwrap();

    let already_resolved = f.get_slot("already resolved")?;
    if get(&already_resolved, &ObjectKey::from("resolved"))? == Value::True {
        return Ok(Value::Null);
    } else {
        set(&already_resolved, &ObjectKey::from("resolved"), Value::True)?;
    }

    let promise = f.get_slot("promise")?;
    let resolution = args.get(0).unwrap_or(&Value::Null).clone();
    if promise == resolution {
        reject_promise(
            agent,
            promise,
            new_error("cannot resolve a promise with itself"),
        )
    } else {
        fulfill_promise(agent, promise, resolution)
    }
}

fn promise_reject_function(
    agent: &Agent,
    ctx: &mut ExecutionContext,
    args: Vec<Value>,
) -> Result<Value, Value> {
    let f = ctx.function.clone().unwrap();

    let already_resolved = f.get_slot("already resolved")?;
    if get(&already_resolved, &ObjectKey::from("resolved"))? == Value::True {
        return Ok(Value::Null);
    } else {
        set(&already_resolved, &ObjectKey::from("resolved"), Value::True)?;
    }

    let promise = f.get_slot("promise")?;
    let resolution = args.get(0).unwrap_or(&Value::Null).clone();
    reject_promise(agent, promise, resolution)
}

fn promise(agent: &Agent, _ctx: &mut ExecutionContext, args: Vec<Value>) -> Result<Value, Value> {
    let executor = args[0].clone();

    if executor.type_of() != "function" {
        return Err(new_error("executor must be a function"));
    }

    let promise = new_custom_object(agent.intrinsics.promise_prototype.clone());
    promise.set_slot("state", Value::from("pending"))?;
    promise.set_slot("fulfill reactions", Value::new_list())?;
    promise.set_slot("reject reactions", Value::new_list())?;

    let already_resolved = new_object(Value::Null);

    let resolve = new_builtin_function(agent, promise_resolve_function);
    resolve.set_slot("promise", promise.clone())?;
    resolve.set_slot("already resolved", already_resolved.clone())?;

    let reject = new_builtin_function(agent, promise_reject_function);
    reject.set_slot("promise", promise.clone())?;
    reject.set_slot("already resolved", already_resolved)?;

    let result = call(agent, executor, Value::Null, vec![resolve, reject.clone()]);

    if let Err(e) = result {
        call(agent, reject, Value::Null, vec![e])?;
    }

    Ok(promise)
}

pub fn create_promise(agent: &Agent, prototype: Value) -> Value {
    let p = new_builtin_function(agent, promise);

    set(&p, &ObjectKey::from("prototype"), prototype.clone()).unwrap();
    set(&prototype, &ObjectKey::from("constructor"), p.clone()).unwrap();

    p
}
