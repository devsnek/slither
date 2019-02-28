use crate::agent::Agent;
use crate::intrinsics::promise::{new_promise_capability, promise_reaction_job, promise_resolve_i};
use crate::value::{
    new_builtin_function, new_custom_object, new_error, new_object, ObjectKey, Value,
};
use crate::vm::ExecutionContext;

fn promise_proto_then(
    agent: &Agent,
    ctx: &ExecutionContext,
    args: Vec<Value>,
) -> Result<Value, Value> {
    let mut on_fulfilled = args.get(0).unwrap_or(&Value::Null).clone();
    let mut on_rejected = args.get(1).unwrap_or(&Value::Null).clone();

    let this = ctx.environment.borrow().this.clone().unwrap();

    let constructor = this.get(&ObjectKey::from("constructor"))?;

    let promise = new_promise_capability(agent, constructor)?;

    if on_fulfilled.type_of() != "function" {
        on_fulfilled = Value::Null;
    }
    if on_rejected.type_of() != "function" {
        on_rejected = Value::Null;
    }

    let fulfill_reaction = new_custom_object(Value::Null);
    fulfill_reaction.set_slot("kind", Value::from("resolve"));
    fulfill_reaction.set_slot("promise", promise.clone());
    fulfill_reaction.set_slot("handler", on_fulfilled);

    let reject_reaction = new_custom_object(Value::Null);
    reject_reaction.set_slot("kind", Value::from("reject"));
    reject_reaction.set_slot("promise", promise.clone());
    reject_reaction.set_slot("handler", on_rejected);

    let state = this.get_slot("promise state");
    if let Value::String(s) = &state {
        match s.as_str() {
            "pending" => {
                if let Value::List(reactions) = &this.get_slot("fulfill reactions") {
                    reactions.borrow_mut().push_back(fulfill_reaction);
                } else {
                    unreachable!();
                }
                if let Value::List(reactions) = &this.get_slot("reject reactions") {
                    reactions.borrow_mut().push_back(reject_reaction);
                } else {
                    unreachable!();
                }
            }
            "fulfilled" => {
                let value = this.get_slot("result");
                agent.enqueue_job(promise_reaction_job, vec![fulfill_reaction, value]);
            }
            "rejected" => {
                let reason = this.get_slot("result");
                agent.enqueue_job(promise_reaction_job, vec![reject_reaction, reason]);
            }
            _ => unreachable!(),
        }
    } else {
        unreachable!();
    }

    Ok(promise)
}

fn promise_proto_catch(
    agent: &Agent,
    ctx: &ExecutionContext,
    args: Vec<Value>,
) -> Result<Value, Value> {
    let on_rejected = args.get(0).unwrap_or(&Value::Null).clone();
    let this = ctx.environment.borrow().this.clone().unwrap();
    let then = this.get(&ObjectKey::from("then"))?;
    then.call(agent, this.clone(), vec![Value::Null, on_rejected])
}

fn value_thunk(_a: &Agent, ctx: &ExecutionContext, _args: Vec<Value>) -> Result<Value, Value> {
    let f = ctx.function.clone().unwrap();
    Ok(f.get_slot("value"))
}

fn value_thrower(_a: &Agent, ctx: &ExecutionContext, _args: Vec<Value>) -> Result<Value, Value> {
    let f = ctx.function.clone().unwrap();
    Err(f.get_slot("value"))
}

fn then_finally_function(
    agent: &Agent,
    ctx: &ExecutionContext,
    args: Vec<Value>,
) -> Result<Value, Value> {
    let f = ctx.function.clone().unwrap();
    let on_finally = f.get_slot("on finally");
    let result = on_finally.call(agent, Value::Null, vec![])?;
    let c = f.get_slot("constructor");
    let promise = promise_resolve_i(agent, c, result)?;
    let value = args.get(0).unwrap_or(&Value::Null).clone();
    let value_thunk = new_builtin_function(agent, value_thunk);
    value_thunk.set_slot("value", value);
    promise
        .get(&ObjectKey::from("then"))?
        .call(agent, promise, vec![value_thunk])
}

fn catch_finally_function(
    agent: &Agent,
    ctx: &ExecutionContext,
    args: Vec<Value>,
) -> Result<Value, Value> {
    let f = ctx.function.clone().unwrap();
    let on_finally = f.get_slot("on finally");
    let result = on_finally.call(agent, Value::Null, vec![])?;
    let c = f.get_slot("constructor");
    let promise = promise_resolve_i(agent, c, result)?;
    let value = args.get(0).unwrap_or(&Value::Null).clone();
    let thrower = new_builtin_function(agent, value_thrower);
    thrower.set_slot("value", value);
    promise
        .get(&ObjectKey::from("then"))?
        .call(agent, promise, vec![thrower])
}

fn promise_proto_finally(
    agent: &Agent,
    ctx: &ExecutionContext,
    args: Vec<Value>,
) -> Result<Value, Value> {
    let promise = ctx.environment.borrow().this.clone().unwrap();
    if promise.type_of() != "object" && promise.type_of() != "function" {
        return Err(new_error("invalid this"));
    }

    let c = promise.get(&ObjectKey::from("constructor"))?;
    if c.type_of() != "object" && c.type_of() != "function" {
        return Err(new_error("this does not derive a valid constructor"));
    }

    let on_finally = args.get(0).unwrap_or(&Value::Null).clone();

    let (then_finally, catch_finally) = if on_finally.type_of() == "function" {
        let then_finally = new_builtin_function(agent, then_finally_function);
        then_finally.set_slot("constructor", c.clone());
        then_finally.set_slot("on finally", on_finally.clone());
        let catch_finally = new_builtin_function(agent, catch_finally_function);
        catch_finally.set_slot("constructor", c);
        catch_finally.set_slot("on finally", on_finally);
        (then_finally, catch_finally)
    } else {
        (on_finally.clone(), on_finally)
    };

    promise
        .get(&ObjectKey::from("then"))?
        .call(agent, promise, vec![then_finally, catch_finally])
}

pub fn create_promise_prototype(agent: &Agent) -> Value {
    let p = new_object(agent.intrinsics.object_prototype.clone());

    p.set(
        &ObjectKey::from("then"),
        new_builtin_function(agent, promise_proto_then),
    )
    .expect("unable to set then on promise prototype");
    p.set(
        &ObjectKey::from("catch"),
        new_builtin_function(agent, promise_proto_catch),
    )
    .expect("unable to set catch on promise prototype");
    p.set(
        &ObjectKey::from("finally"),
        new_builtin_function(agent, promise_proto_finally),
    )
    .expect("unable to set finally on promise prototype");

    p
}
