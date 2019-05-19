use crate::agent::Agent;
use crate::intrinsics::promise::{new_promise_capability, promise_reaction_job, promise_resolve_i};
use crate::value::{Args, ObjectKey, Value};

fn promise_proto_then(args: Args) -> Result<Value, Value> {
    let constructor = args
        .this()
        .get(args.agent(), ObjectKey::from("constructor"))?;

    let on_fulfilled = if args[0].type_of() == "function" {
        args[0].clone()
    } else {
        Value::Null
    };

    let on_rejected = if args[1].type_of() == "function" {
        args[1].clone()
    } else {
        Value::Null
    };

    let promise = new_promise_capability(args.agent(), constructor)?;

    let fulfill_reaction = Value::new_custom_object(Value::Null);
    fulfill_reaction.set_slot("kind", Value::from("resolve"));
    fulfill_reaction.set_slot("promise", promise.clone());
    fulfill_reaction.set_slot("handler", on_fulfilled);

    let reject_reaction = Value::new_custom_object(Value::Null);
    reject_reaction.set_slot("kind", Value::from("reject"));
    reject_reaction.set_slot("promise", promise.clone());
    reject_reaction.set_slot("handler", on_rejected);

    let state = args.this().get_slot("promise state");
    if let Value::String(s) = &state {
        match s.as_str() {
            "pending" => {
                if let Value::List(reactions) = &args.this().get_slot("fulfill reactions") {
                    reactions.borrow_mut().push_back(fulfill_reaction);
                } else {
                    unreachable!();
                }
                if let Value::List(reactions) = &args.this().get_slot("reject reactions") {
                    reactions.borrow_mut().push_back(reject_reaction);
                    args.this().set_slot("promise handled", Value::from(true));
                } else {
                    unreachable!();
                }
            }
            "fulfilled" => {
                let value = args.this().get_slot("result");
                args.agent()
                    .enqueue_job(promise_reaction_job, vec![fulfill_reaction, value]);
            }
            "rejected" => {
                let reason = args.this().get_slot("result");
                args.agent()
                    .enqueue_job(promise_reaction_job, vec![reject_reaction, reason]);
            }
            _ => unreachable!(),
        }
    } else {
        unreachable!();
    }

    Ok(promise)
}

fn promise_proto_catch(args: Args) -> Result<Value, Value> {
    let then = args.this().get(args.agent(), ObjectKey::from("then"))?;
    then.call(
        args.agent(),
        args.this(),
        vec![Value::Null, args[0].clone()],
    )
}

fn value_thunk(args: Args) -> Result<Value, Value> {
    Ok(args.function().get_slot("value"))
}

fn value_thrower(args: Args) -> Result<Value, Value> {
    Err(args.function().get_slot("value"))
}

fn then_finally_function(args: Args) -> Result<Value, Value> {
    let result = args
        .function()
        .get_slot("on_finally")
        .call(args.agent(), Value::Null, vec![])?;
    let promise = promise_resolve_i(
        args.agent(),
        args.function().get_slot("constructor"),
        result,
    )?;
    let value_thunk = Value::new_builtin_function(args.agent(), value_thunk);
    value_thunk.set_slot("value", args[0].clone());
    promise.get(args.agent(), ObjectKey::from("then"))?.call(
        args.agent(),
        promise,
        vec![value_thunk],
    )
}

fn catch_finally_function(args: Args) -> Result<Value, Value> {
    let f = args.this();
    let on_finally = f.get_slot("on finally");
    let result = on_finally.call(args.agent(), Value::Null, vec![])?;
    let c = f.get_slot("constructor");
    let promise = promise_resolve_i(args.agent(), c, result)?;
    let thrower = Value::new_builtin_function(args.agent(), value_thrower);
    thrower.set_slot("value", args[0].clone());
    promise
        .get(args.agent(), ObjectKey::from("then"))?
        .call(args.agent(), promise, vec![thrower])
}

fn promise_proto_finally(args: Args) -> Result<Value, Value> {
    let promise = args.this();
    if promise.type_of() != "object" && promise.type_of() != "function" {
        return Err(Value::new_error(args.agent(), "invalid this"));
    }

    let c = promise.get(args.agent(), ObjectKey::from("constructor"))?;
    if c.type_of() != "object" && c.type_of() != "function" {
        return Err(Value::new_error(
            args.agent(),
            "this does not derive a valid constructor",
        ));
    }

    let (then_finally, catch_finally) = if args[0].type_of() == "function" {
        let then_finally = Value::new_builtin_function(args.agent(), then_finally_function);
        then_finally.set_slot("constructor", c.clone());
        then_finally.set_slot("on finally", args[0].clone());
        let catch_finally = Value::new_builtin_function(args.agent(), catch_finally_function);
        catch_finally.set_slot("constructor", c);
        catch_finally.set_slot("on finally", args[0].clone());
        (then_finally, catch_finally)
    } else {
        (args[0].clone(), args[0].clone())
    };

    promise.get(args.agent(), ObjectKey::from("then"))?.call(
        args.agent(),
        promise,
        vec![then_finally, catch_finally],
    )
}

pub(crate) fn create_promise_prototype(agent: &Agent) -> Value {
    let p = Value::new_object(agent.intrinsics.object_prototype.clone());

    p.set(
        agent,
        ObjectKey::from("then"),
        Value::new_builtin_function(agent, promise_proto_then),
    )
    .expect("unable to set then on promise prototype");
    p.set(
        agent,
        ObjectKey::from("catch"),
        Value::new_builtin_function(agent, promise_proto_catch),
    )
    .expect("unable to set catch on promise prototype");
    p.set(
        agent,
        ObjectKey::from("finally"),
        Value::new_builtin_function(agent, promise_proto_finally),
    )
    .expect("unable to set finally on promise prototype");

    p
}
