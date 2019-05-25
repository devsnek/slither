use crate::agent::Agent;
use crate::value::{Args, ObjectKey, Value, ValueType};

fn trigger_promise_reactions(
    agent: &Agent,
    reactions: Value,
    argument: Value,
) -> Result<Value, Value> {
    if let Value::List(list) = &reactions {
        loop {
            let item = list.borrow_mut().pop_front();
            match item {
                Some(reaction) => {
                    agent.enqueue_job(
                        promise_reaction_job,
                        vec![reaction.clone(), argument.clone()],
                    );
                }
                None => break,
            }
        }
    } else {
        unreachable!();
    }
    Ok(Value::Null)
}

pub(crate) fn promise_reaction_job(agent: &Agent, args: Vec<Value>) -> Result<(), Value> {
    let reaction = args[0].clone();
    let argument = args[1].clone();

    let promise = reaction.get_slot("promise");
    let kind = reaction.get_slot("kind");
    let handler = reaction.get_slot("handler");

    let mut handler_result;
    if handler == Value::Null {
        if kind == Value::from("fulfill") {
            handler_result = Ok(argument);
        } else {
            handler_result = Err(argument);
        }
    } else {
        handler_result = handler.call(agent, Value::Null, vec![argument]);
    }

    if promise != Value::Null {
        match handler_result {
            Ok(v) => promise
                .get_slot("resolve")
                .call(agent, Value::Null, vec![v])?,
            Err(v) => promise
                .get_slot("reject")
                .call(agent, Value::Null, vec![v])?,
        };
    }

    Ok(())
}

fn fulfill_promise(agent: &Agent, promise: Value, value: Value) -> Result<Value, Value> {
    let reactions = promise.get_slot("fulfill reactions");
    promise.set_slot("result", value.clone());
    promise.set_slot("promise state", Value::from("fulfilled"));
    promise.set_slot("fulfill reactions", Value::Null);
    promise.set_slot("reject reactions", Value::Null);
    trigger_promise_reactions(agent, reactions, value)
}

fn reject_promise(agent: &Agent, promise: Value, reason: Value) -> Result<Value, Value> {
    let reactions = promise.get_slot("reject reactions");
    promise.set_slot("result", reason.clone());
    promise.set_slot("promise state", Value::from("rejected"));
    promise.set_slot("fulfill reactions", Value::Null);
    promise.set_slot("reject reactions", Value::Null);
    if promise.has_slot("promise handled") {
        agent.uncaught_exception(reason.clone());
    }
    trigger_promise_reactions(agent, reactions, reason)
}

struct ResolvingFunctions {
    resolve: Value,
    reject: Value,
}

fn create_resolving_functions(agent: &Agent, promise: &Value) -> ResolvingFunctions {
    let already_resolved = Value::new_custom_object(Value::Null);
    already_resolved.set_slot("resolved", Value::from(false));

    let resolve = Value::new_builtin_function(agent, promise_resolve_function);
    resolve.set_slot("promise", promise.clone());
    resolve.set_slot("already resolved", already_resolved.clone());

    let reject = Value::new_builtin_function(agent, promise_reject_function);
    reject.set_slot("promise", promise.clone());
    reject.set_slot("already resolved", already_resolved);

    ResolvingFunctions { resolve, reject }
}

fn promise_resolve_function(args: Args) -> Result<Value, Value> {
    let already_resolved = args.function().get_slot("already resolved");
    if already_resolved.get_slot("resolved") == Value::from(true) {
        return Ok(Value::Null);
    } else {
        already_resolved.set_slot("resolved", Value::from(true));
    }

    let promise = args.function().get_slot("promise");
    if promise == args[0] {
        reject_promise(
            args.agent(),
            promise,
            Value::new_error(args.agent(), "cannot resolve a promise with itself"),
        )
    } else if args[0].has_slot("promise state") {
        let ResolvingFunctions { resolve, reject } =
            create_resolving_functions(args.agent(), &promise);
        let then_call_result = args[0].get(args.agent(), ObjectKey::from("then"))?.call(
            args.agent(),
            args[0].clone(),
            vec![resolve, reject.clone()],
        );
        match then_call_result {
            Ok(v) => Ok(v),
            Err(e) => reject.call(args.agent(), Value::Null, vec![e]),
        }
    } else {
        fulfill_promise(args.agent(), promise, args[0].clone())
    }
}

fn promise_reject_function(args: Args) -> Result<Value, Value> {
    let already_resolved = args.function().get_slot("already resolved");
    if already_resolved.get_slot("resolved") == Value::from(true) {
        return Ok(Value::Null);
    } else {
        already_resolved.set_slot("resolved", Value::from(true));
    }

    let promise = args.function().get_slot("promise");
    reject_promise(args.agent(), promise, args[0].clone())
}

fn promise(args: Args) -> Result<Value, Value> {
    if args[0].type_of() != ValueType::Function {
        return Err(Value::new_error(
            args.agent(),
            "executor must be a function",
        ));
    }

    let promise = Value::new_custom_object(args.agent().intrinsics.promise_prototype.clone());
    promise.set_slot("promise state", Value::from("pending"));
    promise.set_slot("fulfill reactions", Value::new_list());
    promise.set_slot("reject reactions", Value::new_list());

    let ResolvingFunctions { resolve, reject } = create_resolving_functions(args.agent(), &promise);

    let result = args[0].call(args.agent(), Value::Null, vec![resolve, reject.clone()]);

    if let Err(e) = result {
        reject.call(args.agent(), Value::Null, vec![e])?;
    }

    Ok(promise)
}

fn get_capabilities_executor(args: Args) -> Result<Value, Value> {
    let f = args.function();

    if f.get_slot("resolve") != Value::Null || f.get_slot("reject") != Value::Null {
        return Err(Value::new_error(args.agent(), "type error"));
    }

    f.set_slot("resolve", args[0].clone());
    f.set_slot("reject", args[1].clone());

    Ok(Value::Null)
}

pub(crate) fn new_promise_capability(agent: &Agent, constructor: Value) -> Result<Value, Value> {
    let executor = Value::new_builtin_function(agent, get_capabilities_executor);
    executor.set_slot("resolve", Value::Null);
    executor.set_slot("reject", Value::Null);

    let promise = constructor.construct(agent, vec![executor.clone()], constructor.clone())?;
    promise.set_slot("resolve", executor.get_slot("resolve"));
    promise.set_slot("reject", executor.get_slot("reject"));

    Ok(promise)
}

pub(crate) fn promise_resolve_i(agent: &Agent, c: Value, x: Value) -> Result<Value, Value> {
    if x.has_slot("promise state") {
        let x_constructor = x.get(agent, ObjectKey::from("constructor"))?;
        if x_constructor == c {
            return Ok(x.clone());
        }
    }
    let capability = new_promise_capability(agent, c)?;
    capability
        .get_slot("resolve")
        .call(agent, Value::Null, vec![x.clone()])?;
    Ok(capability)
}

fn promise_resolve(args: Args) -> Result<Value, Value> {
    let c = args.this();
    if c.type_of() != ValueType::Object && c.type_of() != ValueType::Function {
        return Err(Value::new_error(args.agent(), "this must be an object"));
    }
    promise_resolve_i(args.agent(), c, args[0].clone())
}

fn promise_reject(args: Args) -> Result<Value, Value> {
    let c = args.this();
    if c.type_of() != ValueType::Object && c.type_of() != ValueType::Function {
        return Err(Value::new_error(args.agent(), "this must be an object"));
    }
    let capability = new_promise_capability(args.agent(), c)?;
    capability
        .get_slot("reject")
        .call(args.agent(), Value::Null, vec![args[0].clone()])?;
    Ok(capability)
}

pub(crate) fn create_promise(agent: &Agent) -> Value {
    let p = Value::new_builtin_function(agent, promise);

    p.set(
        agent,
        ObjectKey::from("prototype"),
        agent.intrinsics.promise_prototype.clone(),
    )
    .unwrap();
    p.set(
        agent,
        ObjectKey::from("resolve"),
        Value::new_builtin_function(agent, promise_resolve),
    )
    .unwrap();
    p.set(
        agent,
        ObjectKey::from("reject"),
        Value::new_builtin_function(agent, promise_reject),
    )
    .unwrap();
    agent
        .intrinsics
        .promise_prototype
        .set(agent, ObjectKey::from("constructor"), p.clone())
        .unwrap();

    p
}
