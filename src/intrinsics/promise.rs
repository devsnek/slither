use crate::module::{call, Agent, ExecutionContext};
use crate::value::{new_builtin_function, new_error, ObjectInfo, ObjectKind, ObjectKey, Value};
use gc::{Gc, GcCell};
use std::collections::HashMap;

#[derive(Debug)]
pub struct PromiseCapability {
    promise: Value,
    resolve: Value,
    reject: Value,
}

#[derive(Debug)]
pub enum PromiseState {
    Pending,
    Fulfilled,
    Rejected,
}

#[derive(Debug)]
enum PromiseReactionKind {
    Fulfill,
    Reject,
}
#[derive(Debug)]
pub struct PromiseReaction {
    capability: PromiseCapability,
    kind: PromiseReactionKind,
    handler: Value,
}

fn promise_resolve_function(
    _agent: &Agent,
    ctx: &mut ExecutionContext,
    _args: Vec<Value>,
) -> Result<Value, Value> {
    let f = ctx.function.clone().unwrap();
    f.type_of();
    Ok(Value::Null)
}
fn promise_reject_function(
    _agent: &Agent,
    _ctx: &mut ExecutionContext,
    _args: Vec<Value>,
) -> Result<Value, Value> {
    Ok(Value::Null)
}

fn promise(agent: &Agent, ctx: &mut ExecutionContext, args: Vec<Value>) -> Result<Value, Value> {
    let executor = args[0].clone();

    if executor.type_of() != "function" {
        return Err(new_error("executor must be a function"));
    }

    let promise = Value::Object(Gc::new(ObjectInfo {
        kind: ObjectKind::Promise(PromiseState::Pending, Value::Null, Vec::new(), Vec::new()),
        properties: GcCell::new(HashMap::new()),
        prototype: agent.intrinsics.promise_prototype.clone(),
    }));

    let resolve = new_builtin_function(agent, promise_resolve_function);
    let reject = new_builtin_function(agent, promise_reject_function);

    let result = call(
        agent,
        ctx,
        executor,
        Value::Null,
        vec![resolve, reject.clone()],
    );

    if let Err(e) = result {
        call(agent, ctx, reject, Value::Null, vec![e])?;
    }

    Ok(promise)
}

pub fn create_promise(agent: &Agent, prototype: Value) -> Value {
    let p = new_builtin_function(agent, promise);

    if let Value::Object(o) = &p {
        o.set(ObjectKey::from("prototype"), prototype.clone(), o.clone())
            .expect("failed to set prototype on promise constructor");
        if let Value::Object(pt) = &prototype {
            pt.set(ObjectKey::from("constructor"), p.clone(), pt.clone())
                .expect("failed to set constructor on promise prototype");
        }
    } else {
        unreachable!();
    }

    return p;
}
