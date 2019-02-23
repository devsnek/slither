use crate::agent::Agent;
use crate::intrinsics::promise::{new_promise_capability, promise_reaction_job};
use crate::value::{new_builtin_function, new_custom_object, new_object, ObjectKey, Value};
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

pub fn create_promise_prototype(agent: &Agent, object_prototype: Value) -> Value {
    let p = new_object(object_prototype);

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

    p
}
