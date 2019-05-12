use crate::interpreter::Context;
use crate::value::ObjectKey;
use crate::{Agent, Value};

// TODO: figure out how to make this a tail call
fn call(agent: &Agent, mut args: Vec<Value>, ctx: &Context) -> Result<Value, Value> {
    let this = args.remove(0);
    ctx.scope.borrow().get_this(agent)?.call(agent, this, args)
}

pub fn create_function_prototype(agent: &mut Agent) {
    let proto = Value::new_object(agent.intrinsics.object_prototype.clone());
    agent.intrinsics.function_prototype = proto.clone();

    proto
        .set(
            agent,
            ObjectKey::from("call"),
            Value::new_builtin_function(agent, call),
        )
        .unwrap();
}
