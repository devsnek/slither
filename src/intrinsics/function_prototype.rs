use crate::value::{Args, ObjectKey};
use crate::{Agent, Value};

// TODO: figure out how to make this a tail call
fn call(args: Args) -> Result<Value, Value> {
    let mut vargs = args.args().clone();
    let this = vargs.remove(0);
    args.this().call(args.agent(), this, vargs)
}

pub(crate) fn create_function_prototype(agent: &mut Agent) {
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
