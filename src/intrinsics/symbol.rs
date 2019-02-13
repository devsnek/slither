use crate::module::{Agent, ExecutionContext};
use crate::value::{new_builtin_function, Value, ObjectKey};

fn symbol(_agent: &Agent, _ctx: &mut ExecutionContext, _args: Vec<Value>) -> Result<Value, Value> {
    Ok(Value::new_symbol(false))
}

pub fn create_symbol(agent: &Agent, prototype: Value) -> Value {
    let s = new_builtin_function(agent, symbol);

    if let Value::Object(o) = &s {
        o.set(ObjectKey::from("prototype"), prototype.clone(), o.clone())
            .expect("failed to set prototype on promise constructor");
        if let Value::Object(pt) = &prototype {
            pt.set(ObjectKey::from("constructor"), s.clone(), pt.clone())
                .expect("failed to set constructor on promise prototype");
        }
    } else {
        unreachable!();
    }

    s
}
