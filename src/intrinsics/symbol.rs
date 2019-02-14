use crate::module::{Agent, ExecutionContext};
use crate::value::{new_builtin_function, new_error, ObjectKey, Value};

fn symbol(_agent: &Agent, _ctx: &mut ExecutionContext, args: Vec<Value>) -> Result<Value, Value> {
    let desc = match args.get(0) {
        Some(Value::String(s)) => Ok(Some(s.clone())),
        Some(_) => Err(new_error("invalid description")),
        None => Ok(None),
    }?;
    Ok(Value::new_symbol(desc))
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
