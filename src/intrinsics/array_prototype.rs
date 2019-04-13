use crate::interpreter::Context;
use crate::sort::merge_sort;
use crate::value::{ObjectKey, ObjectKind};
use crate::{Agent, Value};

fn user_sort(agent: &Agent, f: &Value, a: &Value, b: &Value) -> Result<std::cmp::Ordering, Value> {
    match f.call(agent, Value::Null, vec![a.clone(), b.clone()])? {
        Value::Number(n) => {
            if n == 0.0 {
                Ok(std::cmp::Ordering::Equal)
            } else if n > 0.0 {
                Ok(std::cmp::Ordering::Greater)
            } else if n < 0.0 {
                Ok(std::cmp::Ordering::Less)
            } else {
                Err(Value::new_error(agent, "invalid ordering"))
            }
        }
        _ => Err(Value::new_error(agent, "invalid ordering")),
    }
}

fn builtin_sort(agent: &Agent, a: &Value, b: &Value) -> Result<std::cmp::Ordering, Value> {
    match a.partial_cmp(b) {
        Some(o) => Ok(o),
        None => Err(Value::new_error(agent, "invalid ordering")),
    }
}

fn sort(agent: &Agent, args: Vec<Value>, ctx: &Context) -> Result<Value, Value> {
    match ctx.scope.borrow().get_this(agent)? {
        Value::Object(o) => match &o.kind {
            ObjectKind::Array(values) => {
                match args.get(0).unwrap_or(&Value::Null) {
                    Value::Null => {
                        merge_sort(&mut values.borrow_mut(), |a, b| -> Result<bool, Value> {
                            Ok(builtin_sort(agent, a, b)? == std::cmp::Ordering::Less)
                        })?
                    }
                    v => merge_sort(&mut values.borrow_mut(), |a, b| -> Result<bool, Value> {
                        Ok(user_sort(agent, v, a, b)? == std::cmp::Ordering::Less)
                    })?,
                };
                Ok(ctx.scope.borrow().get_this(agent)?)
            }
            _ => Err(Value::new_error(agent, "invalid receiver")),
        },
        _ => Err(Value::new_error(agent, "invalid receiver")),
    }
}

fn iterator(agent: &Agent, _args: Vec<Value>, ctx: &Context) -> Result<Value, Value> {
    let it = Value::new_custom_object(agent.intrinsics.array_iterator_prototype.clone());
    it.set_slot("array iterator next index", Value::from(0));
    it.set_slot("iterated object", ctx.scope.borrow().get_this(agent)?);
    Ok(it)
}

pub fn create_array_prototype(agent: &Agent) -> Value {
    let p = Value::new_object(agent.intrinsics.object_prototype.clone());

    p.set(
        agent,
        ObjectKey::from("sort"),
        Value::new_builtin_function(agent, sort),
    )
    .unwrap();

    p.set(
        agent,
        Value::new_well_known_symbol("iterator")
            .to_object_key(agent)
            .unwrap(),
        Value::new_builtin_function(agent, iterator),
    )
    .unwrap();

    p
}
