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

fn for_each(agent: &Agent, args: Vec<Value>, ctx: &Context) -> Result<Value, Value> {
    match ctx.scope.borrow().get_this(agent)? {
        Value::Object(o) => match &o.kind {
            ObjectKind::Array(values) => {
                for (i, value) in values.borrow().iter().enumerate() {
                    args.get(0).unwrap_or(&Value::Null).call(
                        agent,
                        Value::Null,
                        vec![value.clone(), Value::from(i as f64)],
                    )?;
                }
                Ok(ctx.scope.borrow().get_this(agent)?)
            }
            _ => Err(Value::new_error(agent, "invalid receiver")),
        },
        _ => Err(Value::new_error(agent, "invalid receiver")),
    }
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
        ObjectKey::from("forEach"),
        Value::new_builtin_function(agent, for_each),
    )
    .unwrap();

    p
}
