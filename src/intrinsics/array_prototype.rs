use crate::sort::merge_sort;
use crate::value::{Args, ObjectKey, ObjectKind};
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

fn sort(args: Args) -> Result<Value, Value> {
    match args.this() {
        Value::Object(o) => match &o.kind {
            ObjectKind::Array(values) => {
                match &args[0] {
                    Value::Null => {
                        merge_sort(&mut values.borrow_mut(), |a, b| -> Result<bool, Value> {
                            Ok(builtin_sort(args.agent(), a, b)? == std::cmp::Ordering::Less)
                        })?
                    }
                    v => merge_sort(&mut values.borrow_mut(), |a, b| -> Result<bool, Value> {
                        Ok(user_sort(args.agent(), v, a, b)? == std::cmp::Ordering::Less)
                    })?,
                };
                Ok(args.this())
            }
            _ => Err(Value::new_error(args.agent(), "invalid receiver")),
        },
        _ => Err(Value::new_error(args.agent(), "invalid receiver")),
    }
}

fn iterator(args: Args) -> Result<Value, Value> {
    let it = Value::new_custom_object(args.agent().intrinsics.array_iterator_prototype.clone());
    it.set_slot("array iterator next index", Value::from(0));
    it.set_slot("iterated object", args.this());
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
        ObjectKey::well_known_symbol("iterator"),
        Value::new_builtin_function(agent, iterator),
    )
    .unwrap();

    p
}
