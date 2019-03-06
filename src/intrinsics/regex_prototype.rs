use crate::agent::Agent;
use crate::value::{
    new_array, new_builtin_function, new_error, new_object, ObjectKey, ObjectKind, Value,
};
use crate::vm::ExecutionContext;

fn match_(agent: &Agent, ctx: &ExecutionContext, args: Vec<Value>) -> Result<Value, Value> {
    let this = ctx.environment.borrow().get_this()?;
    match this {
        Value::Object(o) => {
            if let ObjectKind::Regex(re) = &o.kind {
                let mut args = args;
                match args.pop().unwrap_or(Value::Null) {
                    Value::String(s) => match re.captures(s.as_str()) {
                        Some(captures) => {
                            let o = new_array(agent);
                            let mut i = 0;
                            for name in re.capture_names() {
                                match name {
                                    Some(s) => {
                                        o.set(
                                            &ObjectKey::from(s),
                                            Value::String(
                                                captures.name(s).unwrap().as_str().to_string(),
                                            ),
                                        )?;
                                    }
                                    None => {
                                        o.set(
                                            &ObjectKey::from(i),
                                            Value::String(
                                                captures.get(i).unwrap().as_str().to_string(),
                                            ),
                                        )?;
                                        i += 1;
                                    }
                                }
                            }
                            Ok(o)
                        }
                        None => Ok(Value::Null),
                    },
                    _ => Err(new_error("input must be a string")),
                }
            } else {
                Err(new_error("invalid receiver"))
            }
        }
        _ => Err(new_error("invalid receiver")),
    }
}

fn test(_: &Agent, ctx: &ExecutionContext, args: Vec<Value>) -> Result<Value, Value> {
    let this = ctx.environment.borrow().get_this()?;
    match this {
        Value::Object(o) => {
            if let ObjectKind::Regex(re) = &o.kind {
                let mut args = args;
                match args.pop().unwrap_or(Value::Null) {
                    Value::String(s) => Ok(if re.is_match(s.as_str()) {
                        Value::True
                    } else {
                        Value::False
                    }),
                    _ => Err(new_error("input must be a string")),
                }
            } else {
                Err(new_error("invalid receiver"))
            }
        }
        _ => Err(new_error("invalid receiver")),
    }
}

pub fn create_regex_prototype(agent: &Agent) -> Value {
    let proto = new_object(agent.intrinsics.object_prototype.clone());

    proto
        .set(
            &ObjectKey::from("match"),
            new_builtin_function(agent, match_),
        )
        .unwrap();

    proto
        .set(&ObjectKey::from("test"), new_builtin_function(agent, test))
        .unwrap();

    proto
}
