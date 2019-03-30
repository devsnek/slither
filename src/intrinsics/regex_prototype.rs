use crate::agent::Agent;
use crate::interpreter::Context;
use crate::value::{ObjectKey, ObjectKind, Value};

fn match_(agent: &Agent, args: Vec<Value>, ctx: &Context) -> Result<Value, Value> {
    let this = ctx.scope.borrow().get_this(agent)?;
    match this {
        Value::Object(o) => {
            if let ObjectKind::Regex(re) = &o.kind {
                let mut args = args;
                match args.pop().unwrap_or(Value::Null) {
                    Value::String(s) => match re.captures(s.as_str()) {
                        Some(captures) => {
                            let o = Value::new_array(agent);
                            let mut i = 0;
                            for name in re.capture_names() {
                                match name {
                                    Some(s) => {
                                        o.set(
                                            agent,
                                            ObjectKey::from(s),
                                            Value::from(captures.name(s).unwrap().as_str()),
                                        )?;
                                    }
                                    None => {
                                        o.set(
                                            agent,
                                            ObjectKey::from(i),
                                            Value::from(captures.get(i).unwrap().as_str()),
                                        )?;
                                        i += 1;
                                    }
                                }
                            }
                            Ok(o)
                        }
                        None => Ok(Value::Null),
                    },
                    _ => Err(Value::new_error(agent, "input must be a string")),
                }
            } else {
                Err(Value::new_error(agent, "invalid receiver"))
            }
        }
        _ => Err(Value::new_error(agent, "invalid receiver")),
    }
}

fn test(agent: &Agent, args: Vec<Value>, ctx: &Context) -> Result<Value, Value> {
    let this = ctx.scope.borrow().get_this(agent)?;
    match this {
        Value::Object(o) => {
            if let ObjectKind::Regex(re) = &o.kind {
                let mut args = args;
                match args.pop().unwrap_or(Value::Null) {
                    Value::String(s) => Ok(Value::from(re.is_match(s.as_str()))),
                    _ => Err(Value::new_error(agent, "input must be a string")),
                }
            } else {
                Err(Value::new_error(agent, "invalid receiver"))
            }
        }
        _ => Err(Value::new_error(agent, "invalid receiver")),
    }
}

pub fn create_regex_prototype(agent: &Agent) -> Value {
    let proto = Value::new_object(agent.intrinsics.object_prototype.clone());

    proto
        .set(
            agent,
            ObjectKey::from("match"),
            Value::new_builtin_function(agent, match_),
        )
        .unwrap();

    proto
        .set(
            agent,
            ObjectKey::from("test"),
            Value::new_builtin_function(agent, test),
        )
        .unwrap();

    proto
}
