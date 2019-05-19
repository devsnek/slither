use crate::agent::Agent;
use crate::value::{Args, ObjectKey, ObjectKind, Value};

fn match_(args: Args) -> Result<Value, Value> {
    match args.this() {
        Value::Object(o) => {
            if let ObjectKind::Regex(re) = &o.kind {
                if let Value::String(s) = &args[0] {
                    if let Some(captures) = re.captures(s.as_str()) {
                        let o = Value::new_array(args.agent());
                        let mut i = 0;
                        for name in re.capture_names() {
                            match name {
                                Some(s) => {
                                    o.set(
                                        args.agent(),
                                        ObjectKey::from(s),
                                        Value::from(captures.name(s).unwrap().as_str()),
                                    )?;
                                }
                                None => {
                                    o.set(
                                        args.agent(),
                                        ObjectKey::from(i),
                                        Value::from(captures.get(i).unwrap().as_str()),
                                    )?;
                                    i += 1;
                                }
                            }
                        }
                        Ok(o)
                    } else {
                        Ok(Value::Null)
                    }
                } else {
                    Err(Value::new_error(args.agent(), "input must be a string"))
                }
            } else {
                Err(Value::new_error(args.agent(), "invalid receiver"))
            }
        }
        _ => Err(Value::new_error(args.agent(), "invalid receiver")),
    }
}

fn test(args: Args) -> Result<Value, Value> {
    match args.this() {
        Value::Object(o) => {
            if let ObjectKind::Regex(re) = &o.kind {
                match &args[0] {
                    Value::String(s) => Ok(Value::from(re.is_match(s.as_str()))),
                    _ => Err(Value::new_error(args.agent(), "input must be a string")),
                }
            } else {
                Err(Value::new_error(args.agent(), "invalid receiver"))
            }
        }
        _ => Err(Value::new_error(args.agent(), "invalid receiver")),
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
