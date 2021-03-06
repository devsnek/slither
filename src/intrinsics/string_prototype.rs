use crate::value::{Args, ObjectKey, ObjectKind};
use crate::{Agent, Value};
use unic::normal::StrNormalForm;

fn normalize(args: Args) -> Result<Value, Value> {
    if let Value::Object(o) = args.this() {
        if let ObjectKind::String(s) = &o.kind {
            match &args[0] {
                Value::String(form) => Ok(Value::from(match form.as_str() {
                    "NFC" => s.iter().cloned().nfc().collect::<String>(),
                    "NFD" => s.iter().cloned().nfd().collect::<String>(),
                    "NFKC" => s.iter().cloned().nfkc().collect::<String>(),
                    "NFKD" => s.iter().cloned().nfkd().collect::<String>(),
                    _ => {
                        return Err(Value::new_error(
                            args.agent(),
                            "The normalization form should be one of NFC, NFD, NFKC, NFKD.",
                        ));
                    }
                })),
                Value::Null => Ok(Value::from(s.iter().cloned().nfc().collect::<String>())),
                _ => Err(Value::new_error(
                    args.agent(),
                    "The normalization form should be one of NFC, NFD, NFKC, NFKD.",
                )),
            }
        } else {
            Err(Value::new_error(args.agent(), "invalid receiver"))
        }
    } else {
        Err(Value::new_error(args.agent(), "invalid receiver"))
    }
}

fn substring(args: Args) -> Result<Value, Value> {
    if let Value::Object(o) = args.this() {
        if let ObjectKind::String(s) = &o.kind {
            if let Value::Number(start) = args[0] {
                let end = if let Value::Number(n) = args[1] {
                    n as usize
                } else if args[1] == Value::Null {
                    s.len()
                } else {
                    return Err(Value::new_error(
                        args.agent(),
                        "second argument must be a number",
                    ));
                };
                let start = start as usize;
                Ok(Value::String(s[start..end].iter().cloned().collect()))
            } else {
                Err(Value::new_error(
                    args.agent(),
                    "first argument must be a number",
                ))
            }
        } else {
            Err(Value::new_error(args.agent(), "invalid receiver"))
        }
    } else {
        Err(Value::new_error(args.agent(), "invalid receiver"))
    }
}

pub(crate) fn create_string_prototype(agent: &Agent) -> Value {
    let p = Value::new_object(agent.intrinsics.object_prototype.clone());

    p.set(
        agent,
        ObjectKey::from("normalize"),
        Value::new_builtin_function(agent, normalize, false),
    )
    .unwrap();
    p.set(
        agent,
        ObjectKey::from("substring"),
        Value::new_builtin_function(agent, substring, false),
    )
    .unwrap();

    p
}
