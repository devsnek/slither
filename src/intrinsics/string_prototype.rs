use crate::interpreter::Context;
use crate::value::{ObjectKey, ObjectKind};
use crate::{Agent, Value};
use unic::normal::StrNormalForm;

fn normalize(agent: &Agent, args: Vec<Value>, ctx: &Context) -> Result<Value, Value> {
    if let Value::Object(o) = ctx.scope.borrow().get_this(agent)? {
        if let ObjectKind::String(s) = &o.kind {
            match args.get(0).unwrap_or(&Value::Null) {
                Value::String(form) => Ok(Value::from(match form.as_str() {
                    "NFC" => s.iter().cloned().nfc().collect::<String>(),
                    "NFD" => s.iter().cloned().nfd().collect::<String>(),
                    "NFKC" => s.iter().cloned().nfkc().collect::<String>(),
                    "NFKD" => s.iter().cloned().nfkd().collect::<String>(),
                    _ => {
                        return Err(Value::new_error(
                            agent,
                            "The normalization form should be one of NFC, NFD, NFKC, NFKD.",
                        ));
                    }
                })),
                Value::Null => Ok(Value::from(s.iter().cloned().nfc().collect::<String>())),
                _ => {
                    return Err(Value::new_error(
                        agent,
                        "The normalization form should be one of NFC, NFD, NFKC, NFKD.",
                    ));
                }
            }
        } else {
            Err(Value::new_error(agent, "invalid receiver"))
        }
    } else {
        Err(Value::new_error(agent, "invalid receiver"))
    }
}

pub fn create_string_prototype(agent: &Agent) -> Value {
    let p = Value::new_object(agent.intrinsics.object_prototype.clone());

    p.set(
        agent,
        ObjectKey::from("normalize"),
        Value::new_builtin_function(agent, normalize),
    )
    .unwrap();

    p
}
