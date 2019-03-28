use crate::agent::Agent;
use crate::interpreter::Context;
use crate::value::{ObjectKey, ObjectKind, Value};

fn to_string(agent: &Agent, _: Vec<Value>, ctx: &Context) -> Result<Value, Value> {
    let this = ctx.scope.borrow().get_this(agent)?;

    match this {
        Value::Object(o) => match o.kind {
            ObjectKind::Number(d) => Ok(Value::String(d.to_string())),
            _ => Err(Value::new_error(agent, "invalid receiver")),
        },
        _ => Err(Value::new_error(agent, "invalid receiver")),
    }
}

pub fn create_number_prototype(agent: &Agent) -> Value {
    let proto = Value::new_object(agent.intrinsics.object_prototype.clone());

    proto
        .set(
            agent,
            ObjectKey::from("toString"),
            Value::new_builtin_function(agent, to_string),
        )
        .unwrap();

    macro_rules! FN_1 {
        ( $n:ident ) => {
            fn $n(agent: &Agent, _: Vec<Value>, ctx: &Context) -> Result<Value, Value> {
                if let Value::Object(o) = ctx.scope.borrow().get_this(agent)? {
                    if let ObjectKind::Number(n) = o.kind {
                        Ok(Value::Number(n.$n()))
                    } else {
                        Err(Value::new_error(agent, "invalid receiver"))
                    }
                } else {
                    Err(Value::new_error(agent, "invalid receiver"))
                }
            }
            proto
                .set(
                    agent,
                    ObjectKey::from(stringify!($n)),
                    Value::new_builtin_function(agent, $n),
                )
                .unwrap();
        };
    }

    FN_1!(floor);
    FN_1!(ceil);
    FN_1!(round);
    FN_1!(trunc);
    FN_1!(abs);
    FN_1!(sqrt);
    FN_1!(ln);
    FN_1!(log2);
    FN_1!(log10);
    FN_1!(sin);
    FN_1!(cos);
    FN_1!(tan);
    FN_1!(asin);
    FN_1!(acos);
    FN_1!(atan);

    macro_rules! CHECK {
        ( $n:ident, $sln:expr ) => {
            fn $n(agent: &Agent, _: Vec<Value>, ctx: &Context) -> Result<Value, Value> {
                if let Value::Object(o) = ctx.scope.borrow().get_this(agent)? {
                    if let ObjectKind::Number(n) = o.kind {
                        Ok(if n.$n() { Value::True } else { Value::False })
                    } else {
                        Err(Value::new_error(agent, "invalid receiver"))
                    }
                } else {
                    Err(Value::new_error(agent, "invalid receiver"))
                }
            }
            proto
                .set(
                    agent,
                    ObjectKey::from($sln),
                    Value::new_builtin_function(agent, $n),
                )
                .unwrap();
        };
    }

    CHECK!(is_nan, "isNaN");
    CHECK!(is_infinite, "isInfinite");
    CHECK!(is_finite, "isFinite");

    proto
}
