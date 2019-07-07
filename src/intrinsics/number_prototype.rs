use crate::agent::Agent;
use crate::num_util;
use crate::value::{Args, ObjectKey, ObjectKind, Value};

fn to_string(args: Args) -> Result<Value, Value> {
    match args.this() {
        Value::Object(o) => match o.kind {
            ObjectKind::Number(n) => Ok(Value::from(num_util::to_string(n))),
            _ => Err(Value::new_error(args.agent(), "invalid receiver")),
        },
        _ => Err(Value::new_error(args.agent(), "invalid receiver")),
    }
}

pub(crate) fn create_number_prototype(agent: &Agent) -> Value {
    let proto = Value::new_object(agent.intrinsics.object_prototype.clone());

    proto
        .set(
            agent,
            ObjectKey::well_known_symbol("toString"),
            Value::new_builtin_function(agent, to_string, false),
        )
        .unwrap();

    macro_rules! FN_1 {
        ( $n:ident ) => {
            fn $n(args: Args) -> Result<Value, Value> {
                if let Value::Object(o) = args.this() {
                    if let ObjectKind::Number(n) = o.kind {
                        Ok(Value::from(n.$n()))
                    } else {
                        Err(Value::new_error(args.agent(), "invalid receiver"))
                    }
                } else {
                    Err(Value::new_error(args.agent(), "invalid receiver"))
                }
            }
            proto
                .set(
                    agent,
                    ObjectKey::from(stringify!($n)),
                    Value::new_builtin_function(agent, $n, false),
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
            fn $n(args: Args) -> Result<Value, Value> {
                if let Value::Object(o) = args.this() {
                    if let ObjectKind::Number(n) = o.kind {
                        Ok(Value::from(n.$n()))
                    } else {
                        Err(Value::new_error(args.agent(), "invalid receiver"))
                    }
                } else {
                    Err(Value::new_error(args.agent(), "invalid receiver"))
                }
            }
            proto
                .set(
                    agent,
                    ObjectKey::from($sln),
                    Value::new_builtin_function(agent, $n, false),
                )
                .unwrap();
        };
    }

    CHECK!(is_nan, "isNaN");
    CHECK!(is_infinite, "isInfinite");
    CHECK!(is_finite, "isFinite");

    proto
}
