use crate::agent::Agent;
use crate::value::{Args, Value};
use std::collections::HashMap;

fn min(args: Args) -> Result<Value, Value> {
    if args.args().is_empty() {
        return Err(Value::new_error(args.agent(), "argument must be a number"));
    }

    let mut numbers = Vec::new();
    for arg in args.args() {
        if let Value::Number(n) = arg {
            numbers.push(*n);
        } else {
            return Err(Value::new_error(args.agent(), "argument must be a number"));
        }
    }

    numbers.sort_by(|a, b| a.partial_cmp(b).unwrap());

    Ok(Value::from(numbers[0]))
}

fn max(args: Args) -> Result<Value, Value> {
    if args.args().is_empty() {
        return Err(Value::new_error(args.agent(), "argument must be a number"));
    }

    let mut numbers = Vec::new();
    for arg in args.args() {
        if let Value::Number(n) = arg {
            numbers.push(*n);
        } else {
            return Err(Value::new_error(args.agent(), "argument must be a number"));
        }
    }

    numbers.sort_by(|a, b| b.partial_cmp(a).unwrap());

    Ok(Value::from(numbers[0]))
}

pub fn create(agent: &Agent) -> HashMap<String, Value> {
    let mut module = HashMap::new();

    module.insert("min".to_string(), Value::new_builtin_function(agent, min));
    module.insert("max".to_string(), Value::new_builtin_function(agent, max));

    macro_rules! C {
        ($n:ident) => {
            module.insert(
                stringify!($n).to_string(),
                Value::from(std::f64::consts::$n),
            );
        };
    }

    module.insert("NAN".to_string(), Value::from(std::f64::NAN));
    module.insert("INFINITY".to_string(), Value::from(std::f64::INFINITY));
    module.insert("EPSILON".to_string(), Value::from(std::f64::EPSILON));

    C!(E);
    C!(LN_2);
    C!(LN_10);
    C!(PI);
    C!(SQRT_2);

    module
}
