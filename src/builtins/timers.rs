use crate::agent::{Agent, MioMapType};
use crate::value::{new_builtin_function, new_error, Value};
use crate::vm::ExecutionContext;
use mio::{PollOpt, Ready, Registration, Token};
use num::ToPrimitive;
use std::collections::HashMap;
use std::time::{Duration, Instant};

fn create_timeout(
    agent: &Agent,
    _ctx: &ExecutionContext,
    args: Vec<Value>,
) -> Result<Value, Value> {
    let callback = args.get(0).unwrap_or(&Value::Null);
    if callback.type_of() != "function" {
        return Err(new_error("callback must be a function"));
    }
    match args.get(1).unwrap_or(&Value::Null) {
        Value::Integer(n) => {
            let end = Instant::now() + Duration::from_millis(n.to_u64().unwrap());
            let (registration, set_readiness) = Registration::new2();

            let token = Token(agent.mio_map.borrow().len());

            agent
                .mio
                .register(&registration, token, Ready::readable(), PollOpt::edge())
                .unwrap();
            agent
                .mio_map
                .borrow_mut()
                .insert(token, MioMapType::Timer(registration, callback.clone()));

            std::thread::spawn(move || {
                let now = Instant::now();

                if now < end {
                    std::thread::sleep(end - now);
                }

                set_readiness.set_readiness(Ready::readable()).unwrap();
            });

            Ok(Value::Null)
        }
        _ => Err(new_error("duration must be an integer")),
    }
}

pub fn create(agent: &Agent) -> HashMap<String, Value> {
    let mut module = HashMap::new();
    module.insert(
        "createTimeout".to_string(),
        new_builtin_function(agent, create_timeout),
    );

    module
}
