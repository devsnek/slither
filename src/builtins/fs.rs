use crate::agent::{Agent, MioMapType};
use crate::intrinsics::promise::new_promise_capability;
use crate::value::{new_builtin_function, new_error, Value};
use crate::vm::ExecutionContext;
use mio::{PollOpt, Ready, Registration, Token};
use std::collections::HashMap;
use std::sync::Mutex;

lazy_static! {
    static ref RESPONSES: Mutex<HashMap<Token, FsResponse>> = Mutex::new(HashMap::new());
}

pub enum FsResponse {
    Read(String),
    Error(String),
}

pub fn handle(agent: &Agent, token: Token, promise: Value) {
    let fsr = RESPONSES.lock().unwrap().remove(&token).unwrap();
    match fsr {
        FsResponse::Read(s) => {
            promise
                .get_slot("resolve")
                .call(agent, promise, vec![Value::String(s)])
                .unwrap();
        }
        FsResponse::Error(s) => {
            promise
                .get_slot("reject")
                .call(agent, promise, vec![new_error(s.as_str())])
                .unwrap();
        }
    }
}

fn read_file(agent: &Agent, _c: &ExecutionContext, args: Vec<Value>) -> Result<Value, Value> {
    match args.get(0) {
        Some(Value::String(filename)) => {
            let promise = new_promise_capability(agent, agent.intrinsics.promise.clone())?;

            let (registration, set_readiness) = Registration::new2();
            let token = Token(agent.mio_map.borrow().len());

            agent
                .mio
                .register(&registration, token, Ready::readable(), PollOpt::edge())
                .unwrap();
            agent
                .mio_map
                .borrow_mut()
                .insert(token, MioMapType::FS(registration, promise.clone()));

            let filename = filename.to_string();
            agent
                .pool
                .execute(move || match std::fs::read_to_string(filename) {
                    Ok(s) => {
                        RESPONSES.lock().unwrap().insert(token, FsResponse::Read(s));
                        set_readiness.set_readiness(Ready::readable()).unwrap();
                    }
                    Err(e) => {
                        RESPONSES
                            .lock()
                            .unwrap()
                            .insert(token, FsResponse::Error(format!("{}", e)));
                        set_readiness.set_readiness(Ready::readable()).unwrap();
                    }
                });

            Ok(promise)
        }
        _ => Err(new_error("filename must be a string")),
    }
}

pub fn create(agent: &Agent) -> HashMap<String, Value> {
    let mut module = HashMap::new();
    module.insert(
        "readFile".to_string(),
        new_builtin_function(agent, read_file),
    );

    module
}
