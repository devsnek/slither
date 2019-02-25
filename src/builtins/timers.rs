use crate::agent::{Agent, MioMapType};
use crate::value::{new_builtin_function, new_error, Value};
use crate::vm::ExecutionContext;
use mio::{PollOpt, Ready, Registration, SetReadiness, Token};
use num::ToPrimitive;
use std::collections::HashMap;
use std::sync::Mutex;
use std::time::{Duration, Instant};

struct TimerEntry {
    instant: Instant,
    readiness: SetReadiness,
}

lazy_static! {
    static ref TIMER_LISTS: Mutex<HashMap<Instant, Vec<TimerEntry>>> = Mutex::new(HashMap::new());
    static ref TIMER_SMALLEST: Mutex<Option<Instant>> = Mutex::new(None);
}

fn update_smallest(entries: &mut HashMap<Instant, Vec<TimerEntry>>) {
    let mut keys = entries.keys().collect::<Vec<&Instant>>();
    keys.sort();
    let mut smallest = TIMER_SMALLEST.lock().unwrap();
    *smallest = match keys.get(0) {
        Some(k) => Some(**k),
        None => None,
    }
}

fn insert(timer: TimerEntry) {
    let mut entries = TIMER_LISTS.lock().unwrap();
    match entries.get_mut(&timer.instant) {
        Some(list) => list.push(timer),
        None => {
            let instant = timer.instant;
            entries.insert(instant, vec![timer]);
            update_smallest(&mut entries);
        }
    }
}

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

            insert(TimerEntry {
                instant: end,
                readiness: set_readiness,
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

    std::thread::spawn(move || loop {
        let smallest = *TIMER_SMALLEST.lock().unwrap();
        if let Some(k) = smallest {
            let now = Instant::now();
            if now >= k {
                let mut entries = TIMER_LISTS.lock().unwrap();
                let list = entries.get_mut(&k).unwrap();
                loop {
                    let timer = list.pop();
                    match timer {
                        Some(TimerEntry { readiness, .. }) => {
                            readiness.set_readiness(Ready::readable()).unwrap();
                        }
                        None => break,
                    }
                }
                entries.remove(&k);
                update_smallest(&mut entries);
            }
        }
    });

    module
}
