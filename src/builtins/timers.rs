use crate::agent::{Agent, MioMapType};
use crate::value::{new_builtin_function, new_error, Value};
use crate::vm::ExecutionContext;
use mio::{PollOpt, Ready, Registration, SetReadiness, Token};
use num::ToPrimitive;
use std::collections::HashMap;
use std::sync::Mutex;
use std::time::{Duration, Instant};

use crate::linked_list::LinkedList;

struct TimerList {
    instant: Instant,
    timers: LinkedList<SetReadiness>,
}

impl TimerList {
    fn new(instant: Instant, timer: SetReadiness) -> Self {
        let mut timers = LinkedList::new();
        timers.push_back(timer);
        TimerList { instant, timers }
    }
}

lazy_static! {
    static ref TIMERS: Mutex<LinkedList<TimerList>> = Mutex::new(LinkedList::new());
    static ref THREAD: std::thread::JoinHandle<()> = std::thread::spawn(move || loop {
        let mut timers = TIMERS.lock().unwrap();
        if let Some(list) = timers.cursor().next() {
            if Instant::now() >= list.instant {
                while let Some(r) = list.timers.pop_front() {
                    r.set_readiness(Ready::readable())
                        .expect("failed to set timer readiness");
                }
                timers.pop_front();
            }
        } else {
            std::thread::park();
        }
    });
}

fn insert(instant: Instant, timer: SetReadiness) {
    let mut timers = TIMERS.lock().unwrap();
    let mut cursor = timers.cursor();
    while let Some(item) = cursor.peek_next() {
        if item.instant == instant {
            item.timers.push_back(timer);
            return;
        }

        if item.instant > instant {
            cursor.insert(TimerList::new(instant, timer));
            return;
        }

        cursor.next();
    }

    // empty list or instant is greater than every item in the list
    timers.push_back(TimerList::new(instant, timer));
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
        Value::Number(n) => {
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

            insert(end, set_readiness);
            THREAD.thread().unpark();

            Ok(Value::Null)
        }
        _ => Err(new_error("duration must be a number")),
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
