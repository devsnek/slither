use crate::agent::{Agent, MioMapType};
use crate::linked_list::LinkedList;
use crate::value::{Args, Value};
use lazy_static::lazy_static;
use mio::{PollOpt, Ready, Registration, SetReadiness};
use num::ToPrimitive;
use std::collections::HashMap;
use std::sync::Mutex;
use std::time::{Duration, Instant};

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

fn create_timeout(args: Args) -> Result<Value, Value> {
    if args[0].type_of() != "function" {
        return Err(Value::new_error(
            args.agent(),
            "callback must be a function",
        ));
    }
    match args[1] {
        Value::Number(n) => {
            let end = Instant::now() + Duration::from_millis(n.to_u64().unwrap());

            let (registration, set_readiness) = Registration::new2();
            let token = args.agent().mio_token();

            args.agent()
                .mio
                .register(&registration, token, Ready::readable(), PollOpt::edge())
                .unwrap();
            args.agent()
                .mio_map
                .borrow_mut()
                .insert(token, MioMapType::Timer(registration, args[0].clone()));

            insert(end, set_readiness);
            THREAD.thread().unpark();

            // TODO: return object with cancel()
            Ok(Value::Null)
        }
        _ => Err(Value::new_error(args.agent(), "duration must be a number")),
    }
}

pub fn create(agent: &Agent) -> HashMap<String, Value> {
    let mut module = HashMap::new();
    module.insert(
        "createTimeout".to_string(),
        Value::new_builtin_function(agent, create_timeout),
    );

    module
}
