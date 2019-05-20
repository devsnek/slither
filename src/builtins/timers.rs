use crate::agent::{Agent, MioMapType};
use crate::intrinsics::promise::promise_resolve_i;
use crate::linked_list::LinkedList;
use crate::value::{Args, Value};
use lazy_static::lazy_static;
use mio::{PollOpt, Ready, Registration, SetReadiness};
use num::ToPrimitive;
use std::collections::HashMap;
use std::sync::{Mutex, MutexGuard};
use std::time::{Duration, Instant};

#[derive(Debug, Finalize)]
pub(crate) enum Timer {
    Once(mio::Registration, Value),
    Repeat(mio::Registration, Value),
}

unsafe impl gc::Trace for Timer {
    custom_trace!(this, {
        match this {
            Timer::Once(_, v) => mark(v),
            Timer::Repeat(_, v) => mark(v),
        }
    });
}

struct TimerList {
    instant: Instant,
    timers: LinkedList<(SetReadiness, u64, bool)>,
}

impl TimerList {
    fn new(instant: Instant, timer: SetReadiness, duration: u64, repeat: bool) -> Self {
        let mut timers = LinkedList::new();
        timers.push_back((timer, duration, repeat));
        TimerList { instant, timers }
    }
}

lazy_static! {
    static ref TIMERS: Mutex<LinkedList<TimerList>> = Mutex::new(LinkedList::new());
    static ref THREAD: std::thread::JoinHandle<()> = std::thread::spawn(move || loop {
        let mut to_readd = Vec::new();
        let mut timers = TIMERS.lock().unwrap();
        if let Some(list) = timers.cursor().next() {
            if Instant::now() >= list.instant {
                while let Some((r, d, repeat)) = list.timers.pop_front() {
                    r.set_readiness(Ready::readable())
                        .expect("failed to set timer readiness");
                    if repeat {
                        to_readd.push((d, r));
                    }
                }
                timers.pop_front();
            }
        } else {
            std::thread::park();
        }
        for (duration, r) in to_readd {
            insert(&mut timers, duration, r, true);
        }
    });
}

fn call_timer_job(agent: &Agent, args: Vec<Value>) -> Result<(), Value> {
    args[0].call(agent, Value::Null, Vec::new())?;
    Ok(())
}

pub(crate) fn handle(agent: &Agent, _token: mio::Token, timer: &Timer) -> bool {
    match timer {
        Timer::Once(_, callback) => {
            agent.enqueue_job(call_timer_job, vec![callback.clone()]);
            false
        }
        Timer::Repeat(_, iter) => {
            if let Value::List(queue) = iter.get_slot("timer queue") {
                let value = Value::new_iter_result(agent, Value::Null, false).unwrap();
                if let Some(promise) = queue.borrow_mut().pop_front() {
                    promise
                        .get_slot("resolve")
                        .call(agent, Value::Null, vec![value])
                        .unwrap();
                } else if let Value::List(buffer) = value.get_slot("timer buffer") {
                    buffer.borrow_mut().push_back(
                        promise_resolve_i(agent, agent.intrinsics.promise.clone(), value).unwrap(),
                    );
                } else {
                    unreachable!();
                }
            } else {
                unreachable!();
            }
            true
        }
    }
}

fn insert(
    timers: &mut MutexGuard<LinkedList<TimerList>>,
    duration: u64,
    timer: SetReadiness,
    repeat: bool,
) {
    let instant = Instant::now() + Duration::from_millis(duration);
    let mut cursor = timers.cursor();
    while let Some(item) = cursor.peek_next() {
        if item.instant == instant {
            item.timers.push_back((timer, duration, repeat));
            return;
        }

        if item.instant > instant {
            cursor.insert(TimerList::new(instant, timer, duration, repeat));
            return;
        }

        cursor.next();
    }

    // empty list or instant is greater than every item in the list
    timers.push_back(TimerList::new(instant, timer, duration, repeat));
}

fn create_timeout(args: Args) -> Result<Value, Value> {
    if args[0].type_of() != "function" {
        return Err(Value::new_error(
            args.agent(),
            "callback must be a function",
        ));
    }
    let duration = args[1].as_f64(args.agent())?.to_u64().unwrap();

    let (registration, set_readiness) = Registration::new2();
    let token = args.agent().mio_token();

    args.agent()
        .mio
        .register(&registration, token, Ready::readable(), PollOpt::edge())
        .unwrap();
    args.agent().mio_map.borrow_mut().insert(
        token,
        MioMapType::Timer(Timer::Once(registration, args[0].clone())),
    );

    insert(&mut TIMERS.lock().unwrap(), duration, set_readiness, false);
    THREAD.thread().unpark();

    // TODO: return object with cancel()
    Ok(Value::Null)
}

fn create_interval(args: Args) -> Result<Value, Value> {
    let duration = args[0].as_f64(args.agent())?.to_u64().unwrap();

    let (registration, set_readiness) = Registration::new2();
    let token = args.agent().mio_token();

    args.agent()
        .mio
        .register(&registration, token, Ready::readable(), PollOpt::edge())
        .unwrap();

    let iter = Value::new_custom_object(args.agent().intrinsics.timer_iterator_prototype.clone());
    iter.set_slot("timer buffer", Value::new_list());
    iter.set_slot("timer queue", Value::new_list());
    iter.set_slot("timer token", Value::from(token.0 as f64));

    args.agent().mio_map.borrow_mut().insert(
        token,
        MioMapType::Timer(Timer::Repeat(registration, iter.clone())),
    );
    insert(&mut TIMERS.lock().unwrap(), duration, set_readiness, true);
    THREAD.thread().unpark();

    Ok(iter)
}

pub(crate) fn create(agent: &Agent) -> HashMap<String, Value> {
    let mut module = HashMap::new();
    module.insert(
        "createTimeout".to_string(),
        Value::new_builtin_function(agent, create_timeout),
    );
    module.insert(
        "createInterval".to_string(),
        Value::new_builtin_function(agent, create_interval),
    );

    module
}
