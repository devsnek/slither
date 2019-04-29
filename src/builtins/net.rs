use crate::agent::{Agent, MioMapType};
use crate::interpreter::Context;
use crate::intrinsics::promise::{new_promise_capability, promise_resolve_i};
use crate::value::Value;
use crate::IntoValue;
use mio::{
    net::{TcpListener, TcpStream},
    PollOpt, Ready, Token,
};
use std::collections::HashMap;
use std::io::prelude::*;

#[derive(Debug, Finalize)]
pub enum Net {
    Client(TcpStream, Value),
    Server(TcpListener, Value),
}

unsafe impl gc::Trace for Net {
    custom_trace!(this, {
        match this {
            Net::Client(_, v) => mark(v),
            Net::Server(_, v) => mark(v),
        }
    });
}

fn get_or_create_resolve(kind: &str, agent: &Agent, target: Value, value: Value, done: bool) {
    if let Value::List(queue) = target.get_slot(&format!("net {} queue", kind)) {
        let value = Value::new_iter_result(agent, value, done).unwrap();
        if let Some(promise) = queue.borrow_mut().pop_front() {
            promise
                .get_slot("resolve")
                .call(agent, Value::Null, vec![value])
                .unwrap();
            println!("resolved existing");
        } else if let Value::List(buffer) = target.get_slot(&format!("net {} buffer", kind)) {
            buffer.borrow_mut().push_back(
                promise_resolve_i(agent, agent.intrinsics.promise.clone(), value).unwrap(),
            );
            println!("added new");
        } else {
            unreachable!();
        }
    } else {
        unreachable!();
    }
}

fn get_or_create_reject(kind: &str, agent: &Agent, target: Value, value: Value) {
    if let Value::List(queue) = target.get_slot(&format!("net {} queue", kind)) {
        println!("reject with queue.len = {}", queue.borrow().len());
        if let Some(promise) = queue.borrow_mut().pop_front() {
            promise
                .get_slot("reject")
                .call(agent, Value::Null, vec![value])
                .unwrap();
        } else if let Value::List(buffer) = target.get_slot(&format!("net {} buffer", kind)) {
            let p = new_promise_capability(agent, agent.intrinsics.promise.clone()).unwrap();
            p.get_slot("reject")
                .call(agent, Value::Null, vec![value])
                .unwrap();
            buffer.borrow_mut().push_back(p);
        } else {
            unreachable!();
        }
    } else {
        unreachable!();
    }
}

fn create_client(agent: &Agent, stream: TcpStream) -> Result<Value, Value> {
    let token = agent.mio_token();
    if let Err(e) = agent
        .mio
        .register(&stream, token, Ready::readable(), PollOpt::edge())
    {
        Err(e.into_value(agent))
    } else {
        let client = Value::new_custom_object(agent.intrinsics.net_client_prototype.clone());
        client.set_slot("net client buffer", Value::new_list());
        client.set_slot("net client queue", Value::new_list());
        client.set_slot("net client token", Value::from(token.0 as f64));
        agent
            .mio_map
            .borrow_mut()
            .insert(token, MioMapType::Net(Net::Client(stream, client.clone())));
        Ok(client)
    }
}

pub fn handle(agent: &Agent, token: Token, net: Net) {
    match net {
        Net::Client(mut stream, client) => match stream.take_error() {
            Ok(Some(e)) | Err(e) => {
                let e = Value::new_error(agent, &format!("{}", e));
                get_or_create_reject("client", agent, client, e);
            }
            Ok(None) => {
                let mut buf = Vec::new();
                match stream.read_to_end(&mut buf) {
                    Ok(size) if size == 0 => {
                        get_or_create_resolve("client", agent, client, Value::Null, true);
                        return;
                    }
                    Ok(_) => {
                        let r = Value::new_buffer_from_vec(agent, buf);
                        get_or_create_resolve("client", agent, client.clone(), r, false);
                    }
                    Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                        let r = Value::new_buffer_from_vec(agent, buf);
                        get_or_create_resolve("client", agent, client.clone(), r, false);
                    }
                    Err(e) => {
                        let e = Value::new_error(agent, &format!("{}", e));
                        get_or_create_reject("client", agent, client.clone(), e);
                    }
                }
                agent
                    .mio_map
                    .borrow_mut()
                    .insert(token, MioMapType::Net(Net::Client(stream, client)));
            }
        },
        Net::Server(listener, server) => match listener.take_error() {
            Ok(Some(e)) | Err(e) => {
                let e = Value::new_error(agent, &format!("{}", e));
                get_or_create_reject("server", agent, server, e);
            }
            Ok(None) => {
                match listener.accept() {
                    Ok((stream, ..)) => match create_client(agent, stream) {
                        Ok(client) => {
                            get_or_create_resolve("server", agent, server.clone(), client, false);
                        }
                        Err(e) => {
                            get_or_create_reject("server", agent, server.clone(), e);
                        }
                    },
                    Err(ref e) if e.kind() != std::io::ErrorKind::WouldBlock => {
                        let e = Value::new_error(agent, &format!("{}", e));
                        get_or_create_reject("server", agent, server.clone(), e);
                    }
                    _ => {}
                }
                agent
                    .mio_map
                    .borrow_mut()
                    .insert(token, MioMapType::Net(Net::Server(listener, server)));
            }
        },
    }
}

fn connect(agent: &Agent, args: Vec<Value>, _: &Context) -> Result<Value, Value> {
    match args.get(0).unwrap_or(&Value::Null) {
        Value::String(addr) => {
            let addr: std::net::SocketAddr = match addr.parse() {
                Ok(v) => v,
                Err(e) => return Err(e.into_value(agent)),
            };
            match TcpStream::connect(&addr) {
                Ok(v) => create_client(agent, v),
                Err(e) => Err(e.into_value(agent)),
            }
        }
        _ => Err(Value::new_error(agent, "address must be a string")),
    }
}

fn listen(agent: &Agent, args: Vec<Value>, _: &Context) -> Result<Value, Value> {
    match args.get(0).unwrap_or(&Value::Null) {
        Value::String(addr) => {
            let addr: std::net::SocketAddr = match addr.parse() {
                Ok(v) => v,
                Err(e) => return Err(e.into_value(agent)),
            };
            let listener = match TcpListener::bind(&addr) {
                Ok(v) => v,
                Err(e) => return Err(e.into_value(agent)),
            };
            let token = agent.mio_token();
            match agent
                .mio
                .register(&listener, token, Ready::all(), PollOpt::edge())
            {
                Ok(_) => {
                    let server =
                        Value::new_custom_object(agent.intrinsics.net_server_prototype.clone());
                    server.set_slot("net server buffer", Value::new_list());
                    server.set_slot("net server queue", Value::new_list());
                    server.set_slot("net server token", Value::from(token.0 as f64));
                    agent.mio_map.borrow_mut().insert(
                        token,
                        MioMapType::Net(Net::Server(listener, server.clone())),
                    );
                    Ok(server)
                }
                Err(e) => Err(e.into_value(agent)),
            }
        }
        _ => Err(Value::new_error(agent, "address must be a string")),
    }
}

pub fn create(agent: &Agent) -> HashMap<String, Value> {
    let mut module = HashMap::new();
    module.insert(
        "connect".to_string(),
        Value::new_builtin_function(agent, connect),
    );
    module.insert(
        "listen".to_string(),
        Value::new_builtin_function(agent, listen),
    );

    module
}
