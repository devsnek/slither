use crate::agent::{Agent, MioMapType};
use crate::intrinsics::net_client_prototype::{get_or_create_reject, get_or_create_resolve};
use crate::value::{ObjectKey, Value};
use crate::vm::ExecutionContext;
use mio::{net::TcpStream, PollOpt, Ready, Token};
use std::collections::HashMap;
use std::io::prelude::*;

#[derive(Debug, Finalize)]
pub enum Net {
    Client(TcpStream, Value),
}

unsafe impl gc::Trace for Net {
    custom_trace!(this, {
        match this {
            Net::Client(_, v) => mark(v),
        }
    });
}

pub fn handle(agent: &Agent, token: Token, net: Net) {
    match net {
        Net::Client(mut stream, client) => {
            match stream.take_error() {
                Ok(Some(e)) | Err(e) => {
                    let e = Value::new_error(&format!("{}", e));
                    get_or_create_reject(agent, client, e);
                }
                Ok(None) => {
                    let mut buf = Vec::new();
                    match stream.read_to_end(&mut buf) {
                        Ok(size) if size == 0 => {
                            get_or_create_resolve(agent, client, Value::Null, true);
                            return;
                        }
                        Ok(_) => {
                            // TODO: buffer type of some sort
                            let r = Value::new_array(agent);
                            for (i, v) in buf.iter().enumerate() {
                                r.set(&ObjectKey::from(i), Value::Number((*v).into()))
                                    .unwrap();
                            }
                            get_or_create_resolve(agent, client.clone(), r, false);
                        }
                        Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                            // TODO: buffer type of some sort
                            let r = Value::new_array(agent);
                            for (i, v) in buf.iter().enumerate() {
                                r.set(&ObjectKey::from(i), Value::Number((*v).into()))
                                    .unwrap();
                            }
                            get_or_create_resolve(agent, client.clone(), r, false);
                        }
                        Err(e) => {
                            let e = Value::new_error(&format!("{}", e));
                            get_or_create_reject(agent, client.clone(), e);
                        }
                    }
                    agent
                        .mio_map
                        .borrow_mut()
                        .insert(token, MioMapType::Net(Net::Client(stream, client)));
                }
            }
        }
    }
}

fn connect(agent: &Agent, _: &ExecutionContext, args: Vec<Value>) -> Result<Value, Value> {
    match args.get(0).unwrap_or(&Value::Null) {
        Value::String(addr) => {
            let addr = addr.parse()?;
            let stream = TcpStream::connect(&addr)?;
            let token = Token(agent.mio_map.borrow().len());
            agent
                .mio
                .register(&stream, token, Ready::readable(), PollOpt::edge())?;
            let client = Value::new_custom_object(agent.intrinsics.net_client_prototype.clone());
            client.set_slot("net client buffer", Value::new_list());
            client.set_slot("net client queue", Value::new_list());
            client.set_slot("net client token", Value::Number(token.0.into()));
            agent
                .mio_map
                .borrow_mut()
                .insert(token, MioMapType::Net(Net::Client(stream, client.clone())));
            Ok(client)
        }
        _ => Err(Value::new_error("address must be a string")),
    }
}

pub fn create(agent: &Agent) -> HashMap<String, Value> {
    let mut module = HashMap::new();
    module.insert(
        "connect".to_string(),
        Value::new_builtin_function(agent, connect),
    );

    module
}
