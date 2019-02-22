#![allow(clippy::float_cmp)]

#[macro_use]
extern crate gc_derive;
extern crate byteorder;
extern crate clap;
extern crate gc;

mod agent;
mod builtins;
mod intrinsics;
mod parser;
mod value;
mod vm;

use agent::Agent;
use clap::{App, Arg};

fn main() {
    let matches = App::new("slither")
        .version("0.1")
        .arg(Arg::with_name("filename").required(true))
        .get_matches();

    let filename = matches.value_of("filename").unwrap();
    let referrer = std::env::current_dir().unwrap().join("slither");
    let referrer = referrer.to_str().unwrap();

    let agent = Agent::new();

    match agent.import(filename, referrer) {
        Ok(()) => agent.run_jobs(),
        Err(e) => {
            eprintln!("Uncaught Exception: {}", e);
            std::process::exit(1);
        }
    }
}
