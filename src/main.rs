#![feature(integer_atomics)]
#![allow(clippy::float_cmp)]

#[macro_use]
extern crate gc_derive;
extern crate clap;
extern crate gc;

mod intrinsics;
mod module;
mod parser;
mod value;

use clap::{App, Arg};
use module::Agent;

fn main() {
    let matches = App::new("slither")
        .version("0.1")
        .arg(Arg::with_name("filename").required(true))
        .get_matches();

    let filename = matches.value_of("filename").unwrap();
    let referrer = std::env::current_dir().unwrap().join("slither");
    let referrer = referrer.to_str().unwrap();

    let agent = Agent::new();

    let c = agent.import(filename, referrer);

    println!("res {:?}", c);

    agent.run_jobs();

    std::process::exit(0);
}
