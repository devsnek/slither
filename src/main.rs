#[macro_use]
extern crate gc_derive;
extern crate clap;
extern crate gc;

mod intrinsics;
mod module;
mod parser;
mod value;

use clap::{App, Arg};
use module::{Agent, Module};

fn main() {
    let matches = App::new("slither")
        .version("0.1")
        .arg(Arg::with_name("filename").required(true))
        .get_matches();

    let filename = matches.value_of("filename").unwrap();

    let mut agent = Agent::new();

    let module = Module::new(&mut agent, filename);

    println!("res {:?}", module.unwrap().evaluate());
}
