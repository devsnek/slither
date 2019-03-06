#![allow(clippy::float_cmp)]
#![allow(clippy::cyclomatic_complexity)]
#![allow(clippy::too_many_arguments)]
#![allow(clippy::option_option)]

#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate gc_derive;
extern crate byteorder;
extern crate clap;
extern crate gc;
extern crate mio;
extern crate num_cpus;
extern crate regex;
extern crate rust_decimal;
extern crate threadpool;

mod agent;
mod builtins;
mod intrinsics;
mod linked_list;
mod parser;
mod value;
mod vm;

use agent::Agent;
use value::Value;

pub fn run(specifier: &str, referrer: &str) -> Result<(), Value> {
    let mut agent = Agent::new();

    match agent.import(specifier, referrer) {
        Ok(()) => {
            agent.run_jobs();
            Ok(())
        }
        Err(e) => Err(e),
    }
}
