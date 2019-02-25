#![allow(clippy::float_cmp)]
#![allow(clippy::cyclomatic_complexity)]
#![allow(clippy::option_option)]

#[macro_use]
extern crate gc_derive;
extern crate byteorder;
extern crate clap;
extern crate gc;
extern crate mio;

mod agent;
mod builtins;
mod intrinsics;
mod parser;
mod value;
mod vm;

use agent::Agent;
use value::Value;

pub fn run(specifier: &str, referrer: &str) -> Result<(), Value> {
    let agent = Agent::new();

    match agent.import(specifier, referrer) {
        Ok(()) => {
            agent.run_jobs();
            Ok(())
        }
        Err(e) => Err(e),
    }
}
