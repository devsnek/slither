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

macro_rules! custom_trace {
    ($this:ident, $body:expr) => {
        #[inline]
        unsafe fn trace(&self) {
            #[inline]
            unsafe fn mark<T: gc::Trace>(it: &T) {
                gc::Trace::trace(it);
            }
            let $this = self;
            $body
        }
        #[inline]
        unsafe fn root(&self) {
            #[inline]
            unsafe fn mark<T: gc::Trace>(it: &T) {
                gc::Trace::root(it);
            }
            let $this = self;
            $body
        }
        #[inline]
        unsafe fn unroot(&self) {
            #[inline]
            unsafe fn mark<T: gc::Trace>(it: &T) {
                gc::Trace::unroot(it);
            }
            let $this = self;
            $body
        }
        #[inline]
        fn finalize_glue(&self) {
            gc::Finalize::finalize(self);
            #[inline]
            fn mark<T: gc::Trace>(it: &T) {
                gc::Trace::finalize_glue(it);
            }
            let $this = self;
            $body
        }
    }
}

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
