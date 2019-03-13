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
mod num_util;
mod parser;
mod value;
mod vm;

pub use agent::Agent;
