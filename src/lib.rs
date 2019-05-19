#![allow(clippy::cyclomatic_complexity)]
#![allow(clippy::option_option)]

#[macro_use]
extern crate gc_derive;

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
mod interpreter;
mod intrinsics;
mod linked_list;
mod module;
mod num_util;
mod parser;
mod runtime;
mod serde;
mod sort;
mod value;

pub trait IntoValue: Sized {
    fn into_value(&self, _: &agent::Agent) -> value::Value;
}

pub use agent::Agent;
pub use interpreter::{Context, Interpreter, Scope};
pub use parser::Parser;
pub use value::{ObjectKey, Value};

pub fn disassemble(code: &str) {
    let mut agent = Agent::new();

    let ast = match Parser::parse(code) {
        Ok(ast) => ast,
        Err(e) => panic!(format!("{:?}", e)),
    };
    let _idx = agent.assembler.assemble(&ast);

    interpreter::disassemble(&agent.assembler, 0, std::usize::MAX);
}
