use crate::value::ObjectKey;
use crate::{Agent, Value};

macro_rules! intrinsics {
    ($V:ident) => {
        $V!(
            (ToString, to_string),
            (GetIterator, get_iterator),
            (GetAsyncIterator, get_async_iterator),
            (IteratorDone, iterator_done),
            (ObjectKeysLength, object_keys_length),
        );
    };
}

fn to_string(agent: &Agent, accumulator: &mut Value) -> Result<(), Value> {
    if accumulator.type_of() == "string" {
        Ok(())
    } else {
        let ts = accumulator.get(agent, ObjectKey::well_known_symbol("toString"))?;
        if ts.type_of() != "function" {
            Err(Value::new_error(
                agent,
                "value does not provide a :toString",
            ))
        } else {
            *accumulator = ts.call(agent, accumulator.clone(), vec![])?;
            Ok(())
        }
    }
}

fn get_iterator(agent: &Agent, accumulator: &mut Value) -> Result<(), Value> {
    *accumulator = accumulator.to_iterator(agent)?;
    Ok(())
}

fn get_async_iterator(agent: &Agent, accumulator: &mut Value) -> Result<(), Value> {
    *accumulator = accumulator.to_async_iterator(agent)?;
    Ok(())
}

fn object_keys_length(agent: &Agent, accumulator: &mut Value) -> Result<(), Value> {
    *accumulator = Value::from(accumulator.keys(agent)?.len() as f64);
    Ok(())
}

fn iterator_done(agent: &Agent, accumulator: &mut Value) -> Result<(), Value> {
    if let Value::Iterator(iter, ..) = accumulator {
        let r = iter.get(agent, ObjectKey::from("return"))?;
        if r != Value::Null {
            r.call(agent, *iter.clone(), vec![])?;
        }
    } else {
        unreachable!();
    }
    Ok(())
}

macro_rules! declare_enum {
    ( $( ( $name:ident, $fn:ident ), )* ) => (
        #[derive(Debug, PartialEq, Clone, Copy)]
        #[repr(u8)]
        pub(crate) enum RuntimeFunction {
            $( $name , )*
        }
    );
}

intrinsics!(declare_enum);

impl From<u8> for RuntimeFunction {
    fn from(n: u8) -> RuntimeFunction {
        unsafe { std::mem::transmute::<u8, RuntimeFunction>(n) }
    }
}

impl RuntimeFunction {
    pub(crate) fn get(id: u8) -> fn(&Agent, &mut Value) -> Result<(), Value> {
        macro_rules! matcher {
            ( $( ( $name:ident, $fn:ident ), )* ) => (
                let id: RuntimeFunction = id.into();
                return match id {
                    $( RuntimeFunction::$name => $fn, )*
                };
            )
        }
        intrinsics!(matcher);
    }

    pub(crate) fn name(self) -> String {
        macro_rules! matcher {
            ( $( ( $name:ident, $fn:ident ), )* ) => (
                return match self {
                    $( RuntimeFunction::$name => stringify!($name).to_string(), )*
                };
            )
        }
        intrinsics!(matcher);
    }

    pub(crate) fn id(self) -> u8 {
        self as u8
    }
}
