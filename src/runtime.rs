use crate::value::ObjectKey;
use crate::{Agent, Value};

macro_rules! intrinsics {
    ($V:ident) => {
        $V!(
            (ToString, to_string),
            (GetIterator, get_iterator),
            (GetAsyncIterator, get_async_iterator),
        );
    };
}

fn to_string(agent: &Agent, value: Value) -> Result<Value, Value> {
    if value.type_of() == "string" {
        Ok(value)
    } else {
        let ts = value.get(agent, ObjectKey::well_known_symbol("toString"))?;
        if ts.type_of() != "function" {
            Err(Value::new_error(
                agent,
                "value does not provide a :toString",
            ))
        } else {
            ts.call(agent, value, vec![])
        }
    }
}

fn get_iterator(agent: &Agent, value: Value) -> Result<Value, Value> {
    value.to_iterator(agent)
}

fn get_async_iterator(agent: &Agent, value: Value) -> Result<Value, Value> {
    value.to_async_iterator(agent)
}

macro_rules! declare_enum {
    ( $( ( $name:ident, $fn:ident ), )* ) => (
        #[derive(Debug, PartialEq, Clone, Copy)]
        #[repr(u8)]
        pub enum RuntimeFunction {
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
    pub fn get(id: u8) -> fn(&Agent, Value) -> Result<Value, Value> {
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

    pub fn name(&self) -> String {
        macro_rules! matcher {
            ( $( ( $name:ident, $fn:ident ), )* ) => (
                return match self {
                    $( RuntimeFunction::$name => stringify!($name).to_string(), )*
                };
            )
        }
        intrinsics!(matcher);
    }

    pub fn id(&self) -> u8 {
        *self as u8
    }
}
