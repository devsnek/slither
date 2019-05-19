use crate::value::ObjectKey;
use crate::{Agent, Value};

macro_rules! intrinsics {
    ($V:ident) => {
        $V!(
            (ToString, to_string),
            (GetIterator, get_iterator),
            (GetAsyncIterator, get_async_iterator),
            (ObjectKeys, object_keys),
            (ListLength, list_length),
            (ListPopFront, list_pop_front),
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

fn object_keys(agent: &Agent, accumulator: &mut Value) -> Result<(), Value> {
    let keys = accumulator.keys(agent)?;
    *accumulator = Value::new_list_from_iter(keys.iter().map(Value::from));
    Ok(())
}

fn list_length(_agent: &Agent, accumulator: &mut Value) -> Result<(), Value> {
    if let Value::List(list) = accumulator {
        let len = list.borrow().len() as f64;
        *accumulator = Value::from(len);
    } else {
        unreachable!();
    }
    Ok(())
}

fn list_pop_front(_agent: &Agent, accumulator: &mut Value) -> Result<(), Value> {
    if let Value::List(list) = accumulator {
        let item = list.borrow_mut().pop_front();
        *accumulator = match item {
            None => Value::Empty,
            Some(v) => v,
        };
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
