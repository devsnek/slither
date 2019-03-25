use crate::module::Module;
use crate::num_util::{f64_band, f64_bnot, f64_bor, f64_bxor, f64_shl, f64_shr};
use crate::parser::FunctionKind;
use crate::value::{ObjectKey, ObjectKind};
use crate::{Agent, Value};
use byteorder::{LittleEndian, ReadBytesExt};
use gc::{Gc, GcCell};
use indexmap::IndexMap;
use std::ops::{Div, Mul, Rem, Sub};

#[allow(dead_code)]
pub enum AccumulatorUse {
    None,
    Read,
    Write,
    ReadWrite,
}

pub enum OpArg {
    U8,
    U32,
    F64,
    Boolean,
    String,
    Register,
    Position,
    FunctionInfo,
}

#[rustfmt::skip]
macro_rules! OPS {
    ($V:ident) => {
        $V!(
            (LoadNull, AccumulatorUse::Write),
            (LoadTrue, AccumulatorUse::Write),
            (LoadFalse, AccumulatorUse::Write),
            (LoadF64, AccumulatorUse::Write, OpArg::F64),
            (LoadString, AccumulatorUse::Write, OpArg::String),
            (LoadSymbol, AccumulatorUse::Write, OpArg::String),

            (BuildRegex, AccumulatorUse::Write, OpArg::String),
            (CreateEmptyArray, AccumulatorUse::Write),
            (StoreInArrayLiteral, AccumulatorUse::Read, OpArg::Register, OpArg::U32),
            (CreateEmptyTuple, AccumulatorUse::Write),
            (StoreInTuple, AccumulatorUse::Read, OpArg::Register),
            (CreateEmptyObject, AccumulatorUse::Write),
            (StoreInObjectLiteral, AccumulatorUse::Read, OpArg::Register, OpArg::Register),
            (NewFunction, AccumulatorUse::ReadWrite, OpArg::FunctionInfo),

            (LoadNamedProperty, AccumulatorUse::ReadWrite, OpArg::String),
            (LoadComputedProperty, AccumulatorUse::ReadWrite, OpArg::Register),

            (LexicalDeclaration, AccumulatorUse::None, OpArg::String, OpArg::Boolean),
            (LexicalInitialization, AccumulatorUse::ReadWrite, OpArg::String),
            (ResolveIdentifier, AccumulatorUse::Write, OpArg::String),
            (AssignIdentifier, AccumulatorUse::Read, OpArg::String),

            (GetThis, AccumulatorUse::Write),

            (Call, AccumulatorUse::ReadWrite, OpArg::Register, OpArg::Register, OpArg::Register, OpArg::U8),
            (TailCall, AccumulatorUse::ReadWrite, OpArg::Register, OpArg::Register, OpArg::Register, OpArg::U8),

            (Construct, AccumulatorUse::ReadWrite),
            (ConstructWithArgs, AccumulatorUse::ReadWRite, OpArg::Register, OpArg::Register, OpArg::U8),

            (EnterScope, AccumulatorUse::None),
            (ExitScope, AccumulatorUse::None),

            (Jump, AccumulatorUse::None,OpArg::Position),
            (JumpIfTrue, AccumulatorUse::Read,OpArg::Position),
            (JumpIfFalse, AccumulatorUse::Read,OpArg::Position),

            (PushTry, AccumulatorUse::None,OpArg::Position),
            (PopTry, AccumulatorUse::None),
            (ThrowDynamic, AccumulatorUse::None),
            (SetException, AccumulatorUse::Read),
            (GetException, AccumulatorUse::Write),
            (ClearException, AccumulatorUse::None),

            (Suspend, AccumulatorUse::Write),
            (Return, AccumulatorUse::Write),

            (GetIterator, AccumulatorUse::ReadWrite),
            (GetAsyncIterator, AccumulatorUse::ReadWrite),
            (IteratorNext, AccumulatorUse::ReadWrite, OpArg::Register),
            (AsyncIteratorNext, AccumulatorUse::ReadWrite, OpArg::Register),

            (LoadAccumulatorFromRegister, AccumulatorUse::Write, OpArg::Register),
            (StoreAccumulatorInRegister, AccumulatorUse::Read, OpArg::Register),

            (Add, AccumulatorUse::ReadWrite, OpArg::Register),
            (Sub, AccumulatorUse::ReadWrite, OpArg::Register),
            (Mul, AccumulatorUse::ReadWrite, OpArg::Register),
            (Div, AccumulatorUse::ReadWrite, OpArg::Register),
            (Mod, AccumulatorUse::ReadWrite, OpArg::Register),
            (Pow, AccumulatorUse::ReadWrite, OpArg::Register),
            (BitOR, AccumulatorUse::ReadWrite, OpArg::Register),
            (BitXOR, AccumulatorUse::ReadWrite, OpArg::Register),
            (BitAND, AccumulatorUse::ReadWrite, OpArg::Register),
            (ShiftLeft, AccumulatorUse::ReadWrite, OpArg::Register),
            (ShiftRight, AccumulatorUse::ReadWrite, OpArg::Register),
            (GreaterThan, AccumulatorUse::ReadWrite, OpArg::Register),
            (LessThan, AccumulatorUse::ReadWrite, OpArg::Register),
            (GreaterThanOrEqual, AccumulatorUse::ReadWrite, OpArg::Register),
            (LessThanOrEqual, AccumulatorUse::ReadWrite, OpArg::Register),
            (Eq, AccumulatorUse::ReadWrite, OpArg::Register),
            (Neq, AccumulatorUse::ReadWrite, OpArg::Register),
            (LNOT, AccumulatorUse::ReadWrite),
            (BitNOT, AccumulatorUse::ReadWrite),
            (Typeof, AccumulatorUse::ReadWrite),
            (Void, AccumulatorUse::ReadWrite),
            (UnSub, AccumulatorUse::ReadWrite),
        );
    };
}

macro_rules! define_op_enum {
    ( $( ( $name:ident, $acu:expr $( , $arg:expr )* ), )* ) => (

        #[derive(Debug, PartialEq)]
        #[repr(u8)]
        pub enum Op {
            $( $name , )*
        }

        impl From<u8> for Op {
            fn from(n: u8) -> Op {
                unsafe { std::mem::transmute::<u8, Op>(n) }
            }
        }

    );
}

OPS!(define_op_enum);

pub const REGISTER_COUNT: usize = 16;

mod assembler;
mod disassembler;

pub use assembler::{Assembler, AssemblerFunctionInfo};
pub use disassembler::disassemble;

#[derive(Trace, Finalize, Debug)]
struct Binding {
    mutable: bool,
    value: Option<Value>,
    module: Option<Gc<GcCell<Module>>>,
}

#[derive(Trace, Finalize, Debug)]
pub struct Scope {
    parent: Option<Gc<GcCell<Scope>>>,
    bindings: IndexMap<String, Binding>,
}

impl Scope {
    pub fn new(parent: Option<Gc<GcCell<Scope>>>) -> Gc<GcCell<Scope>> {
        Gc::new(GcCell::new(Scope {
            parent,
            bindings: IndexMap::new(),
        }))
    }

    pub fn create(&mut self, name: &str, mutable: bool) {
        debug_assert!(!self.bindings.contains_key(name));
        self.bindings.insert(
            name.to_string(),
            Binding {
                mutable,
                value: None,
                module: None,
            },
        );
    }

    pub fn create_import(&mut self, name: &str, module: Gc<GcCell<Module>>) {
        self.bindings.insert(
            name.to_string(),
            Binding {
                mutable: false,
                value: None,
                module: Some(module),
            },
        );
    }

    pub fn initialize(&mut self, name: &str, value: Value) {
        match self.bindings.get_mut(name) {
            Some(b) => {
                debug_assert!(!b.value.is_some());
                b.value = Some(value);
            }
            _ => panic!(format!(
                "tried to initialize binding '{}' that doesn't exist!",
                name
            )),
        }
    }

    fn get(&self, agent: &Agent, name: &str) -> Result<Value, Value> {
        match self.bindings.get(name) {
            Some(Binding { value: Some(v), .. }) => Ok(v.clone()),
            Some(Binding {
                module: Some(m), ..
            }) => m.borrow().context.borrow().scope.borrow().get(agent, name),
            Some(..) => Err(Value::new_error(agent, "Reference error")),
            None => match self.parent {
                Some(ref parent) => parent.borrow().get(agent, name),
                None => Err(Value::new_error(agent, "Reference error")),
            },
        }
    }

    fn set(&mut self, agent: &Agent, name: &str, value: Value) -> Result<(), Value> {
        match self.bindings.get_mut(name) {
            Some(b) => {
                if b.value.is_none() {
                    Err(Value::new_error(agent, "Reference error"))
                } else if !b.mutable {
                    Err(Value::new_error(agent, "cannot reassign constant binding"))
                } else {
                    b.value = Some(value);
                    Ok(())
                }
            }
            None => match self.parent {
                Some(ref parent) => parent.borrow_mut().set(agent, name, value),
                None => Err(Value::new_error(agent, "Reference error")),
            },
        }
    }
}

#[derive(Trace, Finalize, Debug)]
pub struct Context {
    pub scope: Gc<GcCell<Scope>>,
    pub interpreter: Option<Interpreter>,
    pub function: Option<Value>,
    pub this: Option<Value>,
}

/*
 */

impl Context {
    pub fn new(scope: Gc<GcCell<Scope>>) -> Gc<GcCell<Context>> {
        Gc::new(GcCell::new(Context {
            scope,
            interpreter: None,
            function: None,
            this: None,
        }))
    }

    pub fn get_this(&self, agent: &Agent) -> Result<Value, Value> {
        match self.this {
            Some(ref v) => Ok(v.clone()),
            None => Err(Value::new_error(agent, "invalid this")),
        }
    }
}

#[derive(Debug, Trace, Finalize)]
pub struct SuspendValue(pub Value);

#[derive(Debug, Trace, Finalize)]
struct Registers {
    last: Option<Box<Registers>>,
    registers: [Value; REGISTER_COUNT],
}

impl Registers {
    fn new(last: Option<Box<Registers>>) -> Registers {
        Registers {
            last,
            registers: [
                Value::Empty,
                Value::Empty,
                Value::Empty,
                Value::Empty,
                Value::Empty,
                Value::Empty,
                Value::Empty,
                Value::Empty,
                Value::Empty,
                Value::Empty,
                Value::Empty,
                Value::Empty,
                Value::Empty,
                Value::Empty,
                Value::Empty,
                Value::Empty,
            ],
        }
    }
}

impl std::ops::Index<usize> for Registers {
    type Output = Value;

    #[inline]
    fn index(&self, index: usize) -> &Value {
        &self.registers[index]
    }
}

impl std::ops::IndexMut<usize> for Registers {
    #[inline]
    fn index_mut(&mut self, index: usize) -> &mut Value {
        &mut self.registers[index]
    }
}

#[derive(Debug, Trace, Finalize)]
pub struct Interpreter {
    pub accumulator: Value,
    pub exception: Option<Value>,
    pc: usize,
    try_stack: Vec<usize>,
    context: Vec<Gc<GcCell<Context>>>,
    positions: Vec<usize>,
    registers: Registers,
}

impl Interpreter {
    pub fn new(pc: usize, ctx: Gc<GcCell<Context>>) -> Interpreter {
        Interpreter {
            accumulator: Value::Empty,
            exception: None,
            pc,
            try_stack: Vec::new(),
            context: vec![ctx],
            positions: Vec::new(),
            registers: Registers::new(None),
        }
    }

    pub fn run(&mut self, agent: &Agent) -> Result<Result<Value, Value>, SuspendValue> {
        macro_rules! push_context {
            ($ctx:expr) => {
                self.context.push($ctx);
                unsafe {
                    std::ptr::write(
                        &mut self.registers,
                        Registers::new(Some(Box::new(std::ptr::read(&self.registers)))),
                    );
                }
            };
        }

        macro_rules! pop_context {
            () => {
                self.context.pop().unwrap();
                self.registers = *self.registers.last.take().unwrap();
            };
        }

        macro_rules! read_u8 {
            () => {{
                let n = agent.assembler.code[self.pc];
                self.pc += 1;
                n
            }};
        }

        macro_rules! read_u32 {
            () => {{
                let n = (&agent.assembler.code[self.pc..])
                    .read_u32::<LittleEndian>()
                    .unwrap();
                self.pc += 4;
                n
            }};
        }

        macro_rules! read_f64 {
            () => {{
                let n = (&agent.assembler.code[self.pc..])
                    .read_f64::<LittleEndian>()
                    .unwrap();
                self.pc += 8;
                n
            }};
        }

        macro_rules! num_binop_num {
            ($fn:expr) => {{
                let lhsid = read_u32!() as usize;
                match self.registers[lhsid] {
                    Value::Number(ln) => match self.accumulator {
                        Value::Number(rn) => {
                            self.accumulator = Value::Number($fn(ln, rn));
                        }
                        _ => handle!(Err(Value::new_error(agent, "rhs must be a number"))),
                    },
                    _ => handle!(Err(Value::new_error(agent, "lhs must be a number"))),
                }
            }};
        }

        macro_rules! num_binop_bool {
            ($fn:expr) => {{
                let lhsid = read_u32!() as usize;
                match self.registers[lhsid] {
                    Value::Number(ln) => match self.accumulator {
                        Value::Number(rn) => {
                            self.accumulator = if $fn(&ln, &rn) {
                                Value::True
                            } else {
                                Value::False
                            };
                        }
                        _ => handle!(Err(Value::new_error(agent, "rhs must be a number"))),
                    },
                    _ => handle!(Err(Value::new_error(agent, "lhs must be a number"))),
                }
            }};
        }

        if self.exception.is_some() {
            match self.try_stack.pop() {
                Some(position) => {
                    self.pc = position;
                }
                None => {
                    return Ok(Err(self.exception.take().unwrap()));
                }
            }
        }

        'main: loop {
            macro_rules! handle {
                ($ex:expr) => {
                    match $ex {
                        Ok(v) => v,
                        Err(e) => {
                            self.exception = Some(e);
                            match self.try_stack.pop() {
                                Some(position) => {
                                    self.pc = position;
                                    continue 'main;
                                }
                                None => {
                                    break 'main;
                                }
                            }
                        }
                    }
                };
            }

            if self.pc >= agent.assembler.code.len() {
                break;
            }
            let op = agent.assembler.code[self.pc].into();
            self.pc += 1;

            match op {
                Op::LoadNull => {
                    self.accumulator = Value::Null;
                }
                Op::LoadTrue => {
                    self.accumulator = Value::True;
                }
                Op::LoadFalse => {
                    self.accumulator = Value::False;
                }
                Op::LoadF64 => {
                    let n = read_f64!();
                    self.accumulator = Value::Number(n);
                }
                Op::LoadString => {
                    let sid = read_u32!() as usize;
                    let s = &agent.assembler.string_table[sid];
                    self.accumulator = Value::String(s.to_string());
                }
                Op::LoadSymbol => {
                    let nid = read_u32!() as usize;
                    let name = agent.assembler.string_table[nid].to_string();
                    let sym = agent.well_known_symbol(name.as_str());
                    self.accumulator = sym;
                }
                Op::BuildRegex => {
                    let pid = read_u32!() as usize;
                    let pattern = &agent.assembler.string_table[pid];
                    let r = handle!(Value::new_regex_object(agent, pattern));
                    self.accumulator = r;
                }
                Op::LoadNamedProperty => {
                    let sid = read_u32!() as usize;
                    let key = &agent.assembler.string_table[sid];
                    let key = ObjectKey::from(key.to_string());
                    self.accumulator = handle!(self.accumulator.get(agent, key));
                }
                Op::LoadComputedProperty => {
                    let objid = read_u32!() as usize;
                    let prop = handle!(self.accumulator.to_object_key(agent));
                    self.accumulator = handle!(self.registers[objid].get(agent, prop));
                }
                Op::LoadAccumulatorFromRegister => {
                    let rid = read_u32!() as usize;
                    self.accumulator = self.registers[rid].clone();
                }
                Op::StoreAccumulatorInRegister => {
                    let rid = read_u32!() as usize;
                    self.registers[rid] = self.accumulator.clone();
                }
                Op::SetException => {
                    self.exception = Some(std::mem::replace(&mut self.accumulator, Value::Empty));
                }
                Op::GetException => {
                    self.accumulator = self.exception.take().unwrap();
                }
                Op::ClearException => {
                    self.exception = None;
                }
                Op::EnterScope => {
                    let mut context = self.context.last().unwrap().borrow_mut();
                    let new = Scope::new(Some(context.scope.clone()));
                    std::mem::replace(&mut context.scope, new);
                }
                Op::ExitScope => {
                    let mut context = self.context.last().unwrap().borrow_mut();
                    let old = context.scope.borrow().parent.clone().unwrap();
                    std::mem::replace(&mut context.scope, old);
                }
                Op::LexicalDeclaration => {
                    let sid = read_u32!() as usize;
                    let mutable = read_u8!() == 1;
                    let name = &agent.assembler.string_table[sid];
                    self.context
                        .last()
                        .unwrap()
                        .borrow()
                        .scope
                        .borrow_mut()
                        .create(name, mutable);
                }
                Op::LexicalInitialization => {
                    let sid = read_u32!() as usize;
                    let name = &agent.assembler.string_table[sid];
                    let value = std::mem::replace(&mut self.accumulator, Value::Null);
                    self.context
                        .last()
                        .unwrap()
                        .borrow()
                        .scope
                        .borrow_mut()
                        .initialize(name, value);
                }
                Op::ResolveIdentifier => {
                    let sid = read_u32!() as usize;
                    let name = &agent.assembler.string_table[sid];
                    self.accumulator = handle!(self
                        .context
                        .last()
                        .unwrap()
                        .borrow()
                        .scope
                        .borrow()
                        .get(agent, name));
                }
                Op::AssignIdentifier => {
                    let sid = read_u32!() as usize;
                    let name = &agent.assembler.string_table[sid];
                    handle!(self
                        .context
                        .last()
                        .unwrap()
                        .borrow()
                        .scope
                        .borrow_mut()
                        .set(agent, name, self.accumulator.clone()));
                }
                Op::GetThis => {
                    self.accumulator =
                        handle!(self.context.last().unwrap().borrow().get_this(agent));
                }
                Op::Suspend => {
                    return Err(SuspendValue(std::mem::replace(
                        &mut self.accumulator,
                        Value::Null,
                    )));
                }
                Op::Construct => {
                    self.accumulator = handle!(self.accumulator.construct(
                        agent,
                        vec![],
                        self.accumulator.clone()
                    ));
                }
                Op::ConstructWithArgs => {
                    let cid = read_u32!() as usize; // callee
                    let sargid = read_u32!() as usize; // first argument register
                    let argc = read_u8!() as usize;

                    let mut args = Vec::with_capacity(argc);
                    for i in 0..argc {
                        args.push(self.registers[sargid + i].clone());
                    }
                    let callee = std::mem::replace(&mut self.registers[cid], Value::Empty);
                    self.accumulator = handle!(callee.construct(agent, args, callee.clone()));
                }
                Op::Call | Op::TailCall => {
                    let rid = read_u32!() as usize; // receiver
                    let cid = read_u32!() as usize; // callee
                    let sargid = read_u32!() as usize; // first argument register
                    let argc = read_u8!() as usize; // number of arguments

                    let callee = std::mem::replace(&mut self.registers[cid], Value::Empty);

                    macro_rules! slow_call {
                        () => {
                            let mut args = Vec::with_capacity(argc);
                            for i in 0..argc {
                                args.push(self.registers[sargid + i].clone());
                            }
                            let receiver =
                                std::mem::replace(&mut self.registers[rid], Value::Empty);
                            self.accumulator = handle!(callee.call(agent, receiver, args));
                        };
                    }

                    match callee {
                        Value::Object(ref o) => match &o.kind {
                            ObjectKind::BytecodeFunction { kind, .. }
                                if *kind != FunctionKind::Normal =>
                            {
                                slow_call!();
                            }
                            ObjectKind::BuiltinFunction(..) => {
                                slow_call!();
                            }
                            ObjectKind::BytecodeFunction {
                                position,
                                parameters,
                                scope,
                                ..
                            } => {
                                let scope = Scope::new(Some(scope.clone()));
                                let ctx = Context::new(scope.clone());

                                for (i, param) in parameters.iter().enumerate() {
                                    scope.borrow_mut().create(param, false);
                                    let value = if i > argc {
                                        Value::Null
                                    } else {
                                        self.registers[sargid + i].clone()
                                    };
                                    scope.borrow_mut().initialize(param, value);
                                }

                                if op == Op::TailCall {
                                    pop_context!();
                                } else {
                                    self.positions.push(self.pc);
                                }
                                push_context!(ctx);
                                self.pc = *position;
                            }
                            _ => handle!(Err(Value::new_error(agent, "value is not a function"))),
                        },
                        _ => handle!(Err(Value::new_error(agent, "value is not a function"))),
                    }
                }
                Op::Return => match self.positions.pop() {
                    Some(p) => {
                        pop_context!();
                        self.pc = p;
                    }
                    None => {
                        break 'main;
                    }
                },
                Op::GetIterator | Op::GetAsyncIterator => {
                    let sym = handle!(if op == Op::GetAsyncIterator {
                        agent.well_known_symbol("asyncIterator")
                    } else {
                        agent.well_known_symbol("iterator")
                    }
                    .to_object_key(agent));
                    let iterator = handle!(self.accumulator.get(agent, sym));
                    let iterator = handle!(iterator.call(agent, self.accumulator.clone(), vec![]));
                    let next = handle!(iterator.get(agent, ObjectKey::from("next")));
                    let iterator = Value::Iterator(Box::new(iterator), Box::new(next));
                    self.accumulator = iterator;
                }
                Op::IteratorNext => {
                    let iid = read_u32!() as usize;
                    if let Value::Iterator(iterator, next) = &self.registers[iid] {
                        self.accumulator = handle!(next.call(agent, (**iterator).clone(), vec![]));
                    } else {
                        unreachable!()
                    }
                }
                Op::AsyncIteratorNext => {
                    let iid = read_u32!() as usize;
                    if let Value::Iterator(iterator, next) = &self.registers[iid] {
                        let promise = handle!(next.call(agent, (**iterator).clone(), vec![]));
                        return Err(SuspendValue(promise));
                    } else {
                        unreachable!()
                    }
                }
                Op::Jump => {
                    let position = read_u32!() as usize;
                    self.pc = position;
                }
                Op::JumpIfTrue => {
                    let position = read_u32!() as usize;
                    if self.accumulator.to_bool() {
                        self.pc = position;
                    }
                }
                Op::JumpIfFalse => {
                    let position = read_u32!() as usize;
                    if !self.accumulator.to_bool() {
                        self.pc = position;
                    }
                }
                Op::PushTry => {
                    let pos = read_u32!() as usize;
                    self.try_stack.push(pos);
                }
                Op::PopTry => {
                    self.try_stack.pop();
                }
                Op::ThrowDynamic => {
                    debug_assert!(self.exception.is_some());
                    match self.try_stack.pop() {
                        Some(position) => {
                            self.pc = position;
                            continue 'main;
                        }
                        None => {
                            break 'main;
                        }
                    }
                }
                Op::CreateEmptyArray => {
                    self.accumulator = Value::new_array(agent);
                }
                Op::StoreInArrayLiteral => {
                    let aid = read_u32!() as usize;
                    let idx = read_u32!();
                    let key = ObjectKey::from(idx);
                    handle!(self.registers[aid].set(agent, key, self.accumulator.clone()));
                }
                Op::CreateEmptyTuple => {
                    self.accumulator = Value::new_tuple();
                }
                Op::StoreInTuple => {
                    let tid = read_u32!() as usize;
                    if let Value::Tuple(items) = &mut self.registers[tid] {
                        items.push(std::mem::replace(&mut self.accumulator, Value::Empty));
                    } else {
                        unreachable!();
                    }
                }
                Op::CreateEmptyObject => {
                    self.accumulator = Value::new_object(agent.intrinsics.object_prototype.clone());
                }
                Op::StoreInObjectLiteral => {
                    let oid = read_u32!() as usize;
                    let kid = read_u32!() as usize;
                    let key = handle!(self.registers[kid].to_object_key(agent));
                    handle!(self.registers[oid].set(agent, key, self.accumulator.clone()));
                }
                Op::NewFunction => {
                    let id = read_u32!() as usize;
                    let info = &agent.assembler.function_info[id];
                    let scope = Scope::new(match self.context.last() {
                        Some(c) => Some(c.borrow().scope.clone()),
                        None => None,
                    });
                    self.accumulator = Value::new_bytecode_function(agent, info, scope);
                }
                Op::Add => {
                    let lhsid = read_u32!() as usize;
                    match self.registers[lhsid] {
                        Value::Number(ln) => match self.accumulator {
                            Value::Number(rn) => {
                                self.accumulator = Value::Number(ln + rn);
                            }
                            _ => handle!(Err(Value::new_error(agent, "rhs must be a number"))),
                        },
                        Value::String(ref ls) => match self.accumulator {
                            Value::String(ref rs) => {
                                self.accumulator = Value::String(format!("{}{}", ls, rs));
                            }
                            _ => handle!(Err(Value::new_error(agent, "rhs must be a string"))),
                        },
                        _ => handle!(Err(Value::new_error(
                            agent,
                            "lhs must be a number or string"
                        ))),
                    }
                }
                Op::Sub => num_binop_num!(f64::sub),
                Op::Mul => num_binop_num!(f64::mul),
                Op::Div => num_binop_num!(f64::div),
                Op::Mod => num_binop_num!(f64::rem),
                Op::Pow => num_binop_num!(f64::powf),
                Op::BitOR => num_binop_num!(f64_bor),
                Op::BitXOR => num_binop_num!(f64_bxor),
                Op::BitAND => num_binop_num!(f64_band),
                Op::ShiftLeft => num_binop_num!(f64_shl),
                Op::ShiftRight => num_binop_num!(f64_shr),
                Op::GreaterThan => num_binop_bool!(f64::gt),
                Op::LessThan => num_binop_bool!(f64::lt),
                Op::GreaterThanOrEqual => num_binop_bool!(f64::ge),
                Op::LessThanOrEqual => num_binop_bool!(f64::le),
                Op::Eq => {
                    let lhsid = read_u32!() as usize;
                    self.accumulator = if self.registers[lhsid] == self.accumulator {
                        Value::True
                    } else {
                        Value::False
                    }
                }
                Op::Neq => {
                    let lhsid = read_u32!() as usize;
                    self.accumulator = if self.registers[lhsid] != self.accumulator {
                        Value::True
                    } else {
                        Value::False
                    }
                }
                Op::LNOT => {
                    self.accumulator = if self.accumulator.to_bool() {
                        Value::False
                    } else {
                        Value::True
                    }
                }
                Op::BitNOT => match self.accumulator {
                    Value::Number(n) => {
                        self.accumulator = Value::Number(f64_bnot(n));
                    }
                    _ => handle!(Err(Value::new_error(agent, "operand must be a number"))),
                },
                Op::Typeof => {
                    self.accumulator = Value::String(self.accumulator.type_of().to_string());
                }
                Op::Void => {
                    self.accumulator = Value::Null;
                }
                Op::UnSub => match self.accumulator {
                    Value::Number(n) => {
                        self.accumulator = Value::Number(-n);
                    }
                    _ => handle!(Err(Value::new_error(agent, "operand must be a number"))),
                },
            }
        }

        Ok(match self.exception.take() {
            Some(e) => Err(e),
            None => Ok(self.accumulator.clone()),
        })
    }
}
