use crate::agent::{Agent, Module};
use crate::parser::FunctionKind;
use crate::value::{ObjectKey, ObjectKind, Value};
use crate::vm::Op;
use byteorder::{LittleEndian, ReadBytesExt};
use gc::{Gc, GcCell};
use std::collections::HashMap;
use std::ops::{Div, Mul, Rem, Sub};

#[derive(Debug, Trace, Finalize, PartialEq)]
struct Binding {
    value: Option<Value>,
    mutable: bool,
    exported: bool,
    module: Option<Module>,
}

#[derive(Trace, Finalize, Debug, PartialEq)]
pub struct LexicalEnvironment {
    pub this: Option<Value>,
    bindings: HashMap<String, Binding>,
    parent: Option<Gc<GcCell<LexicalEnvironment>>>,
}

impl LexicalEnvironment {
    pub fn new(parent: Option<Gc<GcCell<LexicalEnvironment>>>) -> Gc<GcCell<LexicalEnvironment>> {
        Gc::new(GcCell::new(LexicalEnvironment {
            parent,
            this: None,
            bindings: HashMap::new(),
        }))
    }

    pub fn get_this(&self, agent: &Agent) -> Result<Value, Value> {
        match self.this {
            Some(ref t) => Ok(t.clone()),
            None => match &self.parent {
                None => Err(Value::new_error(agent, "invalid this")),
                Some(p) => p.borrow().get_this(agent),
            },
        }
    }

    fn create_binding(
        &mut self,
        agent: &Agent,
        name: &str,
        mutable: bool,
        exported: bool,
        module: Option<Module>,
    ) -> Result<(), Value> {
        match self.bindings.get(name) {
            Some(binding) if !binding.exported => Err(Value::new_error(
                agent,
                &format!("binding {} already declared", name),
            )),
            _ => {
                self.bindings.insert(
                    name.to_string(),
                    Binding {
                        value: None,
                        mutable,
                        exported,
                        module,
                    },
                );
                Ok(())
            }
        }
    }

    pub fn create(&mut self, agent: &Agent, name: &str, mutable: bool) -> Result<(), Value> {
        self.create_binding(agent, name, mutable, false, None)
    }

    pub fn create_export(&mut self, agent: &Agent, name: &str, mutable: bool) -> Result<(), Value> {
        self.create_binding(agent, name, mutable, true, None)
    }

    pub fn create_import(
        &mut self,
        agent: &Agent,
        name: &str,
        module: Module,
    ) -> Result<(), Value> {
        self.create_binding(agent, name, false, false, Some(module))
    }

    pub fn initialize(&mut self, name: &str, value: Value) {
        match self.bindings.get_mut(name) {
            Some(b) => {
                if b.value.is_some() {
                    panic!(format!("already initialized {}", name));
                } else {
                    b.value = Some(value);
                }
            }
            _ => panic!(format!(
                "tried to initialize binding '{}' that doesn't exist!",
                name
            )),
        }
    }

    pub fn set(&mut self, agent: &Agent, name: &str, value: Value) -> Result<(), Value> {
        match self.bindings.get_mut(name) {
            Some(b) => {
                if b.module.is_some() {
                    Err(Value::new_error(agent, "cannot reassign constant binding"))
                } else if b.value.is_none() {
                    Err(Value::new_error(
                        agent,
                        &format!("reference error: {}", name),
                    ))
                } else if !b.mutable {
                    Err(Value::new_error(agent, "cannot reassign constant binding"))
                } else {
                    b.value = Some(value);
                    Ok(())
                }
            }
            _ => match &self.parent {
                Some(p) => p.borrow_mut().set(agent, name, value),
                _ => Err(Value::new_error(
                    agent,
                    &format!("reference error: {}", name),
                )),
            },
        }
    }

    pub fn get(&self, agent: &Agent, name: &str) -> Result<Value, Value> {
        match self.bindings.get(name) {
            Some(Binding {
                module: Some(m), ..
            }) => {
                let module = m.borrow();
                let ctx = module.context.borrow();
                let env = ctx.environment.borrow();
                env.get(agent, name)
            }
            Some(Binding { value: Some(v), .. }) => Ok((*v).clone()),
            Some(Binding { value: None, .. }) => Err(Value::new_error(
                agent,
                &format!("reference error: {}", name),
            )),
            _ => match &self.parent {
                Some(p) => p.borrow().get(agent, name),
                None => Err(Value::new_error(
                    agent,
                    &format!("reference error: {}", name),
                )),
            },
        }
    }
}

#[derive(Trace, Finalize, Debug)]
pub struct ExecutionContext {
    pub function: Option<Value>,
    pub environment: Gc<GcCell<LexicalEnvironment>>,
    pub evaluator: Option<Evaluator>,
}

impl ExecutionContext {
    pub fn new(environment: Gc<GcCell<LexicalEnvironment>>) -> Gc<GcCell<ExecutionContext>> {
        Gc::new(GcCell::new(ExecutionContext {
            function: None,
            environment,
            evaluator: None,
        }))
    }
}

#[derive(Debug, Trace, Finalize)]
pub struct LoopPosition {
    r#break: usize,
    r#continue: usize,
}

#[derive(Debug, Trace, Finalize)]
pub struct Evaluator {
    pc: usize,
    pub stack: Vec<Value>,
    pub scope: Vec<Gc<GcCell<ExecutionContext>>>,
    pub positions: Vec<usize>,
    try_stack: Vec<usize>,
    loop_stack: Vec<LoopPosition>,
    pub exception: Option<Value>,
}

#[derive(Debug, Trace, Finalize)]
pub struct SuspendValue(pub Value);

impl Evaluator {
    pub fn new(pos: (usize, usize)) -> Evaluator {
        Evaluator {
            pc: pos.0,
            stack: Vec::new(),
            scope: Vec::new(),
            positions: Vec::new(),
            try_stack: vec![pos.1],
            loop_stack: Vec::new(),
            exception: None,
        }
    }

    pub fn run(&mut self, agent: &Agent) -> Result<Result<Value, Value>, SuspendValue> {
        evaluate_at(
            agent,
            &mut self.pc,
            &mut self.stack,
            &mut self.scope,
            &mut self.positions,
            &mut self.try_stack,
            &mut self.loop_stack,
            &mut self.exception,
        )
    }
}

fn evaluate_at(
    agent: &Agent,
    pc: &mut usize,
    stack: &mut Vec<Value>,
    scope: &mut Vec<Gc<GcCell<ExecutionContext>>>,
    positions: &mut Vec<usize>,
    try_stack: &mut Vec<usize>,
    loop_stack: &mut Vec<LoopPosition>,
    exception: &mut Option<Value>,
) -> Result<Result<Value, Value>, SuspendValue> {
    let get_u8 = |pc: &mut usize| {
        let v = agent.code[*pc];
        *pc += 1;
        v
    };

    let get_bool = |pc: &mut usize| get_u8(pc) == 1;

    let get_i32 = |pc: &mut usize| {
        let n = (&agent.code[*pc..]).read_i32::<LittleEndian>().unwrap();
        *pc += 4;
        n
    };

    let get_f64 = |pc: &mut usize| {
        let n = (&agent.code[*pc..]).read_f64::<LittleEndian>().unwrap();
        *pc += 8;
        n
    };

    let get_value = |stack: &mut Vec<Value>| {
        let value = stack.pop().unwrap();
        match value {
            Value::EnvironmentReference(env, k) => env.borrow().get(agent, k.as_str()),
            Value::ValueReference(v, p) => v.get(agent, &p),
            _ => Ok(value),
        }
    };

    let get_value_no_consume = |stack: &Vec<Value>| {
        let value = stack.last().unwrap();
        match value {
            Value::EnvironmentReference(env, k) => env.borrow().get(agent, k.as_str()),
            Value::ValueReference(v, p) => v.get(agent, &p),
            _ => Ok(value.clone()),
        }
    };

    macro_rules! num_binop_num {
        ($op:expr) => {{
            let right = handle!(get_value(stack));
            let left = handle!(get_value(stack));
            if let Value::Number(lnum) = left {
                if let Value::Number(rnum) = right {
                    stack.push(Value::Number($op(lnum, rnum)));
                } else {
                    handle!(Err(Value::new_error(agent, "rval must be a number")))
                }
            } else {
                handle!(Err(Value::new_error(agent, "lval must be a umber")))
            }
        }};
    }

    macro_rules! num_binop_bool {
        ($op:expr) => {{
            let right = handle!(get_value(stack));
            let left = handle!(get_value(stack));
            if let Value::Number(lnum) = left {
                if let Value::Number(rnum) = right {
                    stack.push(if $op(&lnum, &rnum) {
                        Value::True
                    } else {
                        Value::False
                    });
                } else {
                    handle!(Err(Value::new_error(agent, "rval must be a number")))
                }
            } else {
                handle!(Err(Value::new_error(agent, "lval must be a umber")))
            }
        }};
    }

    if exception.is_some() {
        *pc = try_stack.pop().expect("try_stack context missing");
    }

    'main: loop {
        macro_rules! handle {
            ($ex:expr) => {
                match $ex {
                    Ok(v) => v,
                    Err(e) => {
                        let position = try_stack.pop().expect("try_stack context missing");
                        *pc = position;
                        *exception = Some(e);
                        continue 'main;
                    }
                }
            };
        }

        if *pc >= agent.code.len() {
            break;
        }
        let op = agent.code[*pc].into();
        *pc += 1;
        match op {
            Op::End => {
                break;
            }
            Op::PushScope => {
                // println!("PushScope");
                let mut ctx = scope.last().unwrap().borrow_mut();
                let new = LexicalEnvironment::new(Some(ctx.environment.clone()));
                std::mem::replace(&mut ctx.environment, new);
            }
            Op::PopScope => {
                // println!("PopScope");
                let mut ctx = scope.last().unwrap().borrow_mut();
                let old = ctx.environment.borrow().parent.clone().unwrap();
                std::mem::replace(&mut ctx.environment, old);
            }
            Op::PushNull => stack.push(Value::Null),
            Op::PushTrue => stack.push(Value::True),
            Op::PushFalse => stack.push(Value::False),
            Op::NewNumber => {
                let value = get_f64(pc);
                stack.push(Value::Number(value));
            }
            Op::NewString => {
                let id = get_i32(pc) as usize;
                let str = &agent.string_table[id];
                stack.push(Value::String(str.clone()));
            }
            Op::NewSymbol => {
                let id = get_i32(pc) as usize;
                let name = &agent.string_table[id];
                let sym = agent.well_known_symbol(name);
                stack.push(sym);
            }
            Op::NewRegex => {
                let id = get_i32(pc) as usize;
                let str = &agent.string_table[id];
                let reg = handle!(Value::new_regex_object(agent, str));
                stack.push(reg);
            }
            Op::NewFunction | Op::NewFunctionWithName => {
                let name;
                if op == Op::NewFunctionWithName {
                    let id = get_i32(pc) as usize;
                    name = &agent.string_table[id];
                } else {
                    #[allow(clippy::invalid_ref)]
                    unsafe {
                        name = std::mem::uninitialized();
                    }
                }
                let argc = get_u8(pc);
                let inherits_this = get_bool(pc);
                let kind: FunctionKind = get_u8(pc).into();
                let index = *pc + 5; // jmp + i32 = 5
                let env = LexicalEnvironment::new(match scope.last() {
                    Some(r) => Some(r.borrow().environment.clone()),
                    None => None,
                });
                let value =
                    Value::new_compiled_function(agent, argc, index, inherits_this, kind, env);
                if op == Op::NewFunctionWithName {
                    handle!(value.set(
                        agent,
                        &ObjectKey::from("name"),
                        Value::String(name.to_string())
                    ));
                }
                stack.push(value);
            }
            Op::ProcessTemplateLiteral => {
                let len = get_i32(pc);
                let last_id = get_i32(pc) as usize;
                let mut end = agent.string_table[last_id].to_string();
                for _ in 0..(len - 1) {
                    // end = quasi + part + end
                    let id = get_i32(pc) as usize;
                    let quasi = agent.string_table[id].to_string();
                    let value = handle!(get_value(stack));
                    let part = if let Value::String(part) = value {
                        part
                    } else {
                        let value = handle!(value.to_object(agent));
                        let to_string = handle!(value.get(agent, &ObjectKey::from("toString")));
                        if let Value::String(part) = handle!(to_string.call(agent, value, vec![])) {
                            part
                        } else {
                            handle!(Err(Value::new_error(
                                agent,
                                "cannot convert template part to string"
                            )));
                            unreachable!();
                        }
                    };
                    end = quasi + part.as_str() + end.as_str();
                }
                stack.push(Value::String(end));
            }
            Op::NewObject => {
                let obj = Value::new_object(Value::Null);
                let inits = get_i32(pc);
                // TODO: keys are inserted in wrong order due to stack
                for _ in 0..inits {
                    let value = handle!(get_value(stack));
                    let key = handle!(get_value(stack));
                    let key = handle!(key.to_object_key(agent));
                    handle!(obj.set(agent, &key, value));
                }
                stack.push(obj);
            }
            Op::NewArray => {
                let len = get_i32(pc);
                let a = Value::new_array(agent);
                for i in 0..len {
                    let value = handle!(get_value(stack));
                    handle!(a.set(agent, &ObjectKey::from(i), value));
                }
                stack.push(a);
            }
            Op::NewIdentifier => {
                let id = get_i32(pc) as usize;
                let name = &agent.string_table[id];
                stack.push(Value::EnvironmentReference(
                    scope.last().unwrap().borrow().environment.clone(),
                    name.to_string(),
                ));
            }
            Op::NewMemberReference | Op::NewMemberReferenceNoConsumeStack => {
                let id = get_i32(pc) as usize;
                let name = &agent.string_table[id];
                let base = handle!(if op == Op::NewMemberReferenceNoConsumeStack {
                    get_value_no_consume(&stack)
                } else {
                    get_value(stack)
                });
                let base = handle!(base.to_object(agent));
                stack.push(Value::ValueReference(
                    Box::new(base),
                    ObjectKey::from(name.to_string()),
                ));
            }
            Op::NewComputedMemberReference | Op::NewComputedMemberReferenceNoConsumeStack => {
                let key = handle!(get_value(stack));
                let base = handle!(if op == Op::NewComputedMemberReferenceNoConsumeStack {
                    get_value_no_consume(&stack)
                } else {
                    get_value(stack)
                });
                let base = handle!(base.to_object(agent));
                let key = handle!(key.to_object_key(agent));
                stack.push(Value::ValueReference(Box::new(base), key));
            }
            Op::SetValue => {
                let value = handle!(get_value(stack));
                let target = stack.pop().unwrap();
                stack.push(handle!(match target {
                    Value::ValueReference(v, p) => v.set(agent, &p, value),
                    Value::EnvironmentReference(env, n) => {
                        handle!(env.borrow_mut().set(agent, n.as_str(), value));
                        Ok(Value::Null)
                    }
                    _ => Err(Value::new_error(
                        agent,
                        &format!("invalid assignment target {:?}", target)
                    )),
                }));
            }
            Op::GetValue => {
                let value = handle!(get_value(stack));
                stack.push(value);
            }
            Op::DropValue => {
                stack.pop();
            }
            Op::GetThis => {
                let this = handle!(scope
                    .last()
                    .unwrap()
                    .borrow()
                    .environment
                    .borrow()
                    .get_this(agent));
                stack.push(this);
            }
            Op::LexicalDeclaration => {
                let mutable = get_bool(pc);
                let id = get_i32(pc) as usize;
                let name = &agent.string_table[id];
                // println!("LexicalDeclaration {} {}", name, mutable);
                handle!(scope
                    .last()
                    .unwrap()
                    .borrow()
                    .environment
                    .borrow_mut()
                    .create(agent, name, mutable));
            }
            Op::LexicalInitialization => {
                let id = get_i32(pc) as usize;
                let name = &agent.string_table[id];
                let value = handle!(get_value(stack));
                let ctx = scope.last().unwrap().borrow();
                let mut env = ctx.environment.borrow_mut();
                env.initialize(name, value);
            }
            Op::Jump => {
                let position = get_i32(pc) as usize;
                *pc = position;
            }
            Op::JumpIfFalse => {
                let position = get_i32(pc) as usize;
                let value = handle!(get_value(stack));
                if !value.is_truthy() {
                    *pc = position;
                }
            }
            Op::JumpIfFalseNoConsume => {
                let position = get_i32(pc) as usize;
                let value = handle!(get_value_no_consume(stack));
                if !value.is_truthy() {
                    *pc = position;
                }
            }
            Op::JumpIfTrueNoConsume => {
                let position = get_i32(pc) as usize;
                let value = handle!(get_value_no_consume(stack));
                if value.is_truthy() {
                    *pc = position;
                }
            }
            // calling convention:
            // 1. push arguments onto stack
            // 2. read number of params
            // 3. adjust stack for extra/missing params
            // 4. jump to index of function body
            // 5. jump back to previous pc
            Op::Call | Op::TailCall => {
                let callee = handle!(get_value(stack));
                let this = handle!(get_value(stack));
                let this = if this == Value::Null {
                    this
                } else {
                    handle!(this.to_object(agent))
                };
                let argc = get_u8(pc);
                if let Value::Object(o) = callee.clone() {
                    match &o.kind {
                        ObjectKind::CompiledFunction {
                            params,
                            index,
                            inherits_this,
                            kind,
                            env,
                        } => {
                            if *kind == FunctionKind::Normal {
                                let paramc = *params;
                                let index = *index;
                                let inherits_this = *inherits_this;
                                if argc > paramc {
                                    let diff = argc - paramc;
                                    for _ in 0..diff {
                                        stack.pop().unwrap();
                                    }
                                } else if argc < paramc {
                                    let diff = paramc - argc;
                                    for _ in 0..diff {
                                        stack.push(Value::Empty);
                                    }
                                }
                                if op == Op::TailCall {
                                    scope.pop();
                                } else {
                                    positions.push(*pc); // jump back to previous pc
                                }
                                let ctx = ExecutionContext::new(LexicalEnvironment::new(Some(
                                    env.clone(),
                                )));
                                ctx.borrow_mut().function = Some(callee);
                                if !inherits_this {
                                    ctx.borrow().environment.borrow_mut().this = Some(this);
                                }
                                scope.push(ctx);
                                *pc = index; // jump to index of function body
                            } else {
                                let mut args: Vec<Value> = Vec::with_capacity(argc as usize);
                                let p = args.as_mut_ptr();
                                for i in (0..argc).rev() {
                                    let value = handle!(get_value(stack));
                                    unsafe {
                                        std::ptr::write(p.offset(i as isize), value);
                                    }
                                }
                                unsafe {
                                    args.set_len(argc as usize);
                                }
                                let r = handle!(callee.call(agent, this, args));
                                stack.push(r);
                            }
                        }
                        ObjectKind::BuiltinFunction(..) => {
                            let mut args: Vec<Value> = Vec::with_capacity(argc as usize);
                            let p = args.as_mut_ptr();
                            for i in (0..argc).rev() {
                                let value = handle!(get_value(stack));
                                unsafe {
                                    std::ptr::write(p.offset(i as isize), value);
                                }
                            }
                            unsafe {
                                args.set_len(argc as usize);
                            }
                            let r = handle!(callee.call(agent, this, args));
                            stack.push(r);
                        }
                        _ => handle!(Err(Value::new_error(agent, "callee is not a function"))),
                    }
                } else {
                    handle!(Err(Value::new_error(agent, "callee is not a function")));
                }
            }
            Op::InitReplace => {
                assert_eq!(get_u8(pc), Op::Jump as u8);
                let position = get_i32(pc) as usize;
                if stack.last().unwrap() == &Value::Empty {
                    stack.pop(); // will be pushed by default evaluation next
                } else {
                    // skip past default evaluation
                    *pc = position;
                }
            }
            Op::New => {
                let constructor = handle!(get_value(stack));
                let result = handle!(constructor.construct(agent, vec![]));
                stack.push(result);
            }
            Op::NewWithArgs => {
                let argc = get_i32(pc);
                let mut args: Vec<Value> = Vec::with_capacity(argc as usize);
                let p = args.as_mut_ptr();
                for i in (0..argc).rev() {
                    unsafe {
                        std::ptr::write(p.offset(i as isize), handle!(get_value(stack)));
                    }
                }
                unsafe {
                    args.set_len(argc as usize);
                }
                let constructor = handle!(get_value(stack));
                let result = handle!(constructor.construct(agent, args));
                stack.push(result);
            }
            Op::Return => {
                scope.pop();
                *pc = positions.pop().unwrap();
            }
            Op::Throw => {
                let position = try_stack.pop().unwrap();
                *exception = Some(handle!(get_value(stack)));
                *pc = position;
            }
            Op::ExceptionToStack => {
                let e = exception.take().unwrap();
                *exception = None;
                stack.push(e);
            }
            Op::PushTry => {
                assert_eq!(get_u8(pc), Op::Jump as u8);
                let position = get_i32(pc) as usize;
                try_stack.push(position);
            }
            Op::PopTry => {
                try_stack.pop();
            }
            Op::PushLoop => {
                assert_eq!(get_u8(pc), Op::Jump as u8);
                let r#break = get_i32(pc) as usize;
                let r#continue = *pc;
                loop_stack.push(LoopPosition {
                    r#break,
                    r#continue,
                });
            }
            Op::PopLoop => {
                loop_stack.pop().unwrap();
            }
            Op::Break => {
                *pc = loop_stack.pop().unwrap().r#break;
            }
            Op::Continue => {
                *pc = loop_stack.last().unwrap().r#continue;
            }
            Op::Await => {
                let value = handle!(get_value(stack));
                return Err(SuspendValue(value));
            }
            Op::Yield => return Err(SuspendValue(Value::Null)),
            Op::YieldWithOperand => {
                let value = handle!(get_value(stack));
                return Err(SuspendValue(value));
            }
            Op::GetIterator | Op::GetAsyncIterator => {
                // println!("GetIterator");
                let target = handle!(get_value(stack));
                let sym = handle!(if op == Op::GetAsyncIterator {
                    agent.well_known_symbol("asyncIterator")
                } else {
                    agent.well_known_symbol("iterator")
                }
                .to_object_key(agent));
                let iterator = handle!(target.get(agent, &sym));
                let iterator = handle!(iterator.call(agent, target, vec![]));
                let next = handle!(iterator.get(agent, &ObjectKey::from("next")));
                let iterator = Value::Iterator(Box::new(iterator), Box::new(next));
                stack.push(iterator);
            }
            Op::IteratorNext => {
                // println!("IteratorNext");
                let iterator = handle!(get_value_no_consume(stack));
                if let Value::Iterator(iterator, next) = iterator {
                    let result = handle!(next.call(agent, *iterator, vec![]));
                    let done = handle!(result.get(agent, &ObjectKey::from("done")));
                    if done == Value::True {
                        *pc = loop_stack.pop().unwrap().r#break;
                    } else {
                        let value = handle!(result.get(agent, &ObjectKey::from("value")));
                        stack.push(value);
                    }
                } else {
                    unreachable!()
                }
            }
            Op::AsyncIteratorNext => {
                let iterator = handle!(get_value_no_consume(stack));
                if let Value::Iterator(iterator, next) = iterator {
                    let promise = handle!(next.call(agent, *iterator, vec![]));
                    return Err(SuspendValue(promise)); // await promise
                } else {
                    unreachable!()
                }
            }
            Op::AsyncIteratorNextContinue => {
                let result = handle!(get_value(stack)); // result from promise above
                let done = handle!(result.get(agent, &ObjectKey::from("done")));
                if done == Value::True {
                    *pc = loop_stack.pop().unwrap().r#break;
                } else {
                    let value = handle!(result.get(agent, &ObjectKey::from("value")));
                    stack.push(value);
                }
            }
            Op::Eq => {
                let right = handle!(get_value(stack));
                let left = handle!(get_value(stack));
                stack.push(if left == right {
                    Value::True
                } else {
                    Value::False
                });
            }
            Op::Ne => {
                let right = handle!(get_value(stack));
                let left = handle!(get_value(stack));
                stack.push(if left == right {
                    Value::False
                } else {
                    Value::True
                });
            }
            Op::Mul => num_binop_num!(f64::mul),
            Op::Div => num_binop_num!(f64::div),
            Op::Mod => num_binop_num!(f64::rem),
            Op::Sub => num_binop_num!(f64::sub),
            Op::Pow => num_binop_num!(f64::powf),
            Op::UnarySub => {
                let value = handle!(get_value(stack));
                match value {
                    Value::Number(num) => stack.push(Value::Number(-num)),
                    _ => handle!(Err(Value::new_error(agent, "invalid number"))),
                };
            }
            Op::ShiftLeft => num_binop_num!(crate::num_util::f64_shl),
            Op::ShiftRight => num_binop_num!(crate::num_util::f64_shr),
            Op::LessThan => num_binop_bool!(f64::lt),
            Op::GreaterThan => num_binop_bool!(f64::gt),
            Op::LessThanOrEqual => num_binop_bool!(f64::le),
            Op::GreaterThanOrEqual => num_binop_bool!(f64::ge),
            Op::Add => {
                let right = handle!(get_value(stack));
                let left = handle!(get_value(stack));
                match left {
                    Value::Number(lnum) => {
                        if let Value::Number(rnum) = right {
                            stack.push(Value::Number(rnum + lnum));
                        } else {
                            handle!(Err(Value::new_error(agent, "rhs must be a number")))
                        }
                    }
                    Value::String(lstr) => {
                        if let Value::String(rstr) = right {
                            stack.push(Value::String(format!("{}{}", lstr, rstr)));
                        } else {
                            handle!(Err(Value::new_error(agent, "rhs must be a string")));
                        }
                    }
                    _ => handle!(Err(Value::new_error(
                        agent,
                        "lhs must be a number or a string"
                    ))),
                }
            }
            Op::Typeof => {
                let value = handle!(get_value(stack));
                let t = value.type_of();
                stack.push(Value::String(t.to_string()));
            }
            Op::Not => {
                let value = handle!(get_value(stack));
                stack.push(if value.is_truthy() {
                    Value::False
                } else {
                    Value::True
                });
            }
        }
    }

    Ok(match exception.take() {
        Some(e) => Err(e),
        None => Ok(stack.pop().unwrap_or(Value::Null)),
    })
}
