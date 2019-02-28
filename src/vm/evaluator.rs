use crate::agent::{Agent, Module};
use crate::value::{
    new_array, new_compiled_function, new_error, new_object, ObjectKey, ObjectKind, Value,
};
use crate::vm::{Compiled, Op};
use byteorder::{LittleEndian, ReadBytesExt};
use gc::{Gc, GcCell};
use rust_decimal::Decimal;
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

    pub fn get_this(&self) -> Result<Value, Value> {
        match self.this {
            Some(ref t) => Ok(t.clone()),
            None => match &self.parent {
                None => Err(new_error("invalid this")),
                Some(p) => p.borrow().get_this(),
            },
        }
    }

    fn create_binding(
        &mut self,
        name: &str,
        mutable: bool,
        exported: bool,
        module: Option<Module>,
    ) -> Result<(), Value> {
        match self.bindings.get(name) {
            Some(binding) if !binding.exported => {
                Err(new_error(&format!("binding {} already declared", name)))
            }
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

    pub fn create(&mut self, name: &str, mutable: bool) -> Result<(), Value> {
        self.create_binding(name, mutable, false, None)
    }

    pub fn create_export(&mut self, name: &str, mutable: bool) -> Result<(), Value> {
        self.create_binding(name, mutable, true, None)
    }

    pub fn create_import(&mut self, name: &str, module: Module) -> Result<(), Value> {
        self.create_binding(name, false, false, Some(module))
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

    pub fn set(&mut self, name: &str, value: Value) -> Result<(), Value> {
        match self.bindings.get_mut(name) {
            Some(b) => {
                if b.module.is_some() {
                    Err(new_error("cannot reassign constant binding"))
                } else if b.value.is_none() {
                    Err(new_error(&format!("reference error: {}", name)))
                } else if !b.mutable {
                    Err(new_error("cannot reassign constant binding"))
                } else {
                    b.value = Some(value);
                    Ok(())
                }
            }
            _ => match &self.parent {
                Some(p) => p.borrow_mut().set(name, value),
                _ => Err(new_error(&format!("reference error: {}", name))),
            },
        }
    }

    pub fn get(&self, name: &str) -> Result<Value, Value> {
        match self.bindings.get(name) {
            Some(Binding {
                module: Some(m), ..
            }) => {
                let module = m.borrow();
                let ctx = module.context.borrow();
                let env = ctx.environment.borrow();
                env.get(name)
            }
            Some(Binding { value: Some(v), .. }) => Ok((*v).clone()),
            Some(Binding { value: None, .. }) => {
                Err(new_error(&format!("reference error: {}", name)))
            }
            _ => match &self.parent {
                Some(p) => p.borrow().get(name),
                None => Err(new_error(&format!("reference error: {}", name))),
            },
        }
    }
}

#[derive(Trace, Finalize, Debug)]
pub struct ExecutionContext {
    pub function: Option<Value>,
    pub environment: Gc<GcCell<LexicalEnvironment>>,
}

impl ExecutionContext {
    pub fn new(environment: Gc<GcCell<LexicalEnvironment>>) -> Gc<GcCell<ExecutionContext>> {
        Gc::new(GcCell::new(ExecutionContext {
            function: None,
            environment,
        }))
    }
}

pub fn evaluate_at(
    agent: &Agent,
    compiled: &Compiled,
    pc: usize,
    stack: &mut Vec<Value>,
    scope: &mut Vec<Gc<GcCell<ExecutionContext>>>,
    positions: &mut Vec<usize>,
) -> Result<Value, Value> {
    let mut try_stack: Vec<usize> = vec![compiled.code.len()];
    let mut loop_stack: Vec<usize> = Vec::new();
    let mut pc: usize = pc;

    let get_u8 = |pc: &mut usize| {
        let v = compiled.code[*pc];
        *pc += 1;
        v
    };

    let get_bool = |pc: &mut usize| get_u8(pc) == 1;

    let get_i32 = |pc: &mut usize| {
        let n = (&compiled.code[*pc..]).read_i32::<LittleEndian>().unwrap();
        *pc += 4;
        n
    };

    let get_value = |stack: &mut Vec<Value>| {
        let value = stack.pop().unwrap();
        match value {
            Value::EnvironmentReference(env, k) => env.borrow().get(k.as_str()),
            Value::ValueReference(v, p) => v.get(&p),
            _ => Ok(value),
        }
    };

    let get_value_no_consume = |stack: &Vec<Value>| {
        let value = stack.last().unwrap();
        match value {
            Value::EnvironmentReference(env, k) => env.borrow().get(k.as_str()),
            Value::ValueReference(v, p) => v.get(&p),
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
                    handle!(Err(new_error("rval must be a number")))
                }
            } else {
                handle!(Err(new_error("lval must be a umber")))
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
                    handle!(Err(new_error("rval must be a number")))
                }
            } else {
                handle!(Err(new_error("lval must be a umber")))
            }
        }};
    }

    let mut exception: Option<Value> = None;

    'main: loop {
        macro_rules! handle {
            ($ex:expr) => {
                match $ex {
                    Ok(v) => v,
                    Err(e) => {
                        let position = try_stack.pop().expect("try_stack context missing");
                        pc = position;
                        exception = Some(e);
                        continue 'main;
                    }
                }
            };
        }

        if pc >= compiled.code.len() {
            break;
        }
        let op = compiled.code[pc].into();
        pc += 1;
        match op {
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
                let id = get_i32(&mut pc) as usize;
                let value = compiled.number_table[id];
                stack.push(Value::Number(value));
            }
            Op::NewString => {
                let id = get_i32(&mut pc) as usize;
                let str = &compiled.string_table[id];
                stack.push(Value::String(str.clone()));
            }
            Op::NewFunction => {
                let argc = get_u8(&mut pc);
                let inherits_this = get_bool(&mut pc);
                let index = pc + 5; // jmp + i32 = 5
                let env = LexicalEnvironment::new(match scope.last() {
                    Some(r) => Some(r.borrow().environment.clone()),
                    None => None,
                });
                // println!("NewFunction {:?}", env);
                let value = new_compiled_function(agent, argc, index, compiled, inherits_this, env);
                stack.push(value);
            }
            Op::ProcessTemplateLiteral => {
                let len = get_i32(&mut pc);
                let last_id = get_i32(&mut pc) as usize;
                let mut end = compiled.string_table[last_id].to_string();
                for _ in 0..(len - 1) {
                    // end = quasi + part + end
                    let id = get_i32(&mut pc) as usize;
                    let quasi = compiled.string_table[id].to_string();
                    let value = handle!(get_value(stack));
                    let part = if let Value::String(part) = value {
                        part
                    } else {
                        let value = handle!(value.to_object(agent));
                        let to_string = handle!(value.get(&ObjectKey::from("toString")));
                        if let Value::String(part) = handle!(to_string.call(agent, value, vec![])) {
                            part
                        } else {
                            handle!(Err(new_error("cannot convert template part to string")));
                            unreachable!();
                        }
                    };
                    end = quasi + part.as_str() + end.as_str();
                }
                stack.push(Value::String(end));
            }
            Op::NewObject => {
                let obj = new_object(Value::Null);
                let inits = get_i32(&mut pc);
                for _ in 0..inits {
                    let value = handle!(get_value(stack));
                    let key = handle!(get_value(stack));
                    let key = handle!(key.to_object_key());
                    handle!(obj.set(&key, value));
                }
                stack.push(obj);
            }
            Op::NewArray => {
                let len = get_i32(&mut pc);
                let a = new_array(agent);
                for i in 0..len {
                    let value = handle!(get_value(stack));
                    handle!(a.set(&ObjectKey::from(i), value));
                }
                stack.push(a);
            }
            Op::NewIdentifier => {
                let id = get_i32(&mut pc) as usize;
                let name = &compiled.string_table[id];
                stack.push(Value::EnvironmentReference(
                    scope.last().unwrap().borrow().environment.clone(),
                    name.to_string(),
                ));
            }
            Op::NewMemberReference | Op::NewMemberReferenceNoConsumeStack => {
                let id = get_i32(&mut pc) as usize;
                let name = &compiled.string_table[id];
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
                let key = handle!(key.to_object_key());
                stack.push(Value::ValueReference(Box::new(base), key));
            }
            Op::SetValue => {
                let value = handle!(get_value(stack));
                let target = stack.pop().unwrap();
                stack.push(handle!(match target {
                    Value::ValueReference(v, p) => v.set(&p, value),
                    Value::EnvironmentReference(env, n) => {
                        handle!(env.borrow_mut().set(n.as_str(), value));
                        Ok(Value::Null)
                    }
                    _ => Err(new_error(&format!(
                        "invalid assignment target {:?}",
                        target
                    ))),
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
                    .get_this());
                stack.push(this);
            }
            Op::LexicalDeclaration => {
                let mutable = get_bool(&mut pc);
                let id = get_i32(&mut pc) as usize;
                let name = &compiled.string_table[id];
                // println!("LexicalDeclaration {} {}", name, mutable);
                handle!(scope
                    .last()
                    .unwrap()
                    .borrow()
                    .environment
                    .borrow_mut()
                    .create(name, mutable));
            }
            Op::LexicalInitialization => {
                let id = get_i32(&mut pc) as usize;
                let name = &compiled.string_table[id];
                let value = handle!(get_value(stack));
                let ctx = scope.last().unwrap().borrow();
                let mut env = ctx.environment.borrow_mut();
                env.initialize(name, value);
            }
            Op::Jump => {
                let position = get_i32(&mut pc) as usize;
                pc = position;
            }
            Op::JumpIfFalse => {
                let position = get_i32(&mut pc) as usize;
                let value = handle!(get_value(stack));
                if !value.is_truthy() {
                    pc = position;
                }
            }
            // calling convention:
            // 1. push arguments onto stack
            // 2. read number of params
            // 3. adjust stack for extra/missing params
            // 4. jump to index of function body
            // 5. jump back to previous pc
            Op::Call | Op::TailCall => {
                // println!("Call");
                let callee = handle!(get_value(stack));
                let this = handle!(get_value(stack));
                let this = if this == Value::Null {
                    this
                } else {
                    this.to_object(agent)?
                };
                let argc = get_u8(&mut pc);
                if let Value::Object(o) = callee.clone() {
                    match &o.kind {
                        ObjectKind::CompiledFunction(paramc, index, cc, inherits_this, env) => {
                            if unsafe { &**cc } == compiled {
                                let paramc = *paramc;
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
                                    positions.push(pc); // jump back to previous pc
                                }
                                // println!("PushContext");
                                let ctx = ExecutionContext::new(LexicalEnvironment::new(Some(
                                    env.clone(),
                                )));
                                ctx.borrow_mut().function = Some(callee);
                                if !inherits_this {
                                    ctx.borrow().environment.borrow_mut().this = Some(this);
                                }
                                scope.push(ctx);
                                pc = index; // jump to index of function body
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
                        _ => handle!(Err(new_error("callee is not a function"))),
                    }
                } else {
                    handle!(Err(new_error("callee is not a function")));
                }
            }
            Op::InitReplace => {
                assert_eq!(get_u8(&mut pc), Op::Jump as u8);
                let position = get_i32(&mut pc) as usize;
                if stack.last().unwrap() == &Value::Empty {
                    stack.pop(); // will be pushed by default evaluation next
                } else {
                    // skip past default evaluation
                    pc = position;
                }
            }
            Op::New => {
                let constructor = handle!(get_value(stack));
                let result = handle!(constructor.construct(agent, vec![]));
                stack.push(result);
            }
            Op::NewWithArgs => {
                let argc = get_i32(&mut pc);
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
            Op::End => {
                scope.pop();
                pc = positions.pop().unwrap();
            }
            Op::Return => {
                scope.pop();
                pc = positions.pop().unwrap();
            }
            Op::Throw => {
                let position = try_stack.pop().unwrap();
                exception = Some(handle!(get_value(stack)));
                pc = position;
            }
            Op::ExceptionToStack => {
                let e = exception.unwrap();
                exception = None;
                stack.push(e);
            }
            Op::PushTry => {
                assert_eq!(get_u8(&mut pc), Op::Jump as u8);
                let position = get_i32(&mut pc) as usize;
                try_stack.push(position);
            }
            Op::PopTry => {
                try_stack.pop();
            }
            Op::PushLoop => {
                assert_eq!(get_u8(&mut pc), Op::JumpIfFalse as u8);
                let position = get_i32(&mut pc) as usize;
                loop_stack.push(position);
            }
            Op::PopLoop => {
                loop_stack.pop();
            }
            Op::Break => {
                let position = loop_stack.pop().unwrap();
                pc = position;
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
            Op::Mul => num_binop_num!(Decimal::mul),
            Op::Div => num_binop_num!(Decimal::div),
            Op::Mod => num_binop_num!(Decimal::rem),
            Op::Sub => num_binop_num!(Decimal::sub),
            Op::Pow => panic!(), // num_binop_num!(Decimal::pow),
            Op::UnarySub => {
                let value = handle!(get_value(stack));
                match value {
                    Value::Number(num) => stack.push(Value::Number(-num)),
                    _ => handle!(Err(new_error("invalid number"))),
                };
            }
            Op::LessThan => num_binop_bool!(Decimal::lt),
            Op::GreaterThan => num_binop_bool!(Decimal::gt),
            Op::LessThanOrEqual => num_binop_bool!(Decimal::le),
            Op::GreaterThanOrEqual => num_binop_bool!(Decimal::ge),
            Op::Add => {
                let right = handle!(get_value(stack));
                let left = handle!(get_value(stack));
                match left {
                    Value::Number(lnum) => {
                        if let Value::Number(rnum) = right {
                            stack.push(Value::Number(rnum + lnum));
                        } else {
                            handle!(Err(new_error("rhs must be a number")))
                        }
                    }
                    Value::String(lstr) => {
                        if let Value::String(rstr) = right {
                            stack.push(Value::String(format!("{}{}", lstr, rstr)));
                        } else {
                            handle!(Err(new_error("rhs must be a string")));
                        }
                    }
                    _ => handle!(Err(new_error("lhs must be a number or a string"))),
                }
            }
            Op::Typeof => {
                let value = handle!(get_value(stack));
                let t = value.type_of();
                stack.push(Value::String(t.to_string()));
            }
        }
    }

    match exception {
        Some(e) => Err(e),
        None => Ok(stack.pop().unwrap_or(Value::Null)),
    }
}
