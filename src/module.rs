use crate::intrinsics::{
    create_array_prototype, create_boolean_prototype, create_function_prototype,
    create_float_prototype, create_object_prototype, create_promise, create_promise_prototype,
    create_string_prototype, create_symbol, create_symbol_prototype,
};
use crate::parser::{Node, Operator, Parser};
use crate::value::{
    new_array, new_boolean_object, new_builtin_function, new_error, new_function,
    new_float_object, new_object, new_string_object, BuiltinFunctionWrap, ObjectKey, ObjectKind,
    Symbol, Value,
};
use gc::{Gc, GcCell};
use std::collections::{HashMap, HashSet, VecDeque};
use std::ops::{Div, Mul, Rem, Sub};
// use std::rc::Gc;

#[derive(Debug, Trace, Finalize)]
struct Binding {
    value: Option<Value>,
    mutable: bool,
    exported: bool,
    module: Option<Module>,
}

#[derive(Debug, Trace, Finalize)]
pub struct LexicalEnvironment {
    parent: Option<Gc<GcCell<LexicalEnvironment>>>,
    bindings: HashMap<String, Binding>,
}

impl LexicalEnvironment {
    fn new() -> LexicalEnvironment {
        LexicalEnvironment {
            parent: None,
            bindings: HashMap::new(),
        }
    }

    fn has(&self, name: &str) -> bool {
        if self.bindings.contains_key(name) {
            true
        } else {
            match &self.parent {
                None => false,
                Some(r) => r.borrow().has(name),
            }
        }
    }

    fn create_binding(
        &mut self,
        name: &str,
        mutable: bool,
        exported: bool,
        module: Option<Module>,
    ) -> Result<(), Value> {
        if self.has(name) {
            Err(new_error("binding already declared"))
        } else {
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

    fn create(&mut self, name: &str, mutable: bool) -> Result<(), Value> {
        self.create_binding(name, mutable, false, None)
    }

    fn create_export(&mut self, name: &str, mutable: bool) -> Result<(), Value> {
        self.create_binding(name, mutable, true, None)
    }

    fn create_import(&mut self, name: &str, module: Module) -> Result<(), Value> {
        self.create_binding(name, false, false, Some(module))
    }

    fn set(&mut self, name: &str, value: Value) -> Result<(), Value> {
        match self.bindings.get_mut(name) {
            Some(m) => {
                if !m.mutable && m.value.is_some() {
                    Err(new_error("Assignment to constant binding"))
                } else {
                    m.value = Some(value);
                    Ok(())
                }
            }
            _ => match &self.parent {
                Some(r) => r.borrow_mut().set(name, value),
                _ => Err(new_error("reference error")),
            },
        }
    }

    fn get(&self, name: &str) -> Result<Value, Value> {
        match self.bindings.get(name) {
            Some(Binding {
                value: None,
                module: Some(m),
                ..
            }) => m.borrow().context.environment.borrow().get(name),
            Some(Binding { value: Some(v), .. }) => Ok(v.clone()),
            Some(Binding { value: None, .. }) => Err(new_error("reference error")),
            _ => match &self.parent {
                Some(r) => r.borrow().get(name),
                _ => Err(new_error("reference error")),
            },
        }
    }
}

#[derive(Debug, Trace, Finalize)]
pub struct ExecutionContext {
    pub this: Option<Value>,
    pub function: Option<Value>,
    pub environment: Gc<GcCell<LexicalEnvironment>>,
}

#[derive(Debug, PartialEq, Clone, Trace, Finalize)]
enum ModuleStatus {
    Uninstantiated,
    Instantiating,
    Instantiated,
    Evaluating,
    Evaluated,
}

#[derive(Debug, Finalize)]
struct ModuleX {
    filename: String,
    ast: Node,
    context: ExecutionContext,
    imports: HashSet<String>,
    status: ModuleStatus,
    dfs_index: u32,
    dfs_ancestor_index: u32,
}

unsafe impl gc::Trace for ModuleX {
    #[inline]
    unsafe fn trace(&self) {
        gc::Trace::trace(&self.context);
    }
    #[inline]
    unsafe fn root(&self) {
        gc::Trace::root(&self.context);
    }
    #[inline]
    unsafe fn unroot(&self) {
        gc::Trace::unroot(&self.context);
    }
    #[inline]
    fn finalize_glue(&self) {
        gc::Finalize::finalize(self);
        gc::Trace::finalize_glue(&self.context);
    }
}

type Module = Gc<GcCell<ModuleX>>;

fn print(_: &Agent, _ctx: &mut ExecutionContext, args: Vec<Value>) -> Result<Value, Value> {
    let mut output = String::new();
    for arg in args {
        match &arg {
            Value::Null => output += " null",
            Value::True => output += " true",
            Value::False => output += " false",
            Value::Float(n) => output += &format!(" {}", n),
            Value::String(s) => output += &format!(" '{}'", s),
            Value::Symbol(Symbol(_, _, d)) => {
                if let Some(s) = d {
                    output += &format!(" Symbol({})", s);
                } else {
                    output += " Symbol()";
                }
            }
            Value::Object(_) => output += " {...}",
            _ => unreachable!(),
        }
    }
    println!("{}", output.trim());
    Ok(Value::Null)
}

fn inner_module_instantiation(
    agent: &Agent,
    module: Module,
    stack: &mut Vec<Module>,
    mut index: u32,
) -> Result<u32, Value> {
    let status = module.borrow().status.clone();
    match status {
        ModuleStatus::Instantiating | ModuleStatus::Instantiated | ModuleStatus::Evaluated => {
            Ok(index)
        }
        _ => {
            assert!(module.borrow().status == ModuleStatus::Uninstantiated);
            module.borrow_mut().status = ModuleStatus::Instantiating;
            module.borrow_mut().dfs_index = index;
            module.borrow_mut().dfs_ancestor_index = index;
            index += 1;
            stack.push(module.clone());
            for import in module.borrow().imports.clone() {
                let m = agent.load(import.as_str(), module.borrow().filename.as_str())?;
                index = inner_module_instantiation(agent, m.clone(), stack, index)?;
                if m.borrow().status == ModuleStatus::Instantiating {
                    module.borrow_mut().dfs_ancestor_index = std::cmp::min(
                        module.borrow().dfs_ancestor_index,
                        m.borrow().dfs_ancestor_index,
                    );
                }
            }
            if module.borrow().dfs_ancestor_index == module.borrow().dfs_index {
                let mut done = false;
                while !done {
                    let m = stack.pop().unwrap();
                    m.borrow_mut().status = ModuleStatus::Instantiated;
                    if m.borrow().filename == module.borrow().filename {
                        done = true;
                    }
                }
            }
            Ok(index)
        }
    }
}

fn inner_module_evaluation(
    agent: &Agent,
    module: Module,
    stack: &mut Vec<Module>,
    mut index: u32,
) -> Result<u32, Value> {
    let status = module.borrow().status.clone();
    if status == ModuleStatus::Evaluated || status == ModuleStatus::Evaluating {
        Ok(index)
    } else {
        assert!(module.borrow().status == ModuleStatus::Instantiated);
        module.borrow_mut().status = ModuleStatus::Evaluating;
        module.borrow_mut().dfs_index = index;
        module.borrow_mut().dfs_ancestor_index = index;
        index += 1;
        stack.push(module.clone());
        for import in module.borrow().imports.clone() {
            let m = agent.load(import.as_str(), module.borrow().filename.as_str())?;
            index = inner_module_evaluation(agent, m.clone(), stack, index)?;
            if m.borrow().status == ModuleStatus::Evaluating {
                m.borrow_mut().dfs_ancestor_index = std::cmp::min(
                    module.borrow().dfs_ancestor_index,
                    m.borrow().dfs_ancestor_index,
                );
            }
        }
        {
            let ast = module.borrow().ast.clone();
            agent.evaluate(&mut module.borrow_mut().context, ast)?;
        }
        if module.borrow().dfs_ancestor_index == module.borrow().dfs_index {
            let mut done = false;
            while !done {
                let m = stack.pop().unwrap();
                m.borrow_mut().status = ModuleStatus::Evaluated;
                if m.borrow().filename == module.borrow().filename {
                    done = true;
                }
            }
        }
        Ok(index)
    }
}

impl ModuleX {
    fn new(filename: &str, agent: &Agent) -> Result<ModuleX, Value> {
        let source = std::fs::read_to_string(filename).expect("no such file");
        let mut module = ModuleX {
            filename: filename.to_string(),
            context: ExecutionContext {
                this: None,
                function: None,
                environment: Gc::new(GcCell::new(LexicalEnvironment::new())),
            },
            ast: Parser::parse(&source)?,
            imports: HashSet::new(),
            status: ModuleStatus::Uninstantiated,
            dfs_index: 0,
            dfs_ancestor_index: 0,
        };

        module.context.environment.borrow_mut().parent = Some(agent.root_env.clone());

        if let Node::StatementList(list) = &module.ast {
            for node in list {
                match &node {
                    Node::ImportDefaultDeclaration(specifier, name) => {
                        let mr = agent.load(specifier, name)?;
                        module
                            .context
                            .environment
                            .borrow_mut()
                            .create_import(name, mr)?;
                        module.imports.insert(specifier.to_string());
                    }
                    Node::ImportNamedDeclaration(specifier, names) => {
                        let mr = agent.load(specifier, filename)?;
                        for name in names {
                            module
                                .context
                                .environment
                                .borrow_mut()
                                .create_import(name, mr.clone())?;
                            module.imports.insert(specifier.to_string());
                        }
                    }
                    Node::ImportStandardDeclaration(specifier, names) => match specifier.as_str() {
                        "debug" => {
                            if names.len() != 1 || names[0] != "print" {
                                return Err(new_error("unknown item from debug"));
                            }
                            module
                                .context
                                .environment
                                .borrow_mut()
                                .create("print", false)?;
                            module
                                .context
                                .environment
                                .borrow_mut()
                                .set("print", new_builtin_function(agent, print))?;
                        }
                        _ => return Err(new_error("unknown standard module")),
                    },
                    Node::ExportDeclaration(decl) => match *decl.clone() {
                        Node::LexicalDeclaration(name, _, mutable) => {
                            module
                                .context
                                .environment
                                .borrow_mut()
                                .create_export(name.as_str(), mutable)?;
                        }
                        Node::FunctionDeclaration(name, args, body) => {
                            module
                                .context
                                .environment
                                .borrow_mut()
                                .create_export(name.as_str(), false)?;
                            let value =
                                new_function(agent, args, body, module.context.environment.clone());
                            module
                                .context
                                .environment
                                .borrow_mut()
                                .set(name.as_str(), value)?;
                        }
                        _ => {}
                    },
                    _ => {}
                }
            }
        }

        Ok(module)
    }
}

pub fn call(a: &Agent, f: Value, v: Value, args: Vec<Value>) -> Result<Value, Value> {
    let mut new_ctx = ExecutionContext {
        this: Some(v),
        function: Some(f.clone()),
        environment: Gc::new(GcCell::new(LexicalEnvironment::new())),
    };
    match &f {
        Value::Object(o) => match &o.kind {
            ObjectKind::Function(params, body, parent_env) => {
                new_ctx.environment.borrow_mut().parent = Some(parent_env.clone());
                for (i, value) in args.iter().enumerate() {
                    let binding = params[i].clone();
                    new_ctx
                        .environment
                        .borrow_mut()
                        .create(binding.as_str(), false)?;
                    new_ctx
                        .environment
                        .borrow_mut()
                        .set(binding.as_str(), value.clone())?;
                }
                let r = a.evaluate(&mut new_ctx, *body.clone());
                match &r {
                    Err(Value::ReturnCompletion(v)) => Ok(*v.clone()),
                    Err(_) => r,
                    _ => Ok(Value::Null),
                }
            }
            ObjectKind::BuiltinFunction(BuiltinFunctionWrap(bfn), _) => bfn(a, &mut new_ctx, args),
            _ => Err(new_error("not a function")),
        },
        _ => Err(new_error("not a function")),
    }
}

pub fn construct(a: &Agent, c: Value, args: Vec<Value>) -> Result<Value, Value> {
    match &c {
        Value::Object(o) => {
            let mut prototype = o.get(ObjectKey::from("prototype"))?;
            if prototype.type_of() != "object" {
                prototype = a.intrinsics.object_prototype.clone();
            }
            let this = new_object(prototype);
            match &o.kind {
                ObjectKind::Function(_, _, _) | ObjectKind::BuiltinFunction(_, _) => {
                    let r = call(a, c, this.clone(), args)?;
                    if r.type_of() == "object" {
                        Ok(r)
                    } else {
                        Ok(this)
                    }
                }
                _ => Err(new_error("not a function")),
            }
        }
        _ => Err(new_error("not a function")),
    }
}

pub fn set(target: &Value, property: &ObjectKey, value: Value) -> Result<Value, Value> {
    match target {
        Value::Object(o) => o.set(property.clone(), value, o.clone()),
        _ => Err(new_error("base must be an object")),
    }
}

pub fn get(target: &Value, property: &ObjectKey) -> Result<Value, Value> {
    match target {
        Value::Object(o) => o.get(property.clone()),
        _ => Err(new_error("base must be an object")),
    }
}

type JobFn = fn(&Agent, Vec<Value>);
#[derive(Finalize)]
struct JobFnWrap(JobFn);

impl std::fmt::Debug for JobFnWrap {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "job")
    }
}

unsafe impl gc::Trace for JobFnWrap {
    #[inline]
    unsafe fn trace(&self) {}
    #[inline]
    unsafe fn root(&self) {}
    #[inline]
    unsafe fn unroot(&self) {}
    #[inline]
    fn finalize_glue(&self) {}
}

#[derive(Debug, Trace, Finalize)]
struct Job(JobFnWrap, Vec<Value>);

#[derive(Debug)]
pub struct Intrinsics {
    pub object_prototype: Value,
    pub array_prototype: Value,
    pub function_prototype: Value,
    pub boolean_prototype: Value,
    pub string_prototype: Value,
    pub float_prototype: Value,
    pub promise_prototype: Value,
    pub promise: Value,
    pub symbol_prototype: Value,
    pub symbol: Value,
}

#[derive(Debug)]
pub struct Agent {
    pub intrinsics: Intrinsics,
    modules: GcCell<HashMap<String, Module>>,
    root_env: Gc<GcCell<LexicalEnvironment>>,
    job_queue: GcCell<VecDeque<Job>>,
}

impl Agent {
    pub fn new() -> Agent {
        let object_prototype = create_object_prototype();
        let array_prototype = create_array_prototype(object_prototype.clone());
        let function_prototype = create_function_prototype(object_prototype.clone());
        let boolean_prototype = create_boolean_prototype(object_prototype.clone());
        let float_prototype = create_float_prototype(object_prototype.clone());
        let string_prototype = create_string_prototype(object_prototype.clone());
        let symbol_prototype = create_symbol_prototype(object_prototype.clone());
        let mut agent = Agent {
            intrinsics: Intrinsics {
                object_prototype: object_prototype.clone(),
                array_prototype,
                function_prototype,
                boolean_prototype,
                float_prototype,
                string_prototype,
                promise_prototype: Value::Null,
                promise: Value::Null,
                symbol_prototype,
                symbol: Value::Null,
            },
            root_env: Gc::new(GcCell::new(LexicalEnvironment::new())),
            modules: GcCell::new(HashMap::new()),
            job_queue: GcCell::new(VecDeque::new()),
        };

        agent.intrinsics.promise_prototype =
            create_promise_prototype(&agent, object_prototype.clone());
        agent.intrinsics.promise =
            create_promise(&agent, agent.intrinsics.promise_prototype.clone());

        agent.intrinsics.symbol = create_symbol(&agent, agent.intrinsics.symbol_prototype.clone());

        {
            let mut env = agent.root_env.borrow_mut();
            env.create("Promise", true).unwrap();
            env.set("Promise", agent.intrinsics.promise.clone())
                .unwrap();

            env.create("Symbol", true).unwrap();
            env.set("Symbol", agent.intrinsics.symbol.clone()).unwrap();
        }

        agent
    }

    fn load(&self, specifier: &str, referrer: &str) -> Result<Module, Value> {
        let filename = std::path::Path::new(referrer)
            .parent()
            .unwrap()
            .join(specifier);
        let filename = filename.to_str().unwrap();
        if !self.modules.borrow().contains_key(filename) {
            let module = Gc::new(GcCell::new(ModuleX::new(filename, self)?));
            // let imports = module.imports.clone();
            self.modules
                .borrow_mut()
                .insert(filename.to_string(), module.clone());
            Ok(module)
        } else {
            let map = self.modules.borrow();
            let module = map.get(filename).unwrap().clone();
            Ok(module)
        }
    }

    pub fn import(&self, specifier: &str, referrer: &str) -> Result<(), Value> {
        let module = self.load(specifier, referrer)?;
        inner_module_instantiation(self, module.clone(), &mut Vec::new(), 0)?;
        inner_module_evaluation(self, module.clone(), &mut Vec::new(), 0)?;
        Ok(())
    }

    pub fn enqueue_job(&self, f: JobFn, args: Vec<Value>) {
        self.job_queue.borrow_mut().push_back(Job(JobFnWrap(f), args));
    }

    pub fn run_jobs(&self) {
        loop {
            let mut job = self.job_queue.borrow_mut().pop_front();
            match &mut job {
                Some(Job(JobFnWrap(f), args)) => {
                    f(self, std::mem::replace(args, Vec::new()));
                }
                None => break,
            }
        }
    }

    fn evaluate(&self, ctx: &mut ExecutionContext, node: Node) -> Result<Value, Value> {
        match node {
            Node::ReturnStatement(expr) => Err(Value::ReturnCompletion(Box::new(
                self.evaluate(ctx, *expr)?,
            ))),
            Node::ThrowStatement(expr) => Err(self.evaluate(ctx, *expr)?),
            Node::ThisExpression => match &ctx.this {
                None => Err(new_error("invalid this")),
                Some(v) => Ok(v.clone()),
            },
            Node::FloatLiteral(n) => Ok(Value::Float(n)),
            Node::StringLiteral(s) => Ok(Value::String(s)),
            Node::NullLiteral => Ok(Value::Null),
            Node::TrueLiteral => Ok(Value::True),
            Node::FalseLiteral => Ok(Value::False),
            Node::ArrayLiteral(nodes) => {
                let array = new_array(self);
                let mut len = 0;
                match &array {
                    Value::Object(o) => {
                        for node in nodes {
                            let value = self.evaluate(ctx, node)?;
                            o.set(ObjectKey::from(len), value, o.clone())?;
                            len += 1;
                        }
                        o.set(
                            ObjectKey::from("length"),
                            Value::Float(f64::from(len)),
                            o.clone(),
                        )?;
                    }
                    _ => unreachable!(),
                }
                Ok(array)
            }
            Node::ObjectLiteral(list) => {
                let obj = new_object(self.intrinsics.object_prototype.clone());
                match &obj {
                    Value::Object(o) => {
                        for init in list {
                            match init {
                                Node::PropertyInitializer(key, expr) => {
                                    let value = self.evaluate(ctx, *expr)?;
                                    o.set(ObjectKey::from(key), value, o.clone())?;
                                }
                                _ => unreachable!(),
                            }
                        }
                    }
                    _ => unreachable!(),
                }
                Ok(obj)
            }
            Node::StatementList(nodes) => self.evaluate_statement_list(ctx, nodes),
            Node::BlockStatement(nodes) => {
                let new = Gc::new(GcCell::new(LexicalEnvironment::new()));
                let old = std::mem::replace(&mut ctx.environment, new);
                ctx.environment.borrow_mut().parent = Some(old.clone());
                let result = self.evaluate_statement_list(ctx, nodes);
                std::mem::replace(&mut ctx.environment, old);
                result
            }
            Node::TryStatement(try_clause, catch_clause) => {
                let result = self.evaluate(ctx, *try_clause);
                match result {
                    Ok(v) => Ok(v),
                    Err(Value::ReturnCompletion(_)) => result,
                    Err(_) => self.evaluate(ctx, *catch_clause),
                }
            }
            Node::BoundTryStatement(try_clause, binding, catch_clause) => {
                let result = self.evaluate(ctx, *try_clause);
                match result {
                    Ok(v) => Ok(v),
                    Err(Value::ReturnCompletion(_)) => result,
                    Err(e) => {
                        let new = Gc::new(GcCell::new(LexicalEnvironment::new()));
                        let old = std::mem::replace(&mut ctx.environment, new);
                        ctx.environment.borrow_mut().parent = Some(old.clone());
                        ctx.environment
                            .borrow_mut()
                            .create(binding.as_str(), false)?;
                        ctx.environment.borrow_mut().set(binding.as_str(), e)?;
                        let r = self.evaluate(ctx, *catch_clause);
                        std::mem::replace(&mut ctx.environment, old);
                        r
                    }
                }
            }
            Node::IfStatement(test, consequent) => {
                if self.evaluate(ctx, *test)?.is_truthy() {
                    self.evaluate(ctx, *consequent)
                } else {
                    Ok(Value::Null)
                }
            }
            Node::IfElseStatement(test, consequent, alternative) => {
                if self.evaluate(ctx, *test)?.is_truthy() {
                    self.evaluate(ctx, *consequent)
                } else {
                    self.evaluate(ctx, *alternative)
                }
            }
            Node::ConditionalExpression(test, consequent, alternative) => {
                if self.evaluate(ctx, *test)?.is_truthy() {
                    self.evaluate(ctx, *consequent)
                } else {
                    self.evaluate(ctx, *alternative)
                }
            }
            Node::ExpressionStatement(expr) => self.evaluate(ctx, *expr),
            Node::CallExpression(name, args) => {
                let this;
                let val;
                match *name.clone() {
                    Node::MemberExpression(base, property) => {
                        let base = self.evaluate(ctx, *base)?;
                        this = self.to_object(base)?;
                        val = get(&this, &ObjectKey::from(property))?;
                    }
                    Node::ComputedMemberExpression(base, property) => {
                        let base = self.evaluate(ctx, *base)?;
                        this = self.to_object(base)?;
                        let property = self.evaluate(ctx, *property)?.to_object_key()?;
                        val = get(&this, &property)?;
                    }
                    _ => {
                        this = Value::Null;
                        val = self.evaluate(ctx, *name)?;
                    }
                };
                let mut values = Vec::new();
                for arg in args {
                    values.push(self.evaluate(ctx, arg.clone())?);
                }
                call(self, val, this, values)
            }
            Node::UnaryExpression(op, expr) => {
                let value = self.evaluate(ctx, *expr)?;
                self.evaluate_unop(op, value)
            }
            Node::BinaryExpression(left, op, right) => match op {
                Operator::Assign => match *left {
                    Node::MemberExpression(base, property) => {
                        let base = self.evaluate(ctx, *base)?;
                        let base = self.to_object(base)?;
                        let rval = self.evaluate(ctx, *right)?;
                        set(&base, &ObjectKey::from(property), rval)
                    }
                    Node::ComputedMemberExpression(base, property) => {
                        let base = self.evaluate(ctx, *base)?;
                        let base = self.to_object(base)?;
                        let key = self.evaluate(ctx, *property)?.to_object_key()?;
                        let rval = self.evaluate(ctx, *right)?;
                        set(&base, &key, rval)
                    }
                    Node::Identifier(name) => {
                        let rval = self.evaluate(ctx, *right)?;
                        ctx.environment
                            .borrow_mut()
                            .set(name.as_str(), rval.clone())?;
                        Ok(rval)
                    }
                    _ => Err(new_error("invalid left hand side")),
                },
                Operator::LogicalAND => {
                    let lval = self.evaluate(ctx, *left)?;
                    if lval.is_truthy() {
                        self.evaluate(ctx, *right)
                    } else {
                        Ok(lval)
                    }
                }
                Operator::LogicalOR => {
                    let lval = self.evaluate(ctx, *left)?;
                    if lval.is_truthy() {
                        Ok(lval)
                    } else {
                        self.evaluate(ctx, *right)
                    }
                }
                _ => {
                    let lval = self.evaluate(ctx, *left)?;
                    let rval = self.evaluate(ctx, *right)?;
                    self.evaluate_binop(op, lval, rval)
                }
            },
            Node::MemberExpression(base, property) => {
                let base = self.evaluate(ctx, *base)?;
                let base = self.to_object(base)?;
                get(&base, &ObjectKey::from(property))
            }
            Node::ComputedMemberExpression(base, property) => {
                let base = self.evaluate(ctx, *base)?;
                let base = self.to_object(base)?;
                let property = self.evaluate(ctx, *property)?.to_object_key()?;
                get(&base, &property)
            }
            Node::LexicalDeclaration(name, value, _) => {
                let value = self.evaluate(ctx, *value)?;
                ctx.environment.borrow_mut().set(name.as_str(), value)?;
                Ok(Value::Null)
            }
            Node::FunctionDeclaration(name, args, body) => {
                let value = new_function(self, args, body, ctx.environment.clone());
                ctx.environment.borrow_mut().set(name.as_str(), value)?;
                Ok(Value::Null)
            }
            Node::FunctionExpression(_name, args, body) => {
                Ok(new_function(self, args, body, ctx.environment.clone()))
            }
            Node::Identifier(name) => ctx.environment.borrow().get(name.as_str()),
            Node::PropertyInitializer(_, _) => unreachable!(),
            Node::NewExpression(expr) => {
                let mut constructor;
                let mut values;
                match *expr {
                    Node::CallExpression(callee, args) => {
                        constructor = self.evaluate(ctx, *callee)?;
                        values = Vec::new();
                        for arg in args {
                            values.push(self.evaluate(ctx, arg.clone())?);
                        }
                    }
                    _ => {
                        constructor = self.evaluate(ctx, *expr)?;
                        values = vec![];
                    }
                }
                construct(self, constructor, values)
            }
            Node::ImportDeclaration(_)
            | Node::ImportDefaultDeclaration(_, _)
            | Node::ImportNamedDeclaration(_, _)
            | Node::ImportStandardDeclaration(_, _) => Ok(Value::Null),
            Node::ExportDeclaration(decl) => match *decl {
                Node::FunctionDeclaration(_, _, _) => Ok(Value::Null),
                _ => self.evaluate(ctx, *decl),
            },
        }
    }

    fn evaluate_statement_list(
        &self,
        ctx: &mut ExecutionContext,
        nodes: Vec<Node>,
    ) -> Result<Value, Value> {
        let mut result = Value::Null;
        for node in &nodes {
            // hoist declarations in block for TDZ
            match node {
                Node::LexicalDeclaration(name, _, mutable) => {
                    ctx.environment
                        .borrow_mut()
                        .create(name.as_str(), *mutable)?;
                }
                Node::FunctionDeclaration(name, _, _) => {
                    ctx.environment.borrow_mut().create(name.as_str(), false)?;
                }
                _ => {}
            }
        }
        for node in nodes {
            result = self.evaluate(ctx, node)?;
        }
        Ok(result)
    }

    fn evaluate_unop(&self, op: Operator, value: Value) -> Result<Value, Value> {
        match op {
            Operator::Typeof => Ok(Value::String(value.type_of().to_string())),
            Operator::Void => Ok(Value::Null),
            Operator::Not => Ok(if value.is_truthy() {
                Value::False
            } else {
                Value::True
            }),
            Operator::Sub => {
                if let Value::Float(num) = value {
                    Ok(Value::Float(-num))
                } else {
                    Err(new_error("invalid float"))
                }
            }
            _ => Err(new_error("unsupported op")),
        }
    }

    fn evaluate_binop(&self, op: Operator, left: Value, right: Value) -> Result<Value, Value> {
        macro_rules! f64_binop_f64 {
            ($fn:expr) => {
                match left {
                    Value::Float(lnum) => match right {
                        Value::Float(rnum) => Ok(Value::Float($fn(lnum, rnum))),
                        _ => Err(new_error("rval must be a float")),
                    },
                    _ => Err(new_error("lval must be a float")),
                }
            };
        }

        macro_rules! f64_binop_bool {
            ($fn:expr) => {
                match left {
                    Value::Float(lnum) => match right {
                        Value::Float(rnum) => Ok(if $fn(&lnum, &rnum) {
                            Value::True
                        } else {
                            Value::False
                        }),
                        _ => Err(new_error("rval must be a float")),
                    },
                    _ => Err(new_error("lval must be a float")),
                }
            };
        }

        match op {
            Operator::Pow => f64_binop_f64!(f64::powf),
            Operator::Mul => f64_binop_f64!(f64::mul),
            Operator::Div => f64_binop_f64!(f64::div),
            Operator::Mod => f64_binop_f64!(f64::rem),
            Operator::Add => match &left {
                Value::Float(lnum) => match right {
                    Value::Float(rnum) => Ok(Value::Float(lnum + rnum)),
                    _ => Err(new_error("rval must be a float")),
                },
                Value::String(lstr) => match &right {
                    Value::String(rstr) => Ok(Value::String(format!("{}{}", lstr, rstr))),
                    _ => Err(new_error("rval must be a string")),
                },
                _ => Err(new_error("lval must be a float or string")),
            },
            Operator::Sub => f64_binop_f64!(f64::sub),
            Operator::LeftShift => {
                f64_binop_f64!(|a: f64, b: f64| ((a.round() as i64) << b.round() as i64) as f64)
            }
            Operator::RightShift => {
                f64_binop_f64!(|a: f64, b: f64| (a.round() as i64 >> b.round() as i64) as f64)
            }
            Operator::LessThan => f64_binop_bool!(f64::lt),
            Operator::GreaterThan => f64_binop_bool!(f64::gt),
            Operator::LessThanOrEqual => f64_binop_bool!(f64::le),
            Operator::GreaterThanOrEqual => f64_binop_bool!(f64::ge),
            Operator::Equal => Ok(if left == right {
                Value::True
            } else {
                Value::False
            }),
            Operator::NotEqual => Ok(if left == right {
                Value::False
            } else {
                Value::True
            }),
            Operator::BitwiseAND => {
                f64_binop_f64!(|a: f64, b: f64| (a.round() as i64 & b.round() as i64) as f64)
            }
            Operator::BitwiseXOR => {
                f64_binop_f64!(|a: f64, b: f64| (a.round() as i64 ^ b.round() as i64) as f64)
            }
            Operator::BitwiseOR => {
                f64_binop_f64!(|a: f64, b: f64| (a.round() as i64 | b.round() as i64) as f64)
            }
            _ => Err(new_error("unsupported op")),
        }
    }

    fn to_object(&self, value: Value) -> Result<Value, Value> {
        match &value {
            Value::Null => Err(new_error("cannot convert null to object")),
            Value::True => Ok(new_boolean_object(self, true)),
            Value::False => Ok(new_boolean_object(self, false)),
            Value::Object(_) => Ok(value),
            Value::Float(n) => Ok(new_float_object(self, *n)),
            Value::String(s) => Ok(new_string_object(self, s.clone())),
            _ => unreachable!(),
        }
    }
}
