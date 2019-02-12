use crate::intrinsics::{
    create_array_prototype, create_boolean_prototype, create_function_prototype,
    create_number_prototype, create_object_prototype, create_string_prototype,
};
use crate::parser::{Node, Operator, Parser};
use crate::value::{
    new_array, new_boolean_object, new_builtin_function, new_error, new_function,
    new_number_object, new_object, new_string_object, BuiltinFunctionWrap, ObjectKind, Value,
};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::ops::{Div, Mul, Rem, Sub};
use std::rc::Rc;

#[derive(Debug)]
struct Binding {
    value: Option<Value>,
    mutable: bool,
    exported: bool,
    module: Option<Module>,
}

#[derive(Debug)]
pub struct LexicalEnvironment {
    parent: Option<Rc<RefCell<LexicalEnvironment>>>,
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

#[derive(Debug)]
struct ExecutionContext {
    pub this: Option<Value>,
    pub environment: Rc<RefCell<LexicalEnvironment>>,
}

#[derive(Debug, PartialEq, Clone)]
enum ModuleStatus {
    Uninstantiated,
    Instantiating,
    Instantiated,
    Evaluating,
    Evaluated,
}

#[derive(Debug)]
struct ModuleX {
    filename: String,
    ast: Node,
    context: ExecutionContext,
    imports: HashSet<String>,
    status: ModuleStatus,
    dfs_index: u32,
    dfs_ancestor_index: u32,
}

type Module = Rc<RefCell<ModuleX>>;

fn print(_: &Agent, args: Vec<Value>) -> Result<Value, Value> {
    for arg in args {
        print!("{:?} ", arg);
    }
    print!("\n");
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
                environment: Rc::new(RefCell::new(LexicalEnvironment::new())),
            },
            ast: Parser::parse(&source)?,
            imports: HashSet::new(),
            status: ModuleStatus::Uninstantiated,
            dfs_index: 0,
            dfs_ancestor_index: 0,
        };

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
                    Node::ImportStandardDeclaration(specifier, names) => {
                        if specifier == "debug" {
                            for name in names {
                                if name == "print" {
                                    module
                                        .context
                                        .environment
                                        .borrow_mut()
                                        .create(name, false)?;
                                    module
                                        .context
                                        .environment
                                        .borrow_mut()
                                        .set(name, new_builtin_function(agent, print))?;
                                } else {
                                    return Err(new_error("invalid standard module"));
                                }
                            }
                        } else {
                            return Err(new_error("invalid standard module"));
                        }
                    }
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

#[derive(Debug)]
pub struct Intrinsics {
    pub object_prototype: Value,
    pub array_prototype: Value,
    pub function_prototype: Value,
    pub boolean_prototype: Value,
    pub string_prototype: Value,
    pub number_prototype: Value,
}

#[derive(Debug)]
pub struct Agent {
    pub intrinsics: Intrinsics,
    modules: RefCell<HashMap<String, Module>>,
}

impl Agent {
    pub fn new() -> Agent {
        let object_prototype = create_object_prototype();
        let array_prototype = create_array_prototype(object_prototype.clone());
        let function_prototype = create_function_prototype(object_prototype.clone());
        let boolean_prototype = create_boolean_prototype(object_prototype.clone());
        let number_prototype = create_number_prototype(object_prototype.clone());
        let string_prototype = create_string_prototype(object_prototype.clone());
        Agent {
            intrinsics: Intrinsics {
                object_prototype,
                array_prototype,
                function_prototype,
                boolean_prototype,
                number_prototype,
                string_prototype,
            },
            modules: RefCell::new(HashMap::new()),
        }
    }

    fn load(&self, specifier: &str, referrer: &str) -> Result<Module, Value> {
        let filename = std::path::Path::new(referrer)
            .parent()
            .unwrap()
            .join(specifier);
        let filename = filename.to_str().unwrap();
        if !self.modules.borrow().contains_key(filename) {
            let module = Rc::new(RefCell::new(ModuleX::new(filename, self)?));
            // let imports = module.imports.clone();
            self.modules
                .borrow_mut()
                .insert(filename.to_string(), module.clone());
            Ok(module)
        // for import in imports {
        //     self.load(import.as_str(), filename)?;
        // }
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
            Node::NumberLiteral(n) => Ok(Value::Number(n)),
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
                            o.set(len.to_string(), value, o.clone())?;
                            len += 1;
                        }
                        o.set("length".to_string(), Value::Number(f64::from(len)), o.clone())?;
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
                                    o.set(key, value, o.clone())?;
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
                let new = Rc::new(RefCell::new(LexicalEnvironment::new()));
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
                        let new = Rc::new(RefCell::new(LexicalEnvironment::new()));
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
                        val = match &this {
                            Value::Object(o) => o.get(property),
                            _ => Err(new_error("invalid left hand side")),
                        }?;
                    }
                    Node::ComputedMemberExpression(base, property) => {
                        let base = self.evaluate(ctx, *base)?;
                        this = self.to_object(base)?;
                        let property = self.evaluate(ctx, *property)?.to_string()?;
                        val = match &this {
                            Value::Object(o) => o.get(property),
                            _ => Err(new_error("invalid left hand side")),
                        }?;
                    }
                    _ => {
                        this = Value::Null;
                        val = self.evaluate(ctx, *name)?;
                    }
                };
                match &val {
                    Value::Object(o) => match &o.kind {
                        ObjectKind::Function(params, body, parent_env) => {
                            let mut new_ctx = ExecutionContext {
                                this: Some(this),
                                environment: Rc::new(RefCell::new(LexicalEnvironment::new())),
                            };
                            new_ctx.environment.borrow_mut().parent = Some(parent_env.clone());
                            for (i, arg) in args.iter().enumerate() {
                                let binding = params[i].clone();
                                let value = self.evaluate(ctx, (*arg).clone())?;
                                new_ctx
                                    .environment
                                    .borrow_mut()
                                    .create(binding.as_str(), false)?;
                                new_ctx
                                    .environment
                                    .borrow_mut()
                                    .set(binding.as_str(), value)?;
                            }
                            let r = self.evaluate(&mut new_ctx, *body.clone());
                            match &r {
                                Err(Value::ReturnCompletion(v)) => Ok(*v.clone()),
                                _ => r,
                            }
                        }
                        ObjectKind::BuiltinFunction(BuiltinFunctionWrap(bfn)) => {
                            let mut values = Vec::new();
                            for arg in args {
                                values.push(self.evaluate(ctx, arg)?);
                            }
                            bfn(self, values)
                        }
                        _ => Err(new_error("not a function")),
                    },
                    _ => Err(new_error("not a function")),
                }
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
                        match &base {
                            Value::Object(o) => {
                                let rval = self.evaluate(ctx, *right)?;
                                o.set(property, rval, o.clone())
                            }
                            _ => Err(new_error("invalid left hand side")),
                        }
                    }
                    Node::ComputedMemberExpression(base, property) => {
                        let base = self.evaluate(ctx, *base)?;
                        let base = self.to_object(base)?;
                        let key = self.evaluate(ctx, *property)?.to_string()?;
                        match &base {
                            Value::Object(o) => {
                                let rval = self.evaluate(ctx, *right)?;
                                o.set(key, rval, o.clone())
                            }
                            _ => Err(new_error("invalid left hand side")),
                        }
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
                match &base {
                    Value::Object(o) => o.get(property),
                    _ => Err(new_error("member expression base must be object")),
                }
            }
            Node::ComputedMemberExpression(base, property) => {
                let base = self.evaluate(ctx, *base)?;
                let base = self.to_object(base)?;
                let property = self.evaluate(ctx, *property)?.to_string()?;
                match &base {
                    Value::Object(o) => o.get(property),
                    _ => Err(new_error("member expression base must be object")),
                }
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
            Node::NewExpression(_) => unreachable!(),
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
                if let Value::Number(num) = value {
                    Ok(Value::Number(-num))
                } else {
                    Err(new_error("invalid number"))
                }
            }
            _ => Err(new_error("unsupported op")),
        }
    }

    fn evaluate_binop(&self, op: Operator, left: Value, right: Value) -> Result<Value, Value> {
        macro_rules! f64_binop_f64 {
            ($fn:expr) => {
                match left {
                    Value::Number(lnum) => match right {
                        Value::Number(rnum) => Ok(Value::Number($fn(lnum, rnum))),
                        _ => Err(new_error("rval must be a number")),
                    },
                    _ => Err(new_error("lval must be a number")),
                }
            };
        }

        macro_rules! f64_binop_bool {
            ($fn:expr) => {
                match left {
                    Value::Number(lnum) => match right {
                        Value::Number(rnum) => Ok(if $fn(&lnum, &rnum) {
                            Value::True
                        } else {
                            Value::False
                        }),
                        _ => Err(new_error("rval must be a number")),
                    },
                    _ => Err(new_error("lval must be a number")),
                }
            };
        }

        match op {
            Operator::Pow => f64_binop_f64!(f64::powf),
            Operator::Mul => f64_binop_f64!(f64::mul),
            Operator::Div => f64_binop_f64!(f64::div),
            Operator::Mod => f64_binop_f64!(f64::rem),
            Operator::Add => match &left {
                Value::Number(lnum) => match right {
                    Value::Number(rnum) => Ok(Value::Number(lnum + rnum)),
                    _ => Err(new_error("rval must be a number")),
                },
                Value::String(lstr) => match &right {
                    Value::String(rstr) => Ok(Value::String(format!("{}{}", lstr, rstr))),
                    _ => Err(new_error("rval must be a string")),
                },
                _ => Err(new_error("lval must be a number or string")),
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
            Value::Number(n) => Ok(new_number_object(self, *n)),
            Value::String(s) => Ok(new_string_object(self, s.clone())),
            _ => unreachable!(),
        }
    }
}
