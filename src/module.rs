use crate::intrinsics::{
    create_boolean_prototype, create_function_prototype, create_number_prototype,
    create_object_prototype, create_string_prototype,
};
use crate::parser::{Node, Operator, Parser};
use crate::value::{
    new_boolean_object, new_error, new_function, new_number_object, new_object, new_string_object,
    BuiltinFunction, ObjectKind, Value,
};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::ops::{Div, Mul, Rem, Sub};

#[derive(Debug)]
struct Binding {
    value: Option<Value>,
    mutable: bool,
    exported: bool,
}

#[derive(Debug)]
struct LexicalEnvironment {
    parent: Option<*mut LexicalEnvironment>,
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
            match self.parent {
                None => false,
                Some(r) => unsafe { (*r).has(name) },
            }
        }
    }

    fn create_binding(&mut self, name: &str, mutable: bool, exported: bool) -> Result<(), Value> {
        if self.has(name) {
            Err(new_error("binding already declared"))
        } else {
            self.bindings.insert(
                name.to_string(),
                Binding {
                    value: None,
                    mutable,
                    exported,
                },
            );
            Ok(())
        }
    }

    fn create(&mut self, name: &str, mutable: bool) -> Result<(), Value> {
        self.create_binding(name, mutable, false)
    }

    fn create_export(&mut self, name: &str, mutable: bool) -> Result<(), Value> {
        self.create_binding(name, mutable, true)
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
            _ => match self.parent {
                Some(r) => unsafe { (*r).set(name, value) },
                _ => Err(new_error("reference error")),
            },
        }
    }

    fn get(&self, name: &str) -> Result<Value, Value> {
        match self.bindings.get(name) {
            Some(Binding { value: Some(v), .. }) => Ok(v.clone()),
            Some(Binding { value: None, .. }) => Err(new_error("reference error")),
            _ => match self.parent {
                Some(r) => unsafe { (*r).get(name) },
                _ => Err(new_error("reference error")),
            },
        }
    }
}

#[derive(Debug)]
struct ExecutionContext {
    pub this: Option<Value>,
    pub environment: LexicalEnvironment,
}

#[derive(Debug)]
struct ModuleX {
    filename: String,
    ast: Node,
    context: ExecutionContext,
    imports: HashSet<String>,
}

impl ModuleX {
    fn new(filename: &str, agent: &Agent) -> Result<ModuleX, Value> {
        let source = std::fs::read_to_string(filename).expect("no such file");
        let mut module = ModuleX {
            filename: filename.to_string(),
            context: ExecutionContext {
                this: None,
                environment: LexicalEnvironment::new(),
            },
            ast: Parser::parse(&source)?,
            imports: HashSet::new(),
        };

        if let Node::StatementList(list) = &module.ast {
            for node in list {
                match &node {
                    Node::ImportDefaultDeclaration(specifier, name) => {
                        module.context.environment.create(name, false)?;
                        module.imports.insert(specifier.to_string());
                    }
                    Node::ImportNamedDeclaration(specifier, names)
                    | Node::ImportStandardDeclaration(specifier, names) => {
                        for name in names {
                            module.context.environment.create(name, false)?;
                            module.imports.insert(specifier.to_string());
                        }
                    }
                    Node::ExportDeclaration(decl) => match *decl.clone() {
                        Node::LexicalDeclaration(name, _, mutable) => {
                            module
                                .context
                                .environment
                                .create_export(name.as_str(), mutable)?;
                        }
                        Node::FunctionDeclaration(name, args, body) => {
                            module
                                .context
                                .environment
                                .create_export(name.as_str(), false)?;
                            let value = new_function(
                                args,
                                body,
                                agent.intrinsics.function_prototype.clone(),
                            );
                            module.context.environment.set(name.as_str(), value)?;
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
    pub function_prototype: Value,
    pub boolean_prototype: Value,
    pub string_prototype: Value,
    pub number_prototype: Value,
}

#[derive(Debug)]
pub struct Agent {
    pub intrinsics: Intrinsics,
    modules: RefCell<HashMap<String, ModuleX>>,
}

impl Agent {
    pub fn new() -> Agent {
        let object_prototype = create_object_prototype();
        let function_prototype = create_function_prototype(object_prototype.clone());
        let boolean_prototype = create_boolean_prototype(object_prototype.clone());
        let number_prototype = create_number_prototype(object_prototype.clone());
        let string_prototype = create_string_prototype(object_prototype.clone());
        Agent {
            intrinsics: Intrinsics {
                object_prototype,
                function_prototype,
                boolean_prototype,
                number_prototype,
                string_prototype,
            },
            modules: RefCell::new(HashMap::new()),
        }
    }

    fn load(&self, specifier: &str, referrer: &str) -> Result<String, Value> {
        let filename = std::path::Path::new(referrer)
            .parent()
            .unwrap()
            .join(specifier);
        let filename = filename.to_str().unwrap();
        if !self.modules.borrow().contains_key(filename) {
            let module = ModuleX::new(filename, self)?;
            let imports = module.imports.clone();
            self.modules
                .borrow_mut()
                .insert(filename.to_string(), module);
            for import in imports {
                self.load(import.as_str(), filename)?;
            }
        }
        Ok(filename.to_string())
    }

    pub fn import(&self, specifier: &str, referrer: &str) -> Result<(), Value> {
        let filename = self.load(specifier, referrer)?;
        let mut map = self.modules.borrow_mut();
        let module = map.get_mut(&filename).unwrap();
        self.evaluate(&mut module.context, module.ast.clone())?;
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
                let new = LexicalEnvironment::new();
                let mut old = std::mem::replace(&mut ctx.environment, new);
                ctx.environment.parent = Some(&mut old);
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
                        let new = LexicalEnvironment::new();
                        let mut old = std::mem::replace(&mut ctx.environment, new);
                        ctx.environment.parent = Some(&mut old);
                        ctx.environment.create(binding.as_str(), false)?;
                        ctx.environment.set(binding.as_str(), e)?;
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
                        ObjectKind::Function(params, body) => {
                            let mut new_ctx = ExecutionContext {
                                this: Some(this),
                                environment: LexicalEnvironment::new(),
                            };
                            for (i, arg) in args.iter().enumerate() {
                                let binding = params[i].clone();
                                let value = self.evaluate(&mut new_ctx, (*arg).clone())?;
                                ctx.environment.create(binding.as_str(), false)?;
                                ctx.environment.set(binding.as_str(), value)?;
                            }
                            let r = self.evaluate(&mut new_ctx, *body.clone());
                            match &r {
                                Err(Value::ReturnCompletion(v)) => Ok(*v.clone()),
                                _ => r,
                            }
                        }
                        ObjectKind::BuiltinFunction(BuiltinFunction(bfn)) => {
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
                        ctx.environment.set(name.as_str(), rval.clone())?;
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
                ctx.environment.set(name.as_str(), value)?;
                Ok(Value::Null)
            }
            Node::FunctionDeclaration(name, args, body) => {
                let value = new_function(args, body, self.intrinsics.function_prototype.clone());
                ctx.environment.set(name.as_str(), value)?;
                Ok(Value::Null)
            }
            Node::FunctionExpression(_name, args, body) => Ok(new_function(
                args,
                body,
                self.intrinsics.function_prototype.clone(),
            )),
            Node::Identifier(name) => ctx.environment.get(name.as_str()),
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
                    ctx.environment.create(name.as_str(), *mutable)?;
                }
                Node::FunctionDeclaration(name, _, _) => {
                    ctx.environment.create(name.as_str(), false)?;
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
