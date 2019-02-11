use crate::intrinsics::{
    create_boolean_prototype, create_function_prototype, create_number_prototype,
    create_object_prototype, create_string_prototype,
};
use crate::parser::{Node, Operator, Parser};
use crate::value::{
    new_boolean_object, new_error, new_function, new_number_object, new_object, new_string_object,
    BuiltinFunction, ObjectKind, Value,
};
use std::collections::HashMap;
use std::ops::{Div, Mul, Rem, Sub};

#[derive(Debug)]
struct Binding {
    value: Option<Value>,
    mutable: bool,
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

    fn create(&mut self, name: &str, mutable: bool) -> Result<(), Value> {
        if self.has(name) {
            Err(new_error("binding already declared"))
        } else {
            self.bindings.insert(
                name.to_string(),
                Binding {
                    value: None,
                    mutable,
                },
            );
            Ok(())
        }
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
pub struct Module<'a> {
    agent: &'a mut Agent,
    filename: String,
    ast: Node,
    context: ExecutionContext,
}

impl<'a> Module<'a> {
    pub fn new(agent: &'a mut Agent, filename: &str) -> Result<Module<'a>, Value> {
        let source = std::fs::read_to_string(filename).expect("no such file");
        let mut module = Module {
            agent,
            filename: filename.to_string(),
            context: ExecutionContext {
                this: None,
                environment: LexicalEnvironment::new(),
            },
            ast: Parser::parse(&source)?,
        };

        if let Node::StatementList(list) = &module.ast {
            for node in list {
                match &node {
                    Node::ImportDefaultDeclaration(_, name) => {
                        module.context.environment.create(name, false)?;
                    }
                    Node::ImportNamedDeclaration(_, names)
                    | Node::ImportStandardDeclaration(_, names) => {
                        for name in names {
                            module.context.environment.create(name, false)?;
                        }
                    }
                    _ => {}
                }
            }
        }

        Ok(module)
    }

    pub fn evaluate(&mut self) -> Result<Value, Value> {
        self.agent.execution_context_stack.push(&mut self.context);
        let r = self.agent.evaluate(self.ast.clone());
        self.agent.execution_context_stack.pop(); // TODO: assert this is the same context
        r
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
    execution_context_stack: Vec<*mut ExecutionContext>,
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
            execution_context_stack: Vec::new(),
        }
    }

    fn context(&mut self) -> &mut ExecutionContext {
        unsafe {
            let r = self.execution_context_stack.last_mut().expect("no ctx");
            &mut **r
        }
    }

    fn evaluate(&mut self, node: Node) -> Result<Value, Value> {
        match node {
            Node::ReturnStatement(expr) => {
                Err(Value::ReturnCompletion(Box::new(self.evaluate(*expr)?)))
            }
            Node::ThrowStatement(expr) => Err(self.evaluate(*expr)?),
            Node::ThisExpression => {
                let context = self.context();
                match &context.this {
                    None => Err(new_error("invalid this")),
                    Some(v) => Ok(v.clone()),
                }
            }
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
                                    let value = self.evaluate(*expr)?;
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
            Node::StatementList(nodes) => self.evaluate_statement_list(nodes),
            Node::BlockStatement(nodes) => {
                let ctx = self.context();
                let new = LexicalEnvironment::new();
                let mut old = std::mem::replace(&mut ctx.environment, new);
                ctx.environment.parent = Some(&mut old);
                let result = self.evaluate_statement_list(nodes);
                let ctx = self.context();
                std::mem::replace(&mut ctx.environment, old);
                result
            }
            Node::TryStatement(try_clause, catch_clause) => {
                let result = self.evaluate(*try_clause);
                match result {
                    Ok(v) => Ok(v),
                    Err(Value::ReturnCompletion(_)) => result,
                    Err(_) => self.evaluate(*catch_clause),
                }
            }
            Node::BoundTryStatement(try_clause, binding, catch_clause) => {
                let result = self.evaluate(*try_clause);
                match result {
                    Ok(v) => Ok(v),
                    Err(Value::ReturnCompletion(_)) => result,
                    Err(e) => {
                        let ctx = self.context();
                        let new = LexicalEnvironment::new();
                        let mut old = std::mem::replace(&mut ctx.environment, new);
                        ctx.environment.parent = Some(&mut old);
                        ctx.environment.create(binding.as_str(), false)?;
                        ctx.environment.set(binding.as_str(), e)?;
                        let r = self.evaluate(*catch_clause);
                        let ctx = self.context();
                        std::mem::replace(&mut ctx.environment, old);
                        r
                    }
                }
            }
            Node::IfStatement(test, consequent) => {
                if self.evaluate(*test)?.is_truthy() {
                    self.evaluate(*consequent)
                } else {
                    Ok(Value::Null)
                }
            }
            Node::IfElseStatement(test, consequent, alternative) => {
                if self.evaluate(*test)?.is_truthy() {
                    self.evaluate(*consequent)
                } else {
                    self.evaluate(*alternative)
                }
            }
            Node::ConditionalExpression(test, consequent, alternative) => {
                if self.evaluate(*test)?.is_truthy() {
                    self.evaluate(*consequent)
                } else {
                    self.evaluate(*alternative)
                }
            }
            Node::ExpressionStatement(expr) => self.evaluate(*expr),
            Node::CallExpression(name, args) => {
                let this;
                let val;
                match *name.clone() {
                    Node::MemberExpression(base, property) => {
                        let base = self.evaluate(*base)?;
                        this = self.to_object(base)?;
                        val = match &this {
                            Value::Object(o) => o.get(property),
                            _ => Err(new_error("invalid left hand side")),
                        }?;
                    }
                    Node::ComputedMemberExpression(base, property) => {
                        let base = self.evaluate(*base)?;
                        this = self.to_object(base)?;
                        let property = self.evaluate(*property)?.to_string()?;
                        val = match &this {
                            Value::Object(o) => o.get(property),
                            _ => Err(new_error("invalid left hand side")),
                        }?;
                    }
                    _ => {
                        this = Value::Null;
                        val = self.evaluate(*name)?;
                    }
                };
                match &val {
                    Value::Object(o) => match &o.kind {
                        ObjectKind::Function(params, body) => {
                            let mut ctx = ExecutionContext {
                                this: Some(this),
                                environment: LexicalEnvironment::new(),
                            };
                            self.execution_context_stack.push(&mut ctx);
                            for (i, arg) in args.iter().enumerate() {
                                let binding = params[i].clone();
                                let value = self.evaluate((*arg).clone())?;
                                let ctx = self.context();
                                ctx.environment.create(binding.as_str(), false)?;
                                ctx.environment.set(binding.as_str(), value)?;
                            }
                            let r = self.evaluate(*body.clone());
                            self.execution_context_stack.pop();
                            match &r {
                                Err(Value::ReturnCompletion(v)) => Ok(*v.clone()),
                                _ => r,
                            }
                        }
                        ObjectKind::BuiltinFunction(BuiltinFunction(bfn)) => {
                            let mut values = Vec::new();
                            for arg in args {
                                values.push(self.evaluate(arg)?);
                            }
                            bfn(self, values)
                        }
                        _ => Err(new_error("not a function")),
                    },
                    _ => Err(new_error("not a function")),
                }
            }
            Node::UnaryExpression(op, expr) => {
                let value = self.evaluate(*expr)?;
                self.evaluate_unop(op, value)
            }
            Node::BinaryExpression(left, op, right) => match op {
                Operator::Assign => match *left {
                    Node::MemberExpression(base, property) => {
                        let base = self.evaluate(*base)?;
                        let base = self.to_object(base)?;
                        match &base {
                            Value::Object(o) => {
                                let rval = self.evaluate(*right)?;
                                o.set(property, rval, o.clone())
                            }
                            _ => Err(new_error("invalid left hand side")),
                        }
                    }
                    Node::ComputedMemberExpression(base, property) => {
                        let base = self.evaluate(*base)?;
                        let base = self.to_object(base)?;
                        let key = self.evaluate(*property)?.to_string()?;
                        match &base {
                            Value::Object(o) => {
                                let rval = self.evaluate(*right)?;
                                o.set(key, rval, o.clone())
                            }
                            _ => Err(new_error("invalid left hand side")),
                        }
                    }
                    Node::Identifier(name) => {
                        let rval = self.evaluate(*right)?;
                        let ctx = self.context();
                        ctx.environment.set(name.as_str(), rval.clone())?;
                        Ok(rval)
                    }
                    _ => Err(new_error("invalid left hand side")),
                },
                Operator::LogicalAND => {
                    let lval = self.evaluate(*left)?;
                    if lval.is_truthy() {
                        self.evaluate(*right)
                    } else {
                        Ok(lval)
                    }
                }
                Operator::LogicalOR => {
                    let lval = self.evaluate(*left)?;
                    if lval.is_truthy() {
                        Ok(lval)
                    } else {
                        self.evaluate(*right)
                    }
                }
                _ => {
                    let lval = self.evaluate(*left)?;
                    let rval = self.evaluate(*right)?;
                    self.evaluate_binop(op, lval, rval)
                }
            },
            Node::MemberExpression(base, property) => {
                let base = self.evaluate(*base)?;
                let base = self.to_object(base)?;
                match &base {
                    Value::Object(o) => o.get(property),
                    _ => Err(new_error("member expression base must be object")),
                }
            }
            Node::ComputedMemberExpression(base, property) => {
                let base = self.evaluate(*base)?;
                let base = self.to_object(base)?;
                let property = self.evaluate(*property)?.to_string()?;
                match &base {
                    Value::Object(o) => o.get(property),
                    _ => Err(new_error("member expression base must be object")),
                }
            }
            Node::LexicalDeclaration(name, value, _) => {
                let value = self.evaluate(*value)?;
                let ctx = self.context();
                ctx.environment.set(name.as_str(), value)?;
                Ok(Value::Null)
            }
            Node::FunctionDeclaration(name, args, body) => {
                let value = new_function(args, body, self.intrinsics.function_prototype.clone());
                let context = self.context();
                context.environment.set(name.as_str(), value)?;
                Ok(Value::Null)
            }
            Node::FunctionExpression(_name, args, body) => Ok(new_function(
                args,
                body,
                self.intrinsics.function_prototype.clone(),
            )),
            Node::Identifier(name) => {
                let ctx = self.context();
                ctx.environment.get(name.as_str())
            }
            Node::PropertyInitializer(_, _) => unreachable!(),
            Node::NewExpression(_) => unreachable!(),
            Node::ImportDeclaration(_) => unreachable!(),
            Node::ImportDefaultDeclaration(_, _) => unreachable!(),
            Node::ImportNamedDeclaration(_, _) => unreachable!(),
            Node::ImportStandardDeclaration(_, _) => unreachable!(),
            Node::ExportDeclaration(_) => unreachable!(),
        }
    }

    fn evaluate_statement_list(&mut self, nodes: Vec<Node>) -> Result<Value, Value> {
        let mut result = Value::Null;
        for node in &nodes {
            // hoist declarations in block for TDZ
            let context = self.context();
            match node {
                Node::LexicalDeclaration(name, _, mutable) => {
                    context.environment.create(name.as_str(), *mutable)?;
                }
                Node::FunctionDeclaration(name, _, _) => {
                    context.environment.create(name.as_str(), false)?;
                }
                _ => {}
            }
        }
        for node in nodes {
            result = self.evaluate(node)?;
        }
        Ok(result)
    }

    fn evaluate_unop(&mut self, op: Operator, value: Value) -> Result<Value, Value> {
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

    fn evaluate_binop(&mut self, op: Operator, left: Value, right: Value) -> Result<Value, Value> {
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
