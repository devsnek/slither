use crate::parser::{Node, Operator};
use byteorder::{LittleEndian, WriteBytesExt};
use rust_decimal::Decimal;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Error;

#[repr(u8)]
#[derive(PartialEq, Debug)]
pub enum Op {
    PushScope,
    PopScope,
    End,
    NewNumber,
    NewString,
    NewSymbol,
    NewRegex,
    ProcessTemplateLiteral,
    NewFunction,
    NewObject,
    NewArray,
    NewIdentifier,
    NewMemberReference,
    NewComputedMemberReference,
    NewMemberReferenceNoConsumeStack,
    NewComputedMemberReferenceNoConsumeStack,
    PushNull,
    PushTrue,
    PushFalse,
    SetValue,
    GetValue,
    DropValue,
    GetThis,
    LexicalDeclaration,
    LexicalInitialization,
    Jump,
    JumpIfFalse,
    JumpIfFalseNoConsume,
    Call,
    TailCall,
    InitReplace,
    New,
    NewWithArgs,
    Return,
    Throw,
    ExceptionToStack,
    PushTry,
    PopTry,
    PushLoop,
    PopLoop,
    Break,
    Continue,
    Add,
    Sub,
    UnarySub,
    Mul,
    Div,
    Mod,
    Pow,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Eq,
    Ne,
    Typeof,
    Not,
}

impl From<u8> for Op {
    fn from(n: u8) -> Op {
        unsafe { std::mem::transmute::<u8, Op>(n) }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Compiled {
    pub string_table: Vec<String>,
    pub number_table: Vec<Decimal>,
    pub code: Vec<u8>,
}

struct Jump {
    index: Option<usize>,
    targets: Vec<usize>,
}

impl Drop for Jump {
    fn drop(&mut self) {
        if self.index == None {
            panic!("jump was not marked");
        }
    }
}

macro_rules! label {
    ($name:ident) => {
        let mut $name = Jump {
            index: None,
            targets: Vec::new(),
        };
    };
}

macro_rules! mark {
    ($self:ident, $name:ident) => {{
        let pos = $self.code.len();
        $name.index = Some(pos);
        for index in &$name.targets {
            $self.code[*index] = ((pos >> 0) & 0xff) as u8;
            $self.code[index + 1] = ((pos >> 8) & 0xff) as u8;
            $self.code[index + 2] = ((pos >> 16) & 0xff) as u8;
            $self.code[index + 3] = ((pos >> 24) & 0xff) as u8;
        }
    }};
}

macro_rules! jmp {
    ($self:ident, $name:ident) => {{
        if let Some(index) = $name.index {
            $self.push_i32(index as i32);
        } else {
            let target = $self.code.len();
            $self.push_i32(0);
            $name.targets.push(target);
        }
    }};
}

macro_rules! jump {
    ($self:ident, $name:ident) => {
        $self.push_op(Op::Jump);
        jmp!($self, $name);
    };
}

macro_rules! jump_if_false {
    ($self:ident, $name:ident) => {
        $self.push_op(Op::JumpIfFalse);
        jmp!($self, $name);
    };
}

macro_rules! jump_if_false_no_consume {
    ($self:ident, $name:ident) => {
        $self.push_op(Op::JumpIfFalseNoConsume);
        jmp!($self, $name);
    };
}

pub struct Compiler {
    string_table: Vec<String>,
    number_table: Vec<Decimal>,
    code: Vec<u8>,
}

impl Compiler {
    pub fn go(node: &Node) -> Result<Compiled, Error> {
        let mut compiler = Compiler {
            string_table: Vec::new(),
            number_table: Vec::new(),
            code: Vec::new(),
        };

        compiler.compile(node)?;

        Ok(Compiled {
            code: compiler.code,
            string_table: compiler.string_table,
            number_table: compiler.number_table,
        })
    }

    fn push_op(&mut self, op: Op) {
        self.code.push(op as u8);
    }

    fn string_id(&mut self, string: &str) -> i32 {
        let index = self.string_table.iter().position(|s| s == string);
        match index {
            Some(i) => i as i32,
            None => {
                let id = self.string_table.len();
                self.string_table.push(string.to_string());
                id as i32
            }
        }
    }

    fn number_id(&mut self, number: &Decimal) -> i32 {
        let index = self.number_table.iter().position(|n| n == number);
        match index {
            Some(i) => i as i32,
            None => {
                let id = self.number_table.len();
                self.number_table.push(*number);
                id as i32
            }
        }
    }

    fn push_i32(&mut self, n: i32) {
        self.code.write_i32::<LittleEndian>(n).unwrap();
    }

    fn push_u8(&mut self, n: u8) {
        self.code.push(n);
    }

    fn compile(&mut self, node: &Node) -> Result<(), Error> {
        // println!("compile => {:?}", node);
        match node {
            Node::ExpressionStatement(expr) => {
                self.compile(expr)?;
                self.push_op(Op::GetValue);
                self.push_op(Op::DropValue);
                Ok(())
            }
            Node::ParenthesizedExpression(expr) => {
                self.compile(expr)?;
                self.push_op(Op::GetValue);
                Ok(())
            }
            Node::BlockStatement(nodes, declarations, top) => {
                self.compile_block_statement(nodes, declarations, *top)
            }
            Node::LexicalInitialization(name, val) => {
                self.compile_lexical_initialization(name, val)
            }
            Node::Identifier(name) => self.compile_identifier(name),
            Node::UnaryExpression(op, node) => self.compile_unary_expression(op, node),
            Node::BinaryExpression(left, op, right) => {
                self.compile_binary_expression(left, op, right)
            }
            Node::NumberLiteral(n) => {
                self.push_op(Op::NewNumber);
                let id = self.number_id(n);
                self.push_i32(id);
                Ok(())
            }
            Node::NullLiteral => {
                self.push_op(Op::PushNull);
                Ok(())
            }
            Node::TrueLiteral => {
                self.push_op(Op::PushTrue);
                Ok(())
            }
            Node::FalseLiteral => {
                self.push_op(Op::PushFalse);
                Ok(())
            }
            Node::StringLiteral(s) => {
                self.push_op(Op::NewString);
                let id = self.string_id(s);
                self.push_i32(id);
                Ok(())
            }
            Node::SymbolLiteral(s) => {
                self.push_op(Op::NewSymbol);
                let id = self.string_id(s);
                self.push_i32(id);
                Ok(())
            }
            Node::RegexLiteral(s) => {
                self.push_op(Op::NewRegex);
                let id = self.string_id(s);
                self.push_i32(id);
                Ok(())
            }
            Node::TemplateLiteral(quasis, exprs) => self.compile_template_literal(quasis, exprs),
            Node::ObjectLiteral(inits) => self.compile_object_literal(inits),
            Node::ArrayLiteral(inits) => self.compile_array_literal(inits),
            Node::FunctionDeclaration(name, args, body) => {
                self.compile_function_declaration(name, args, body)
            }
            Node::FunctionExpression(name, args, body) => {
                self.compile_function_expression(name, args, body, false)
            }
            Node::ArrowFunctionExpression(args, body) => {
                self.compile_function_expression(&None, args, body, true)
            }
            Node::CallExpression(callee, args) => self.compile_call_expression(callee, args, false),
            Node::TailCallExpression(callee, args) => {
                self.compile_call_expression(callee, args, true)
            }
            Node::NewExpression(callee) => self.compile_new_expression(callee),
            Node::ReturnStatement(expr) => self.compile_return_statement(expr),
            Node::IfStatement(test, consequent) => self.compile_if_statement(test, consequent),
            Node::IfElseStatement(test, consequent, alternative) => {
                self.compile_if_else_statement(test, consequent, alternative)
            }
            Node::WhileStatement(test, body) => self.compile_while_statement(test, body),
            Node::BreakStatement => self.compile_break_statement(),
            Node::ContinueStatement => self.compile_continue_statement(),
            Node::ThrowStatement(value) => self.compile_throw_statement(value),
            Node::TryStatement(try_clause, binding, catch, finally) => {
                self.compile_try_statement(try_clause, binding, catch, finally)
            }
            Node::MemberExpression(base, property) => {
                self.compile_member_expression(base, property)
            }
            Node::ComputedMemberExpression(base, prop) => {
                self.compile_computed_member_expression(base, prop)
            }
            Node::ThisExpression => {
                self.push_op(Op::GetThis);
                Ok(())
            }
            Node::ImportStandardDeclaration(..) => Ok(()),
            Node::ImportDefaultDeclaration(..) => Ok(()),
            Node::ImportNamedDeclaration(..) => Ok(()),
            Node::ExportDeclaration(decl) => self.compile(decl),
            _ => panic!("Unsupported: {:?}", node),
        }
    }

    fn compile_block_statement(
        &mut self,
        nodes: &[Node],
        declarations: &HashMap<String, bool>,
        top: bool,
    ) -> Result<(), Error> {
        if !top {
            self.push_op(Op::PushScope);
        }
        for (name, mutable) in declarations {
            self.create_lexical_declaration(name, *mutable)?;
        }
        for node in nodes {
            self.compile(node)?;
            if let Node::ReturnStatement(..) = node {
                break;
            }
        }
        if !top {
            self.push_op(Op::PopScope);
        }
        Ok(())
    }

    fn create_lexical_declaration(&mut self, name: &str, mutable: bool) -> Result<(), Error> {
        self.push_op(Op::LexicalDeclaration);
        self.push_u8(mutable as u8);
        let id = self.string_id(&name);
        self.push_i32(id);
        Ok(())
    }

    fn compile_lexical_initialization(&mut self, name: &str, value: &Node) -> Result<(), Error> {
        self.compile(value)?;
        self.push_op(Op::LexicalInitialization);
        let id = self.string_id(name);
        self.push_i32(id);
        Ok(())
    }

    fn compile_identifier(&mut self, name: &str) -> Result<(), Error> {
        self.push_op(Op::NewIdentifier);
        let id = self.string_id(name);
        self.push_i32(id);
        Ok(())
    }

    fn compile_unary_expression(&mut self, op: &Operator, node: &Node) -> Result<(), Error> {
        self.compile(node)?;
        match op {
            Operator::Typeof => self.push_op(Op::Typeof),
            Operator::Sub => self.push_op(Op::UnarySub),
            Operator::Not => self.push_op(Op::Not),
            _ => panic!("{:?}", op),
        };
        Ok(())
    }

    fn compile_binary_expression(
        &mut self,
        left: &Node,
        op: &Operator,
        right: &Node,
    ) -> Result<(), Error> {
        if *op == Operator::LogicalAND {
            label!(end);
            self.compile(left)?;
            jump_if_false_no_consume!(self, end);
            self.push_op(Op::DropValue);
            self.compile(right)?;
            mark!(self, end);
            return Ok(());
        }
        self.compile(left)?;
        self.compile(right)?;
        match op {
            Operator::Mul => self.push_op(Op::Mul),
            Operator::Div => self.push_op(Op::Div),
            Operator::Mod => self.push_op(Op::Mod),
            Operator::Pow => self.push_op(Op::Pow),
            Operator::Add => self.push_op(Op::Add),
            Operator::Sub => self.push_op(Op::Sub),
            Operator::LessThan => self.push_op(Op::LessThan),
            Operator::LessThanOrEqual => self.push_op(Op::LessThanOrEqual),
            Operator::GreaterThan => self.push_op(Op::GreaterThan),
            Operator::GreaterThanOrEqual => self.push_op(Op::GreaterThanOrEqual),
            Operator::Equal => self.push_op(Op::Eq),
            Operator::NotEqual => self.push_op(Op::Ne),
            Operator::Assign => self.push_op(Op::SetValue),
            _ => panic!("{:?}", op),
        };
        Ok(())
    }

    fn compile_template_literal(&mut self, quasis: &[String], exprs: &[Node]) -> Result<(), Error> {
        for expr in exprs {
            self.compile(expr)?;
        }
        self.push_op(Op::ProcessTemplateLiteral);
        self.push_i32(quasis.len() as i32);
        for quasi in quasis.iter().rev() {
            let id = self.string_id(quasi);
            self.push_i32(id);
        }
        Ok(())
    }

    fn compile_object_literal(&mut self, inits: &[Node]) -> Result<(), Error> {
        for init in inits {
            if let Node::ObjectInitializer(key, expr) = init {
                self.compile(key)?;
                self.compile(expr)?;
            } else {
                unreachable!();
            }
        }
        self.push_op(Op::NewObject);
        self.push_i32(inits.len() as i32);
        Ok(())
    }

    fn compile_array_literal(&mut self, inits: &[Node]) -> Result<(), Error> {
        for init in inits {
            self.compile(init)?;
        }
        self.push_op(Op::NewArray);
        self.push_i32(inits.len() as i32);
        Ok(())
    }

    fn compile_function_declaration(
        &mut self,
        name: &str,
        args: &[Node],
        body: &Node,
    ) -> Result<(), Error> {
        self.compile_function(Some(name), args, body, false)?;
        self.push_op(Op::LexicalInitialization);
        let id = self.string_id(name);
        self.push_i32(id);
        Ok(())
    }

    fn compile_function_expression(
        &mut self,
        name: &Option<String>,
        args: &[Node],
        body: &Node,
        inherits_this: bool,
    ) -> Result<(), Error> {
        self.compile_function(
            match name {
                Some(s) => Some(s.as_str()),
                None => None,
            },
            args,
            body,
            inherits_this,
        )
    }

    fn compile_function(
        &mut self,
        _name: Option<&str>,
        args: &[Node],
        body: &Node,
        inherits_this: bool,
    ) -> Result<(), Error> {
        label!(end);

        self.push_op(Op::NewFunction);
        self.push_u8(args.len() as u8);
        self.push_u8(inherits_this as u8);
        jump!(self, end); // skip evaluating body when declaring function

        // pc will be set to this location, right after the jump

        // PushContext happens inside the call handler
        // self.push_op(Op::PushContext);
        self.push_op(Op::PushScope); // holds variables for individual call

        for node in args.iter().rev() {
            match node {
                Node::Identifier(name) => {
                    label!(set);
                    self.create_lexical_declaration(name, false)?;
                    self.push_op(Op::InitReplace);
                    jump!(self, set);
                    self.push_op(Op::PushNull);
                    mark!(self, set);
                    self.push_op(Op::LexicalInitialization);
                    let id = self.string_id(name);
                    self.push_i32(id);
                }
                Node::Initializer(name, init) => {
                    label!(set);
                    self.create_lexical_declaration(name, false)?;
                    self.push_op(Op::InitReplace);
                    jump!(self, set);
                    self.compile(init)?;
                    mark!(self, set);
                    self.push_op(Op::LexicalInitialization);
                    let id = self.string_id(name);
                    self.push_i32(id);
                }
                _ => unreachable!(),
            }
        }

        if let Node::BlockStatement(nodes, declarations, ..) = body {
            for (name, mutable) in declarations {
                self.create_lexical_declaration(name, *mutable)?;
            }
            for node in nodes {
                self.compile(node)?;
            }
        } else {
            unreachable!();
        }

        self.push_op(Op::End);

        mark!(self, end);
        Ok(())
    }

    fn compile_call_expression(
        &mut self,
        callee: &Node,
        args: &[Node],
        tail: bool,
    ) -> Result<(), Error> {
        for arg in args {
            self.compile(arg)?;
        }
        match callee {
            Node::MemberExpression(base, prop) => {
                self.compile(base)?;
                self.push_op(Op::NewMemberReferenceNoConsumeStack);
                let id = self.string_id(prop);
                self.push_i32(id);
            }
            Node::ComputedMemberExpression(base, expr) => {
                self.compile(base)?;
                self.push_op(Op::NewComputedMemberReferenceNoConsumeStack);
                self.compile(expr)?;
            }
            _ => {
                self.push_op(Op::PushNull); // `this` is `null`
                self.compile(callee)?;
            }
        };
        if tail {
            self.push_op(Op::TailCall);
        } else {
            self.push_op(Op::Call);
        }
        self.push_u8(args.len() as u8);
        Ok(())
    }

    fn compile_new_expression(&mut self, expr: &Node) -> Result<(), Error> {
        match expr {
            Node::CallExpression(callee, args) => {
                self.compile(callee)?;
                for arg in args {
                    self.compile(arg)?;
                }
                self.push_op(Op::NewWithArgs);
                self.push_i32(args.len() as i32);
            }
            _ => {
                self.compile(expr)?;
                self.push_op(Op::New);
            }
        }
        Ok(())
    }

    fn compile_return_statement(&mut self, expr: &Node) -> Result<(), Error> {
        self.compile(expr)?;
        self.push_op(Op::Return);
        Ok(())
    }

    fn compile_if_statement(&mut self, test: &Node, consequent: &Node) -> Result<(), Error> {
        label!(end);
        self.compile(test)?;

        jump_if_false!(self, end);

        self.compile(consequent)?;

        mark!(self, end);

        Ok(())
    }

    fn compile_if_else_statement(
        &mut self,
        test: &Node,
        consequent: &Node,
        alternative: &Node,
    ) -> Result<(), Error> {
        label!(alt);
        label!(end);

        self.compile(test)?;
        jump_if_false!(self, alt);

        self.compile(consequent)?;
        jump!(self, end);

        mark!(self, alt);

        self.compile(alternative)?;

        mark!(self, end);

        Ok(())
    }

    fn compile_while_statement(&mut self, test: &Node, body: &Node) -> Result<(), Error> {
        label!(check);
        label!(end);
        label!(real_end);

        self.push_op(Op::PushLoop);
        jump!(self, real_end); // break location, not actually evaluated

        mark!(self, check);
        self.compile(test)?;
        jump_if_false!(self, end);

        self.compile(body)?;
        jump!(self, check);

        mark!(self, end);
        self.push_op(Op::PopLoop);

        mark!(self, real_end);

        Ok(())
    }

    fn compile_break_statement(&mut self) -> Result<(), Error> {
        self.push_op(Op::Break);
        Ok(())
    }

    fn compile_continue_statement(&mut self) -> Result<(), Error> {
        self.push_op(Op::Continue);
        Ok(())
    }

    fn compile_throw_statement(&mut self, value: &Node) -> Result<(), Error> {
        self.compile(value)?;
        self.push_op(Op::Throw);
        Ok(())
    }

    fn compile_try_statement(
        &mut self,
        try_clause: &Node,
        binding: &Option<String>,
        catch_clause: &Option<Box<Node>>,
        finally_clause: &Option<Box<Node>>,
    ) -> Result<(), Error> {
        label!(catch);
        label!(after);
        self.push_op(Op::PushTry);
        jump!(self, catch); // mark location of catch

        self.compile(try_clause)?;
        self.push_op(Op::PopTry);
        jump!(self, after);

        mark!(self, catch);
        self.push_op(Op::PushScope);
        if let Some(binding) = binding {
            self.create_lexical_declaration(binding, false)?;
            self.push_op(Op::ExceptionToStack);
            self.push_op(Op::LexicalInitialization);
            let id = self.string_id(binding);
            self.push_i32(id);
        } else {
            self.push_op(Op::DropValue); // drop thrown value
        }
        if let Some(catch_clause) = catch_clause {
            if let Node::BlockStatement(nodes, declarations, ..) = &**catch_clause {
                for (name, mutable) in declarations {
                    self.create_lexical_declaration(name, *mutable)?;
                }
                for node in nodes {
                    self.compile(node)?;
                }
            } else {
                unreachable!();
            }
        } else {
            unreachable!();
        }
        self.push_op(Op::PopScope);

        mark!(self, after);
        if let Some(finally_clause) = finally_clause {
            self.compile(finally_clause)?;
        }
        Ok(())
    }

    fn compile_member_expression(&mut self, base: &Node, prop: &str) -> Result<(), Error> {
        self.compile(base)?;
        self.push_op(Op::NewMemberReference);
        let id = self.string_id(prop);
        self.push_i32(id);
        Ok(())
    }

    fn compile_computed_member_expression(
        &mut self,
        base: &Node,
        prop: &Node,
    ) -> Result<(), Error> {
        self.compile(base)?;
        self.compile(prop)?;
        self.push_op(Op::NewComputedMemberReference);
        Ok(())
    }
}
