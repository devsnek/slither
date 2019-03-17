use crate::agent::Agent;
use crate::parser::{FunctionKind, Node, Operator, SourceSpan};
use byteorder::{LittleEndian, WriteBytesExt};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Error;

#[repr(u8)]
#[derive(PartialEq, Debug)]
pub enum Op {
    End,
    PushScope,
    PopScope,
    NewNumber,
    NewString,
    NewSymbol,
    NewRegex,
    ProcessTemplateLiteral,
    NewFunction,
    NewFunctionWithName,
    NewObject,
    NewArray,
    NewTuple,
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
    JumpIfTrueNoConsume,
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
    Await,
    Yield,
    YieldWithOperand,
    GetIterator,
    GetAsyncIterator,
    IteratorNext,
    AsyncIteratorNext,
    AsyncIteratorNextContinue,
    Add,
    Sub,
    UnarySub,
    Mul,
    Div,
    Mod,
    Pow,
    ShiftLeft,
    ShiftRight,
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

struct Jump {
    index: Option<usize>,
    targets: Vec<usize>,
}

impl Drop for Jump {
    fn drop(&mut self) {
        if !self.targets.is_empty() && self.index == None {
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
    ($agent:ident, $name:ident) => {{
        let pos = $agent.code.len();
        $name.index = Some(pos);
        for index in &$name.targets {
            $agent.code[*index] = ((pos >> 0) & 0xff) as u8;
            $agent.code[index + 1] = ((pos >> 8) & 0xff) as u8;
            $agent.code[index + 2] = ((pos >> 16) & 0xff) as u8;
            $agent.code[index + 3] = ((pos >> 24) & 0xff) as u8;
        }
    }};
}

macro_rules! jmp {
    ($agent:ident, $name:ident) => {{
        if let Some(index) = $name.index {
            push_i32($agent, index as i32);
        } else {
            let target = $agent.code.len();
            push_i32($agent, 0);
            $name.targets.push(target);
        }
    }};
}

macro_rules! jump {
    ($agent:ident, $name:ident) => {
        push_op($agent, Op::Jump);
        jmp!($agent, $name);
    };
}

macro_rules! jump_if_false {
    ($agent:ident, $name:ident) => {
        push_op($agent, Op::JumpIfFalse);
        jmp!($agent, $name);
    };
}

macro_rules! jump_if_false_no_consume {
    ($agent:ident, $name:ident) => {
        push_op($agent, Op::JumpIfFalseNoConsume);
        jmp!($agent, $name);
    };
}

macro_rules! jump_if_true_no_consume {
    ($agent:ident, $name:ident) => {
        push_op($agent, Op::JumpIfTrueNoConsume);
        jmp!($agent, $name);
    };
}

#[inline]
fn push_op(agent: &mut Agent, op: Op) {
    agent.code.push(op as u8);
}

#[inline]
fn push_u8(agent: &mut Agent, n: u8) {
    agent.code.push(n);
}

#[inline]
fn push_i32(agent: &mut Agent, n: i32) {
    agent.code.write_i32::<LittleEndian>(n).unwrap();
}

#[inline]
fn push_f64(agent: &mut Agent, n: f64) {
    agent.code.write_f64::<LittleEndian>(n).unwrap();
}

fn string_id(agent: &mut Agent, string: &str) -> i32 {
    let index = agent.string_table.iter().position(|s| s == string);
    match index {
        Some(i) => i as i32,
        None => {
            let id = agent.string_table.len();
            agent.string_table.push(string.to_string());
            id as i32
        }
    }
}

pub fn real_compile(
    agent: &mut Agent,
    node: &Node,
    _positions: HashMap<*const Node, SourceSpan>,
) -> Result<(usize, usize), Error> {
    let start = agent.code.len();
    compile(agent, node)?;
    push_op(agent, Op::End);
    let end = agent.code.len() - 1;
    Ok((start, end))
}

fn compile(agent: &mut Agent, node: &Node) -> Result<(), Error> {
    match node {
        Node::ExpressionStatement(expr) => {
            compile(agent, expr)?;
            push_op(agent, Op::GetValue);
            push_op(agent, Op::DropValue);
            Ok(())
        }
        Node::ParenthesizedExpression(expr) => {
            compile(agent, expr)?;
            push_op(agent, Op::GetValue);
            Ok(())
        }
        Node::BlockStatement(nodes, declarations, top) => {
            compile_block_statement(agent, nodes, declarations, *top)
        }
        Node::LexicalInitialization(name, val) => compile_lexical_initialization(agent, name, val),
        Node::Identifier(name) => {
            push_op(agent, Op::NewIdentifier);
            let id = string_id(agent, name);
            push_i32(agent, id);
            Ok(())
        }
        Node::NullLiteral => {
            push_op(agent, Op::PushNull);
            Ok(())
        }
        Node::TrueLiteral => {
            push_op(agent, Op::PushTrue);
            Ok(())
        }
        Node::FalseLiteral => {
            push_op(agent, Op::PushFalse);
            Ok(())
        }
        Node::NumberLiteral(n) => {
            push_op(agent, Op::NewNumber);
            push_f64(agent, *n);
            Ok(())
        }
        Node::StringLiteral(s) => {
            push_op(agent, Op::NewString);
            let id = string_id(agent, s);
            push_i32(agent, id);
            Ok(())
        }
        Node::SymbolLiteral(name) => {
            push_op(agent, Op::NewSymbol);
            let id = string_id(agent, name);
            push_i32(agent, id);
            Ok(())
        }
        Node::RegexLiteral(pattern) => {
            push_op(agent, Op::NewRegex);
            let id = string_id(agent, pattern);
            push_i32(agent, id);
            Ok(())
        }
        Node::TemplateLiteral(quasis, exprs) => compile_template_literal(agent, quasis, exprs),
        Node::ObjectLiteral(inits) => compile_object_literal(agent, inits),
        Node::ArrayLiteral(inits) => compile_array_literal(agent, inits),
        Node::TupleLiteral(items) => compile_tuple_literal(agent, items),
        Node::CallExpression(callee, args) => compile_call_expression(agent, callee, args, false),
        Node::TailCallExpression(callee, args) => {
            compile_call_expression(agent, callee, args, true)
        }
        Node::ReturnStatement(expr) => {
            compile(agent, expr)?;
            push_op(agent, Op::Return);
            Ok(())
        }
        Node::ThrowStatement(expr) => {
            compile(agent, expr)?;
            push_op(agent, Op::Throw);
            Ok(())
        }
        Node::IfStatement(test, consequent) => compile_if_statement(agent, test, consequent, None),
        Node::IfElseStatement(test, consequent, alternative) => {
            compile_if_statement(agent, test, consequent, Some(alternative))
        }
        Node::WhileStatement(test, body) => compile_while_statement(agent, test, body),
        Node::ForStatement(asyn, binding, target, body) => {
            compile_for_statement(*asyn, agent, binding, target, body)
        }
        Node::BreakStatement => {
            push_op(agent, Op::Break);
            Ok(())
        }
        Node::ContinueStatement => {
            push_op(agent, Op::Continue);
            Ok(())
        }
        Node::TryStatement(try_clause, binding, catch, finally) => {
            compile_try_statement(agent, try_clause, binding, catch, finally)
        }
        Node::NewExpression(expr) => compile_new_expression(agent, expr),
        Node::MemberExpression(base, prop) => {
            compile(agent, base)?;
            push_op(agent, Op::NewMemberReference);
            let id = string_id(agent, prop);
            push_i32(agent, id);
            Ok(())
        }
        Node::ComputedMemberExpression(base, prop) => {
            compile(agent, base)?;
            compile(agent, prop)?;
            push_op(agent, Op::NewComputedMemberReference);
            Ok(())
        }
        Node::ThisExpression => {
            push_op(agent, Op::GetThis);
            Ok(())
        }
        Node::UnaryExpression(op, expr) => compile_unary_expression(agent, op, expr),
        Node::BinaryExpression(left, op, right) => {
            compile_binary_expression(agent, op, left, right)
        }
        Node::ConditionalExpression(test, consequent, alternative) => {
            compile_conditional_expression(agent, test, consequent, alternative)
        }
        Node::FunctionDeclaration(name, body, args, kind) => {
            compile_function_declaration(agent, name, body, args, *kind)
        }
        Node::FunctionExpression(name, body, args, kind) => {
            compile_function_expression(agent, name, body, args, false, *kind)
        }
        Node::ArrowFunctionExpression(body, args, kind) => {
            compile_function_expression(agent, &None, body, args, true, *kind)
        }
        Node::AwaitExpression(expr) => {
            compile(agent, expr)?;
            push_op(agent, Op::Await);
            Ok(())
        }
        Node::YieldExpression(expr) => {
            match expr {
                Some(expr) => {
                    compile(agent, expr)?;
                    push_op(agent, Op::YieldWithOperand);
                }
                None => {
                    push_op(agent, Op::Yield);
                }
            }
            Ok(())
        }
        Node::ImportStandardDeclaration(..) => Ok(()),
        Node::ImportDefaultDeclaration(..) => Ok(()),
        Node::ImportNamedDeclaration(..) => Ok(()),
        Node::ImportDeclaration(..) => Ok(()),
        Node::ExportDeclaration(decl) => compile(agent, decl),
        Node::Initializer(..) | Node::ObjectInitializer(..) => panic!(),
    }
}

fn create_lexical_declaration(agent: &mut Agent, name: &str, mutable: bool) -> Result<(), Error> {
    push_op(agent, Op::LexicalDeclaration);
    push_u8(agent, mutable as u8);
    let id = string_id(agent, &name);
    push_i32(agent, id);
    Ok(())
}

fn compile_lexical_initialization(
    agent: &mut Agent,
    name: &str,
    value: &Node,
) -> Result<(), Error> {
    compile(agent, value)?;
    push_op(agent, Op::LexicalInitialization);
    let id = string_id(agent, name);
    push_i32(agent, id);
    Ok(())
}

fn compile_block_statement(
    agent: &mut Agent,
    nodes: &[Node],
    declarations: &HashMap<String, bool>,
    top: bool,
) -> Result<(), Error> {
    if !top {
        push_op(agent, Op::PushScope);
    }
    for (name, mutable) in declarations {
        create_lexical_declaration(agent, name, *mutable)?;
    }
    for node in nodes {
        compile(agent, node)?;
        if let Node::ReturnStatement(..) = node {
            break;
        }
    }
    if !top {
        push_op(agent, Op::PopScope);
    }
    Ok(())
}

fn compile_template_literal(
    agent: &mut Agent,
    quasis: &[String],
    exprs: &[Node],
) -> Result<(), Error> {
    for expr in exprs {
        compile(agent, expr)?;
    }
    push_op(agent, Op::ProcessTemplateLiteral);
    push_i32(agent, quasis.len() as i32);
    for quasi in quasis.iter().rev() {
        let id = string_id(agent, quasi);
        push_i32(agent, id);
    }
    Ok(())
}

fn compile_object_literal(agent: &mut Agent, inits: &[Node]) -> Result<(), Error> {
    for init in inits {
        if let Node::ObjectInitializer(key, expr) = init {
            compile(agent, key)?;
            compile(agent, expr)?;
        } else {
            unreachable!();
        }
    }
    push_op(agent, Op::NewObject);
    push_i32(agent, inits.len() as i32);
    Ok(())
}

fn compile_array_literal(agent: &mut Agent, inits: &[Node]) -> Result<(), Error> {
    for init in inits {
        compile(agent, init)?;
    }
    push_op(agent, Op::NewArray);
    push_i32(agent, inits.len() as i32);
    Ok(())
}

fn compile_tuple_literal(agent: &mut Agent, items: &[Node]) -> Result<(), Error> {
    for item in items {
        compile(agent, item)?;
    }
    push_op(agent, Op::NewTuple);
    push_i32(agent, items.len() as i32);
    Ok(())
}

fn compile_function_declaration(
    agent: &mut Agent,
    name: &str,
    args: &[Node],
    body: &Node,
    kind: FunctionKind,
) -> Result<(), Error> {
    compile_function(agent, Some(name), args, body, false, kind)?;
    push_op(agent, Op::LexicalInitialization);
    let id = string_id(agent, name);
    push_i32(agent, id);
    Ok(())
}

fn compile_function_expression(
    agent: &mut Agent,
    name: &Option<String>,
    args: &[Node],
    body: &Node,
    inherits_this: bool,
    kind: FunctionKind,
) -> Result<(), Error> {
    compile_function(
        agent,
        match name {
            Some(s) => Some(s.as_str()),
            None => None,
        },
        args,
        body,
        inherits_this,
        kind,
    )
}

fn compile_function(
    agent: &mut Agent,
    name: Option<&str>,
    args: &[Node],
    body: &Node,
    inherits_this: bool,
    kind: FunctionKind,
) -> Result<(), Error> {
    label!(end);

    if let Some(name) = name {
        push_op(agent, Op::NewFunctionWithName);
        let id = string_id(agent, name);
        push_i32(agent, id);
    } else {
        push_op(agent, Op::NewFunction);
    }
    push_u8(agent, args.len() as u8);
    push_u8(agent, inherits_this as u8);
    push_u8(agent, kind as u8);
    jump!(agent, end); // skip evaluating body when declaring function

    // pc will be set to this location, right after the jump

    push_op(agent, Op::PushScope); // holds variables for individual call

    for node in args.iter().rev() {
        match node {
            Node::Identifier(name) => {
                label!(set);
                create_lexical_declaration(agent, name, false)?;
                push_op(agent, Op::InitReplace);
                jump!(agent, set);
                push_op(agent, Op::PushNull);
                mark!(agent, set);
                push_op(agent, Op::LexicalInitialization);
                let id = string_id(agent, name);
                push_i32(agent, id);
            }
            Node::Initializer(name, init) => {
                label!(set);
                create_lexical_declaration(agent, name, false)?;
                push_op(agent, Op::InitReplace);
                jump!(agent, set);
                compile(agent, init)?;
                mark!(agent, set);
                push_op(agent, Op::LexicalInitialization);
                let id = string_id(agent, name);
                push_i32(agent, id);
            }
            _ => unreachable!(),
        }
    }

    if let Node::BlockStatement(nodes, declarations, ..) = body {
        for (name, mutable) in declarations {
            create_lexical_declaration(agent, name, *mutable)?;
        }
        for node in nodes {
            compile(agent, node)?;
        }
    } else {
        unreachable!();
    }

    push_op(agent, Op::PushNull);
    push_op(agent, Op::Return);

    mark!(agent, end);
    Ok(())
}

fn compile_call_expression(
    agent: &mut Agent,
    callee: &Node,
    args: &[Node],
    tail: bool,
) -> Result<(), Error> {
    for arg in args {
        compile(agent, arg)?;
    }
    match callee {
        Node::MemberExpression(base, prop) => {
            compile(agent, base)?;
            push_op(agent, Op::NewMemberReferenceNoConsumeStack);
            let id = string_id(agent, prop);
            push_i32(agent, id);
        }
        Node::ComputedMemberExpression(base, expr) => {
            compile(agent, base)?;
            push_op(agent, Op::NewComputedMemberReferenceNoConsumeStack);
            compile(agent, expr)?;
        }
        _ => {
            push_op(agent, Op::PushNull); // `this` is `null`
            compile(agent, callee)?;
        }
    };
    if tail {
        push_op(agent, Op::TailCall);
    } else {
        push_op(agent, Op::Call);
    }
    push_u8(agent, args.len() as u8);
    Ok(())
}

fn compile_if_statement(
    agent: &mut Agent,
    test: &Node,
    consequent: &Node,
    alternative: Option<&Node>,
) -> Result<(), Error> {
    label!(alt);
    label!(end);

    compile(agent, test)?;
    if alternative.is_some() {
        jump_if_false!(agent, alt);
    } else {
        jump_if_false!(agent, end);
    }

    compile(agent, consequent)?;

    if alternative.is_some() {
        jump!(agent, end);

        mark!(agent, alt);
        compile(agent, alternative.unwrap())?;
    }

    mark!(agent, end);
    Ok(())
}

fn compile_while_statement(agent: &mut Agent, test: &Node, body: &Node) -> Result<(), Error> {
    label!(check);
    label!(end);
    label!(real_end);

    push_op(agent, Op::PushLoop);
    jump!(agent, real_end); // break location, not actually evaluated

    mark!(agent, check);
    compile(agent, test)?;
    jump_if_false!(agent, end);

    compile(agent, body)?;
    jump!(agent, check);

    mark!(agent, end);
    push_op(agent, Op::PopLoop);

    mark!(agent, real_end);

    Ok(())
}

fn compile_for_statement(
    asyn: bool,
    agent: &mut Agent,
    binding: &str,
    target: &Node,
    body: &Node,
) -> Result<(), Error> {
    label!(head);
    label!(end);
    label!(real_end);

    /*
    for (await) BINDING in TARGET { BODY }
    @=>
    iterator = GetIterator(TARGET)
    loop {
      <head>
      { done, value } = (await) IteratorNext(iterator)
      if done {
        break
      }
      BINDING = value

      BODY

      <end>
    }
    <real_end>
    */

    push_op(agent, Op::PushLoop);
    jump!(agent, real_end); // break location, not actually evaluated

    compile(agent, target)?; // stack is [target]
    push_op(
        agent,
        if asyn {
            Op::GetAsyncIterator
        } else {
            Op::GetIterator
        },
    );
    // consumes target, stack is [iterator]

    mark!(agent, head);
    if asyn {
        push_op(agent, Op::AsyncIteratorNext); // performs await
        push_op(agent, Op::AsyncIteratorNextContinue);
    // AsyncIteratorNextContinue jumps if done is true
    // stack is [iterator, value]
    } else {
        push_op(agent, Op::IteratorNext);
        // IteratorNext jumps if done is true
        // stack is [iterator, value]
    }

    push_op(agent, Op::PushScope);
    create_lexical_declaration(agent, binding, false)?;
    push_op(agent, Op::LexicalInitialization);
    let id = string_id(agent, binding);
    push_i32(agent, id);
    // stack is [iterator], `binding` is set to `value`
    if let Node::BlockStatement(nodes, declarations, ..) = body {
        for (name, mutable) in declarations {
            create_lexical_declaration(agent, name, *mutable)?;
        }
        for node in nodes {
            compile(agent, node)?;
        }
    } else {
        unreachable!();
    }
    push_op(agent, Op::PopScope);
    jump!(agent, head);

    mark!(agent, end);
    push_op(agent, Op::PopLoop);

    mark!(agent, real_end);

    Ok(())
}

fn compile_try_statement(
    agent: &mut Agent,
    try_clause: &Node,
    binding: &Option<String>,
    catch_clause: &Option<Box<Node>>,
    finally_clause: &Option<Box<Node>>,
) -> Result<(), Error> {
    label!(catch);
    label!(after);

    push_op(agent, Op::PushTry);
    jump!(agent, catch); // mark location of catch

    compile(agent, try_clause)?;
    push_op(agent, Op::PopTry);
    jump!(agent, after);

    mark!(agent, catch);
    push_op(agent, Op::PushScope);
    if let Some(binding) = binding {
        create_lexical_declaration(agent, binding, false)?;
        push_op(agent, Op::ExceptionToStack);
        push_op(agent, Op::LexicalInitialization);
        let id = string_id(agent, binding);
        push_i32(agent, id);
    } else {
        push_op(agent, Op::DropValue); // drop thrown value
    }
    if let Some(catch_clause) = catch_clause {
        if let Node::BlockStatement(nodes, declarations, ..) = &**catch_clause {
            for (name, mutable) in declarations {
                create_lexical_declaration(agent, name, *mutable)?;
            }
            for node in nodes {
                compile(agent, node)?;
            }
        } else {
            unreachable!();
        }
    } else {
        unreachable!();
    }
    push_op(agent, Op::PopScope);

    mark!(agent, after);
    if let Some(finally_clause) = finally_clause {
        compile(agent, finally_clause)?;
    }
    Ok(())
}

fn compile_new_expression(agent: &mut Agent, expr: &Node) -> Result<(), Error> {
    match expr {
        Node::CallExpression(callee, args) => {
            compile(agent, callee)?;
            for arg in args {
                compile(agent, arg)?;
            }
            push_op(agent, Op::NewWithArgs);
            push_i32(agent, args.len() as i32);
        }
        _ => {
            compile(agent, expr)?;
            push_op(agent, Op::New);
        }
    }
    Ok(())
}

fn compile_unary_expression(agent: &mut Agent, op: &Operator, expr: &Node) -> Result<(), Error> {
    compile(agent, expr)?;
    match op {
        Operator::Typeof => push_op(agent, Op::Typeof),
        Operator::Sub => push_op(agent, Op::UnarySub),
        Operator::Not => push_op(agent, Op::Not),
        _ => panic!("unhandled unop {:?}", op),
    }
    Ok(())
}

fn compile_binary_expression(
    agent: &mut Agent,
    op: &Operator,
    left: &Node,
    right: &Node,
) -> Result<(), Error> {
    if *op == Operator::LogicalAND {
        label!(end);
        compile(agent, left)?;
        jump_if_false_no_consume!(agent, end);
        push_op(agent, Op::DropValue);
        compile(agent, right)?;
        mark!(agent, end);
        return Ok(());
    }
    if *op == Operator::LogicalOR {
        label!(end);
        compile(agent, left)?;
        jump_if_true_no_consume!(agent, end);
        push_op(agent, Op::DropValue);
        compile(agent, right)?;
        mark!(agent, end);
        return Ok(());
    }
    compile(agent, left)?;
    compile(agent, right)?;
    match op {
        Operator::Mul => push_op(agent, Op::Mul),
        Operator::Div => push_op(agent, Op::Div),
        Operator::Mod => push_op(agent, Op::Mod),
        Operator::Pow => push_op(agent, Op::Pow),
        Operator::Add => push_op(agent, Op::Add),
        Operator::Sub => push_op(agent, Op::Sub),
        Operator::LeftShift => push_op(agent, Op::ShiftLeft),
        Operator::RightShift => push_op(agent, Op::ShiftRight),
        Operator::LessThan => push_op(agent, Op::LessThan),
        Operator::LessThanOrEqual => push_op(agent, Op::LessThanOrEqual),
        Operator::GreaterThan => push_op(agent, Op::GreaterThan),
        Operator::GreaterThanOrEqual => push_op(agent, Op::GreaterThanOrEqual),
        Operator::Equal => push_op(agent, Op::Eq),
        Operator::NotEqual => push_op(agent, Op::Ne),
        Operator::Assign => push_op(agent, Op::SetValue),
        _ => panic!("{:?}", op),
    };
    Ok(())
}

fn compile_conditional_expression(
    agent: &mut Agent,
    test: &Node,
    consequent: &Node,
    alternative: &Node,
) -> Result<(), Error> {
    label!(alt);
    label!(end);
    compile(agent, test)?;
    jump_if_false!(agent, alt);
    compile(agent, consequent)?;
    jump!(agent, end);
    mark!(agent, alt);
    compile(agent, alternative)?;
    mark!(agent, end);
    Ok(())
}
