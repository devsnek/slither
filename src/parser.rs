use crate::{Agent, IntoValue, Value};
use indexmap::IndexMap;
use std::collections::VecDeque;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operator {
    Add,
    AddAssign,
    Sub,
    SubAssign,
    Mul,
    MulAssign,
    Pow,
    PowAssign,
    Div,
    DivAssign,
    Mod,
    ModAssign,
    LeftShift,
    RightShift,
    GreaterThan,
    GreaterThanOrEqual,
    Not,
    LessThan,
    LessThanOrEqual,
    BitwiseAND,
    BitwiseOR,
    BitwiseXOR,
    BitwiseNOT,
    LogicalAND,
    LogicalOR,
    Assign,
    Equal,
    NotEqual,
    Typeof,
    Void,
}

#[derive(Debug, PartialEq, Clone)]
enum Token {
    Null,
    True,
    False,
    NumberLiteral(f64),
    StringLiteral(String),

    Identifier(String),

    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    LeftParen,
    RightParen,
    Semicolon,
    Colon,
    Question,
    Dot,
    At,
    Comma,
    BackQuote,
    Ellipsis,
    Arrow,

    This,
    Function,
    Class,
    Extends,
    New,
    Let,
    Const,
    Return,
    Throw,
    Break,
    Continue,
    Try,
    Catch,
    Finally,
    If,
    Else,
    While,
    For,
    In,
    Yield,
    Await,
    Async,
    Gen,
    Import,
    Export,
    Default,
    From,

    Operator(Operator),
}

#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum FunctionKind {
    Normal = 0b0001,
    Async = 0b0010,
    Generator = 0b0100,
    Arrow = 0b1000,
}

impl From<u8> for FunctionKind {
    fn from(n: u8) -> Self {
        unsafe { std::mem::transmute::<u8, Self>(n) }
    }
}

impl std::ops::BitAnd for FunctionKind {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self {
        (self as u8 & rhs as u8).into()
    }
}

impl std::ops::BitOr for FunctionKind {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        (self as u8 | rhs as u8).into()
    }
}

#[derive(Debug, PartialEq)]
pub enum ScopeKind {
    TopLevel,
    Block,
}

#[derive(Debug, PartialEq)]
pub struct Scope {
    pub kind: ScopeKind,
    pub bindings: IndexMap<String, bool>,
}

impl Scope {
    fn new(scope: ParseScope) -> Scope {
        Scope {
            kind: match scope {
                ParseScope::TopLevel => ScopeKind::TopLevel,
                ParseScope::Block => ScopeKind::Block,
                ParseScope::Loop => ScopeKind::Block,
                ParseScope::Function => ScopeKind::Block,
                ParseScope::AsyncFunction => ScopeKind::Block,
                ParseScope::GeneratorFunction => ScopeKind::Block,
            },
            bindings: IndexMap::new(),
        }
    }

    fn declare(&mut self, name: &str, mutable: bool) -> bool {
        if self.bindings.contains_key(name) {
            false
        } else {
            self.bindings.insert_full(name.to_string(), mutable);
            true
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Node {
    NullLiteral,
    TrueLiteral,
    FalseLiteral,
    NumberLiteral(f64),
    StringLiteral(String),
    SymbolLiteral(String),
    RegexLiteral(String),
    ObjectLiteral(Vec<Node>),
    ArrayLiteral(Vec<Node>),
    TupleLiteral(Vec<Node>),

    Identifier(String),

    Block(Scope, Vec<Node>),

    IfStatement(Box<Node>, Box<Node>, Option<Box<Node>>),
    ConditionalExpression(Box<Node>, Box<Node>, Box<Node>),

    WhileLoop(Box<Node>, Box<Node>),
    ForLoop(bool, String, Box<Node>, Box<Node>),

    ExpressionStatement(Box<Node>),
    UnaryExpression(Operator, Box<Node>),
    BinaryExpression(Operator, Box<Node>, Box<Node>),
    ParenthesizedExpression(Box<Node>),

    YieldExpression(Option<Box<Node>>),
    AwaitExpression(Box<Node>),
    ThisExpression,
    NewExpression(Box<Node>),

    MemberExpression(Box<Node>, String),
    ComputedMemberExpression(Box<Node>, Box<Node>),
    CallExpression(Box<Node>, Vec<Node>),
    TailCallExpression(Box<Node>, Vec<Node>),

    FunctionExpression(FunctionKind, Option<String>, Vec<Node>, Box<Node>),
    FunctionDeclaration(FunctionKind, String, Vec<Node>, Box<Node>),
    ArrowFunctionExpression(FunctionKind, Vec<Node>, Box<Node>),

    ClassExpression(String, Option<Box<Node>>, Vec<Node>),
    ClassDeclaration(String, Option<Box<Node>>, Vec<Node>),

    LexicalInitialization(String, Box<Node>),

    ReturnStatement(Option<Box<Node>>),
    ThrowStatement(Box<Node>),
    BreakStatement,
    ContinueStatement,
    TryStatement(
        Box<Node>,
        Option<String>,
        Option<Box<Node>>,
        Option<Box<Node>>,
    ),

    ImportDeclaration(String),
    ImportNamedDeclaration(String, Vec<String>),
    ImportDefaultDeclaration(String, String),
    ImportStandardDeclaration(String, Vec<String>),
    ExportDeclaration(Box<Node>),

    Initializer(Box<Node>, Box<Node>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u8)]
#[rustfmt::skip]
enum ParseScope {
    TopLevel          = 0b0000_0001,
    Block             = 0b0000_0010,
    Loop              = 0b0000_0100,
    Function          = 0b0000_1000,
    AsyncFunction     = 0b0001_1000,
    GeneratorFunction = 0b0010_1000,
}

#[derive(Debug)]
pub enum Error {
    NormalEOF,
    UnexpectedEOF,
    UnexpectedToken,
    DuplicateBinding,
    InvalidAssignmentTarget,
}

impl IntoValue for Error {
    fn into_value(&self, agent: &Agent) -> Value {
        Value::new_error(agent, &format!("{:?}", self))
    }
}

struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    peeked: Option<Option<Token>>,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Lexer<'a> {
        Lexer {
            peeked: None,
            chars: code.chars().peekable(),
        }
    }

    fn next(&mut self) -> Option<Token> {
        match self.peeked.take() {
            Some(v) => v,
            None => match self.chars.next() {
                Some(char) => match char {
                    ' ' | '\t' | '\r' | '\n' => self.next(),
                    '0'...'9' => {
                        let mut str = char.to_string();
                        let mut one_dot = false;
                        while let Some(c) = self.chars.peek() {
                            match c {
                                '0'...'9' => {
                                    str.push(self.chars.next().unwrap());
                                }
                                '.' => {
                                    if !one_dot {
                                        one_dot = true;
                                        str.push(self.chars.next().unwrap());
                                    } else {
                                        break;
                                    }
                                }
                                _ => break,
                            }
                        }
                        let num = str
                            .parse::<f64>()
                            .unwrap_or_else(|_| panic!("Invalid number {}", str));
                        Some(Token::NumberLiteral(num))
                    }
                    '"' | '\'' => {
                        let mut str = String::new();
                        while let Some(c) = self.chars.peek() {
                            if c == &char {
                                self.chars.next();
                                break;
                            }
                            let c = self.chars.next().unwrap();
                            match c {
                                '\\' => match self.chars.next().unwrap() {
                                    'n' => str.push('\n'),
                                    't' => str.push('\t'),
                                    '"' => str.push('"'),
                                    '\'' => str.push('\''),
                                    '\\' => str.push('\\'),
                                    c => str.push(c),
                                },
                                '\r' | '\n' => {
                                    panic!("unexpected end of string");
                                }
                                c => str.push(c),
                            }
                        }
                        Some(Token::StringLiteral(str))
                    }
                    'a'...'z' | 'A'...'Z' | '_' => {
                        let mut ident = char.to_string();
                        while let Some(c) = self.chars.peek() {
                            match c {
                                'a'...'z' | 'A'...'Z' | '0'...'9' | '_' => {
                                    ident.push(self.chars.next().unwrap())
                                }
                                _ => break,
                            }
                        }
                        // UPDATE parse_identifier WHEN YOU ADD TO THIS LIST!!!!!!
                        Some(match ident.as_ref() {
                            "true" => Token::True,
                            "false" => Token::False,
                            "null" => Token::Null,
                            "this" => Token::This,
                            "class" => Token::Class,
                            "extends" => Token::Extends,
                            "function" => Token::Function,
                            "let" => Token::Let,
                            "const" => Token::Const,
                            "throw" => Token::Throw,
                            "return" => Token::Return,
                            "try" => Token::Try,
                            "catch" => Token::Catch,
                            "finally" => Token::Finally,
                            "break" => Token::Break,
                            "continue" => Token::Continue,
                            "if" => Token::If,
                            "else" => Token::Else,
                            "while" => Token::While,
                            "for" => Token::For,
                            "in" => Token::In,
                            "new" => Token::New,
                            "import" => Token::Import,
                            "export" => Token::Export,
                            "default" => Token::Default,
                            "from" => Token::From,
                            "async" => Token::Async,
                            "await" => Token::Await,
                            "gen" => Token::Gen,
                            "yield" => Token::Yield,
                            "typeof" => Token::Operator(Operator::Typeof),
                            "void" => Token::Operator(Operator::Void),
                            _ => Token::Identifier(ident),
                        })
                    }
                    '{' => Some(Token::LeftBrace),
                    '}' => Some(Token::RightBrace),
                    '[' => Some(Token::LeftBracket),
                    ']' => Some(Token::RightBracket),
                    '(' => Some(Token::LeftParen),
                    ')' => Some(Token::RightParen),
                    ':' => Some(Token::Colon),
                    ';' => Some(Token::Semicolon),
                    '?' => Some(Token::Question),
                    '.' => Some(match self.chars.peek() {
                        Some('.') => {
                            self.chars.next();
                            if let Some('.') = self.chars.peek() {
                                self.chars.next();
                                Token::Ellipsis
                            } else {
                                panic!();
                            }
                        }
                        _ => Token::Dot,
                    }),
                    ',' => Some(Token::Comma),
                    '`' => Some(Token::BackQuote),
                    '+' => Some(match self.chars.peek() {
                        Some('=') => {
                            self.chars.next();
                            Token::Operator(Operator::AddAssign)
                        }
                        _ => Token::Operator(Operator::Add),
                    }),
                    '-' => Some(match self.chars.peek() {
                        Some('=') => {
                            self.chars.next();
                            Token::Operator(Operator::SubAssign)
                        }
                        _ => Token::Operator(Operator::Sub),
                    }),
                    '*' => Some(match self.chars.peek() {
                        Some('*') => {
                            self.chars.next();
                            match self.chars.peek() {
                                Some('=') => {
                                    self.chars.next();
                                    Token::Operator(Operator::PowAssign)
                                }
                                _ => Token::Operator(Operator::Pow),
                            }
                        }
                        _ => match self.chars.peek() {
                            Some('=') => {
                                self.chars.next();
                                Token::Operator(Operator::MulAssign)
                            }
                            _ => Token::Operator(Operator::Mul),
                        },
                    }),
                    '/' => match self.chars.peek() {
                        Some('=') => {
                            self.chars.next();
                            Some(Token::Operator(Operator::DivAssign))
                        }
                        Some('*') => {
                            loop {
                                if self.chars.peek() == None {
                                    return None; // Err(Error::UnexpectedEOF);
                                }
                                if let Some('*') = self.chars.next() {
                                    if let Some('/') = self.chars.next() {
                                        break;
                                    }
                                }
                            }
                            self.next()
                        }
                        Some('/') => {
                            loop {
                                if self.chars.peek() == None {
                                    return None; // Err(Error::UnexpectedEOF);
                                }
                                if let Some('\n') = self.chars.next() {
                                    break;
                                }
                            }
                            self.next()
                        }
                        _ => Some(Token::Operator(Operator::Div)),
                    },
                    '%' => Some(match self.chars.peek() {
                        Some('=') => {
                            self.chars.next();
                            Token::Operator(Operator::ModAssign)
                        }
                        _ => Token::Operator(Operator::Mod),
                    }),
                    '<' => Some(match self.chars.peek() {
                        Some('<') => {
                            self.chars.next();
                            Token::Operator(Operator::LeftShift)
                        }
                        Some('=') => {
                            self.chars.next();
                            Token::Operator(Operator::LessThanOrEqual)
                        }
                        _ => Token::Operator(Operator::LessThan),
                    }),
                    '!' => Some(match self.chars.peek() {
                        Some('=') => {
                            self.chars.next();
                            Token::Operator(Operator::NotEqual)
                        }
                        _ => Token::Operator(Operator::Not),
                    }),
                    '>' => Some(match self.chars.peek() {
                        Some('>') => {
                            self.chars.next();
                            Token::Operator(Operator::RightShift)
                        }
                        Some('=') => {
                            self.chars.next();
                            Token::Operator(Operator::GreaterThanOrEqual)
                        }
                        _ => Token::Operator(Operator::GreaterThan),
                    }),
                    '&' => Some(match self.chars.peek() {
                        Some('&') => {
                            self.chars.next();
                            Token::Operator(Operator::LogicalAND)
                        }
                        _ => Token::Operator(Operator::BitwiseAND),
                    }),
                    '|' => Some(match self.chars.peek() {
                        Some('|') => {
                            self.chars.next();
                            Token::Operator(Operator::LogicalOR)
                        }
                        _ => Token::Operator(Operator::BitwiseOR),
                    }),
                    '^' => Some(Token::Operator(Operator::BitwiseXOR)),
                    '~' => Some(Token::Operator(Operator::BitwiseNOT)),
                    '=' => Some(match self.chars.peek() {
                        Some('=') => {
                            self.chars.next();
                            Token::Operator(Operator::Equal)
                        }
                        Some('>') => {
                            self.chars.next();
                            Token::Arrow
                        }
                        _ => Token::Operator(Operator::Assign),
                    }),
                    '@' => Some(Token::At),
                    _ => {
                        panic!("unexpected token {}", char);
                    }
                },
                None => None,
            },
        }
    }

    #[inline]
    pub fn peek(&mut self) -> Option<&Token> {
        if self.peeked.is_none() {
            self.peeked = Some(self.next());
        }
        match self.peeked {
            Some(Some(ref value)) => Some(value),
            Some(None) => None,
            _ => unreachable!(),
        }
    }

    #[inline]
    pub fn peek_immutable(&self) -> Option<&Token> {
        if self.peeked.is_none() {
            panic!();
        }
        match self.peeked {
            Some(Some(ref value)) => Some(value),
            Some(None) => None,
            _ => unreachable!(),
        }
    }

    fn skip_hashbang(&mut self) {
        if self.chars.peek() == Some(&'#') {
            self.chars.next();
            if self.chars.peek() == Some(&'!') {
                loop {
                    match self.chars.next() {
                        Some('\n') | None => break,
                        _ => {}
                    }
                }
            }
        }
    }
}

macro_rules! binop_production {
    ( $name:ident, $lower:ident, [ $( $op:path ),* ] ) => {
        fn $name(&mut self) -> Result<Node, Error> {
            let mut lhs = self.$lower()?;
            match self.lexer.peek() {
                Some(Token::Operator(op)) if $( op == &$op )||* => {
                    let op = op.clone();
                    self.lexer.next();
                    let rhs = self.$name()?;
                    lhs = Node::BinaryExpression(op, Box::new(lhs), Box::new(rhs));
                }
                _ => {},
            }
            Ok(lhs)
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    scope: Vec<Scope>,
    scope_bits: u8,
}

impl<'a> Parser<'a> {
    pub fn parse(code: &'a str) -> Result<Node, Error> {
        let mut parser = Parser {
            lexer: Lexer::new(code),
            scope_bits: 0,
            scope: Vec::new(),
        };

        parser.lexer.skip_hashbang();

        if let Node::Block(scope, mut stmts) = parser.parse_block(ParseScope::TopLevel)? {
            if let Some(Node::ExpressionStatement(..)) = stmts.last() {
                // if the last item is an expression statement, replace it with the expression
                // so that the value will be left on the stack to inspect in tests
                if let Node::ExpressionStatement(expr) = stmts.pop().unwrap() {
                    stmts.push(Node::ParenthesizedExpression(expr));
                    Ok(Node::Block(scope, stmts))
                } else {
                    unreachable!();
                }
            } else {
                Ok(Node::Block(scope, stmts))
            }
        } else {
            unreachable!();
        }
    }

    fn scope(&self, scope: ParseScope) -> bool {
        (self.scope_bits & scope as u8) == scope as u8
    }

    fn declare(&mut self, name: &str, mutable: bool) -> Result<(), Error> {
        let scope = self.scope.last_mut().unwrap();
        if scope.declare(name, mutable) {
            Ok(())
        } else {
            Err(Error::DuplicateBinding)
        }
    }

    fn peek(&mut self, token: Token) -> bool {
        self.lexer.peek() == Some(&token)
    }

    #[inline]
    fn eat(&mut self, token: Token) -> bool {
        if self.peek(token) {
            self.lexer.next();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, token: Token) -> Result<Token, Error> {
        let t = self.lexer.next();
        match t {
            Some(ref t) if t == &token => Ok(token),
            None => Err(Error::UnexpectedEOF),
            _ => Err(Error::UnexpectedToken),
        }
    }

    fn parse_statement(&mut self) -> Result<Node, Error> {
        self.lexer.peek();
        match self.lexer.peek_immutable() {
            None => Err(Error::NormalEOF),
            Some(Token::LeftBrace) => self.parse_block(ParseScope::Block),
            Some(Token::Let) | Some(Token::Const) => self.parse_lexical_declaration(),
            Some(Token::Function) => {
                self.lexer.next();
                self.parse_function(false, FunctionKind::Normal)
            }
            Some(Token::Async) => {
                self.lexer.next();
                self.expect(Token::Function)?;
                self.parse_function(false, FunctionKind::Async)
            }
            Some(Token::Gen) => {
                self.lexer.next();
                self.expect(Token::Function)?;
                self.parse_function(false, FunctionKind::Generator)
            }
            Some(Token::Class) => self.parse_class(false),
            Some(Token::If) => self.parse_if_statement(),
            Some(Token::While) => self.parse_while(),
            Some(Token::For) => self.parse_for(),
            Some(Token::Continue) if self.scope(ParseScope::Loop) => {
                self.lexer.next();
                self.expect(Token::Semicolon)?;
                Ok(Node::ContinueStatement)
            }
            Some(Token::Break) if self.scope(ParseScope::Loop) => {
                self.lexer.next();
                self.expect(Token::Semicolon)?;
                Ok(Node::BreakStatement)
            }
            Some(Token::Return) if self.scope(ParseScope::Function) => self.parse_return(),
            Some(Token::Throw) => self.parse_throw(),
            Some(Token::Try) => self.parse_try(),
            Some(Token::At) => self.parse_decorators(),
            Some(Token::Import) if self.scope(ParseScope::TopLevel) => self.parse_import(),
            Some(Token::Export) if self.scope(ParseScope::TopLevel) => self.parse_export(),
            _ => {
                let r = self.parse_expression()?;
                self.expect(Token::Semicolon)?;
                Ok(Node::ExpressionStatement(Box::new(r)))
            }
        }
    }

    fn parse_block(&mut self, scope: ParseScope) -> Result<Node, Error> {
        if scope != ParseScope::TopLevel {
            self.expect(Token::LeftBrace)?;
        }
        let saved = self.scope_bits;
        self.scope_bits |= scope as u8;
        self.scope.push(Scope::new(scope));
        let mut statements = Vec::new();
        while !self.eat(Token::RightBrace) {
            match self.parse_statement() {
                Ok(s) => statements.push(s),
                Err(Error::NormalEOF) if scope == ParseScope::TopLevel => break,
                Err(e) => {
                    self.scope_bits = saved;
                    self.scope.pop();
                    return Err(e);
                }
            }
        }
        let scope = self.scope.pop().unwrap();
        self.scope_bits = saved;
        Ok(Node::Block(scope, statements))
    }

    fn parse_lexical_declaration(&mut self) -> Result<Node, Error> {
        let mutable = if self.eat(Token::Let) {
            true
        } else if self.eat(Token::Const) {
            false
        } else {
            return Err(Error::UnexpectedToken);
        };
        let name = self.parse_identifier(false)?;
        self.declare(name.as_str(), mutable)?;
        self.expect(Token::Operator(Operator::Assign))?;
        let init = self.parse_expression()?;
        self.expect(Token::Semicolon)?;
        Ok(Node::LexicalInitialization(name, Box::new(init)))
    }

    fn parse_function(&mut self, expression: bool, kind: FunctionKind) -> Result<Node, Error> {
        let name = if expression {
            if let Some(Token::Identifier(..)) = self.lexer.peek() {
                Some(self.parse_identifier(false)?)
            } else {
                None
            }
        } else {
            Some(self.parse_identifier(false)?)
        };
        self.expect(Token::LeftParen)?;
        let args = self.parse_parameters(Token::RightParen)?;
        let body = self.parse_block(match kind {
            FunctionKind::Normal => ParseScope::Function,
            FunctionKind::Async => ParseScope::AsyncFunction,
            FunctionKind::Generator => ParseScope::GeneratorFunction,
            _ => unreachable!(),
        })?;
        Ok(if expression {
            Node::FunctionExpression(kind, name, args, Box::new(body))
        } else {
            let name = name.unwrap();
            self.declare(name.as_str(), false)?;
            Node::FunctionDeclaration(kind, name, args, Box::new(body))
        })
    }

    fn parse_if_statement(&mut self) -> Result<Node, Error> {
        self.expect(Token::If)?;
        let test = self.parse_expression()?;
        let consequent = self.parse_block(ParseScope::Block)?;
        if self.eat(Token::Else) {
            let alternative = if self.lexer.peek() == Some(&Token::If) {
                self.parse_if_statement()?
            } else {
                self.parse_block(ParseScope::Block)?
            };
            Ok(Node::IfStatement(
                Box::new(test),
                Box::new(consequent),
                Some(Box::new(alternative)),
            ))
        } else {
            Ok(Node::IfStatement(
                Box::new(test),
                Box::new(consequent),
                None,
            ))
        }
    }

    fn parse_while(&mut self) -> Result<Node, Error> {
        self.expect(Token::While)?;
        let test = self.parse_expression()?;
        let body = self.parse_block(ParseScope::Loop)?;
        Ok(Node::WhileLoop(Box::new(test), Box::new(body)))
    }

    fn parse_for(&mut self) -> Result<Node, Error> {
        self.expect(Token::For)?;
        let r#async = self.eat(Token::Await);
        let binding = self.parse_identifier(false)?;
        self.expect(Token::In)?;
        let target = self.parse_assignment_expression()?;
        let body = self.parse_block(ParseScope::Loop)?;
        Ok(Node::ForLoop(
            r#async,
            binding,
            Box::new(target),
            Box::new(body),
        ))
    }

    fn parse_return(&mut self) -> Result<Node, Error> {
        self.expect(Token::Return)?;
        if self.eat(Token::Semicolon) {
            Ok(Node::ReturnStatement(None))
        } else {
            let expr = self.parse_expression()?;
            self.expect(Token::Semicolon)?;
            Ok(Node::ReturnStatement(Some(Box::new(
                if let Node::CallExpression(callee, arguments) = expr {
                    Node::TailCallExpression(callee, arguments)
                } else {
                    expr
                },
            ))))
        }
    }

    fn parse_throw(&mut self) -> Result<Node, Error> {
        self.expect(Token::Throw)?;
        let expr = self.parse_expression()?;
        self.expect(Token::Semicolon)?;
        Ok(Node::ThrowStatement(Box::new(expr)))
    }

    fn parse_try(&mut self) -> Result<Node, Error> {
        self.expect(Token::Try)?;
        let try_clause = Box::new(self.parse_block(ParseScope::Block)?);
        if self.eat(Token::Finally) {
            let finally_clause = self.parse_block(ParseScope::Block)?;
            Ok(Node::TryStatement(
                try_clause,
                None,
                None,
                Some(Box::new(finally_clause)),
            ))
        } else {
            self.expect(Token::Catch)?;
            let binding = if let Some(Token::Identifier(..)) = self.lexer.peek() {
                Some(self.parse_identifier(false)?)
            } else {
                None
            };
            let catch_clause = Box::new(self.parse_block(ParseScope::Block)?);
            let finally_clause = if self.eat(Token::Finally) {
                Some(Box::new(self.parse_block(ParseScope::Block)?))
            } else {
                None
            };
            Ok(Node::TryStatement(
                try_clause,
                binding,
                Some(catch_clause),
                finally_clause,
            ))
        }
    }

    fn parse_decorators(&mut self) -> Result<Node, Error> {
        let mut decorators = VecDeque::new();
        while self.eat(Token::At) {
            let d = self.parse_left_hand_side_expression()?;
            decorators.push_front(d);
        }
        let kind = if self.eat(Token::Async) {
            FunctionKind::Async
        } else if self.eat(Token::Gen) {
            FunctionKind::Generator
        } else if self.eat(Token::Function) {
            FunctionKind::Normal
        } else {
            return Err(Error::UnexpectedToken);
        };
        if let Node::FunctionDeclaration(kind, name, args, body) =
            self.parse_function(false, kind)?
        {
            let mut top = Node::FunctionExpression(kind, None, args, body);
            for d in decorators {
                top = Node::CallExpression(Box::new(d), vec![top]);
            }
            Ok(Node::LexicalInitialization(name, Box::new(top)))
        } else {
            unreachable!();
        }
    }

    fn parse_import(&mut self) -> Result<Node, Error> {
        self.expect(Token::Import)?;
        match self.lexer.peek() {
            // import "specifier";
            Some(Token::StringLiteral(..)) => {
                let specifier = match self.lexer.next() {
                    Some(Token::StringLiteral(s)) => s,
                    _ => unreachable!(),
                };
                self.lexer.next();
                self.expect(Token::Semicolon)?;
                Ok(Node::ImportDeclaration(specifier))
            }

            // import { x, y } from "specifier";
            // import { x, y } from standard:xy;
            Some(Token::LeftBrace) => {
                self.lexer.next();
                let bindings = self.parse_identifier_list(Token::RightBrace)?;
                self.expect(Token::From)?;
                match self.lexer.next() {
                    Some(Token::StringLiteral(s)) => {
                        self.expect(Token::Semicolon)?;
                        Ok(Node::ImportNamedDeclaration(s, bindings))
                    }
                    Some(Token::Identifier(ref s)) if s == "standard" => {
                        self.expect(Token::Colon)?;
                        let namespace = self.parse_identifier(true)?;
                        self.expect(Token::Semicolon)?;
                        Ok(Node::ImportStandardDeclaration(namespace, bindings))
                    }
                    _ => Err(Error::UnexpectedToken),
                }
            }

            // import x from "specifier";
            // import x from standard:xy;
            Some(Token::Identifier(..)) => {
                let binding = self.parse_identifier(false)?;
                self.expect(Token::From)?;
                let specifier = match self.lexer.next() {
                    Some(Token::StringLiteral(s)) => s,
                    _ => unreachable!(),
                };
                self.expect(Token::Semicolon)?;
                Ok(Node::ImportDefaultDeclaration(specifier, binding))
            }

            _ => Err(Error::UnexpectedToken),
        }
    }

    fn parse_export(&mut self) -> Result<Node, Error> {
        self.expect(Token::Export)?;
        let decl = match self.lexer.peek() {
            Some(Token::Let) | Some(Token::Const) => self.parse_lexical_declaration(),
            Some(Token::Function) => {
                self.lexer.next();
                self.parse_function(false, FunctionKind::Normal)
            }
            _ => Err(Error::UnexpectedToken),
        }?;
        Ok(Node::ExportDeclaration(Box::new(decl)))
    }

    fn parse_expression(&mut self) -> Result<Node, Error> {
        self.parse_assignment_expression()
    }

    fn parse_assignment_expression(&mut self) -> Result<Node, Error> {
        if self.eat(Token::Yield) && self.scope(ParseScope::GeneratorFunction) {
            match self.lexer.peek() {
                Some(Token::Semicolon)
                | Some(Token::RightBrace)
                | Some(Token::RightBracket)
                | Some(Token::RightParen)
                | Some(Token::Colon)
                | Some(Token::Comma) => {
                    return Ok(Node::YieldExpression(None));
                }
                _ => {
                    let exp = self.parse_assignment_expression()?;
                    return Ok(Node::YieldExpression(Some(Box::new(exp))));
                }
            }
        }
        let mut lhs = self.parse_conditional_expression()?;

        macro_rules! op_assign {
            ($op:expr) => {{
                self.lexer.next();
                self.check_assignment_target(&lhs)?;
                let rhs = self.parse_assignment_expression()?;
                lhs = Node::BinaryExpression($op, Box::new(lhs), Box::new(rhs));
            }};
        }

        self.lexer.peek();
        match self.lexer.peek_immutable() {
            Some(Token::Operator(Operator::Assign)) => op_assign!(Operator::Assign),
            Some(Token::Operator(Operator::AddAssign)) => op_assign!(Operator::AddAssign),
            Some(Token::Operator(Operator::SubAssign)) => op_assign!(Operator::SubAssign),
            Some(Token::Operator(Operator::MulAssign)) => op_assign!(Operator::MulAssign),
            Some(Token::Operator(Operator::PowAssign)) => op_assign!(Operator::PowAssign),
            Some(Token::Operator(Operator::DivAssign)) => op_assign!(Operator::DivAssign),
            Some(Token::Operator(Operator::ModAssign)) => op_assign!(Operator::ModAssign),
            _ => {}
        }

        Ok(lhs)
    }

    fn check_assignment_target(&self, node: &Node) -> Result<(), Error> {
        match node {
            Node::Identifier(..) => Ok(()),
            Node::MemberExpression(..) => Ok(()),
            Node::ComputedMemberExpression(..) => Ok(()),
            _ => Err(Error::InvalidAssignmentTarget),
        }
    }

    fn parse_conditional_expression(&mut self) -> Result<Node, Error> {
        let lhs = self.parse_logical_or_expression()?;
        if self.eat(Token::Question) {
            let consequent = self.parse_assignment_expression()?;
            self.expect(Token::Colon)?;
            let alternative = self.parse_assignment_expression()?;
            return Ok(Node::ConditionalExpression(
                Box::new(lhs),
                Box::new(consequent),
                Box::new(alternative),
            ));
        }
        Ok(lhs)
    }

    binop_production!(
        parse_logical_or_expression,
        parse_logical_and_expression,
        [Operator::LogicalOR]
    );

    binop_production!(
        parse_logical_and_expression,
        parse_bitwise_or_expression,
        [Operator::LogicalAND]
    );

    binop_production!(
        parse_bitwise_or_expression,
        parse_bitwise_xor_expression,
        [Operator::BitwiseOR]
    );

    binop_production!(
        parse_bitwise_xor_expression,
        parse_bitwise_and_expression,
        [Operator::BitwiseXOR]
    );

    binop_production!(
        parse_bitwise_and_expression,
        parse_equality_expression,
        [Operator::BitwiseAND]
    );

    binop_production!(
        parse_equality_expression,
        parse_relational_expression,
        [Operator::Equal, Operator::NotEqual]
    );

    binop_production!(
        parse_relational_expression,
        parse_shift_expression,
        [
            Operator::LessThan,
            Operator::GreaterThan,
            Operator::LessThanOrEqual,
            Operator::GreaterThanOrEqual
        ]
    );

    binop_production!(
        parse_shift_expression,
        parse_additive_expression,
        [Operator::LeftShift, Operator::RightShift]
    );

    binop_production!(
        parse_additive_expression,
        parse_multiplicate_expression,
        [Operator::Add, Operator::Sub]
    );

    binop_production!(
        parse_multiplicate_expression,
        parse_exponentiation_expression,
        [Operator::Mul, Operator::Div, Operator::Mod]
    );

    binop_production!(
        parse_exponentiation_expression,
        parse_unary_expression,
        [Operator::Pow]
    );

    fn parse_unary_expression(&mut self) -> Result<Node, Error> {
        self.lexer.peek();
        match self.lexer.peek_immutable() {
            Some(Token::Operator(Operator::Add)) => {
                self.lexer.next();
                let expr = self.parse_unary_expression()?;
                Ok(Node::UnaryExpression(Operator::Add, Box::new(expr)))
            }
            Some(Token::Operator(Operator::Sub)) => {
                self.lexer.next();
                let expr = self.parse_unary_expression()?;
                Ok(Node::UnaryExpression(Operator::Sub, Box::new(expr)))
            }
            Some(Token::Operator(Operator::BitwiseNOT)) => {
                self.lexer.next();
                let expr = self.parse_unary_expression()?;
                Ok(Node::UnaryExpression(Operator::BitwiseNOT, Box::new(expr)))
            }
            Some(Token::Operator(Operator::Not)) => {
                self.lexer.next();
                let expr = self.parse_unary_expression()?;
                Ok(Node::UnaryExpression(Operator::Not, Box::new(expr)))
            }
            Some(Token::Operator(Operator::Typeof)) => {
                self.lexer.next();
                let expr = self.parse_unary_expression()?;
                Ok(Node::UnaryExpression(Operator::Typeof, Box::new(expr)))
            }
            Some(Token::Operator(Operator::Void)) => {
                self.lexer.next();
                let expr = self.parse_unary_expression()?;
                Ok(Node::UnaryExpression(Operator::Void, Box::new(expr)))
            }
            Some(Token::Await) if self.scope(ParseScope::AsyncFunction) => {
                self.lexer.next();
                let expr = self.parse_unary_expression()?;
                Ok(Node::AwaitExpression(Box::new(expr)))
            }
            _ => self.parse_left_hand_side_expression(),
        }
    }

    fn parse_left_hand_side_expression(&mut self) -> Result<Node, Error> {
        let mut base = self.parse_primary_expression()?;
        loop {
            if self.eat(Token::Dot) {
                let property = self.parse_identifier(true)?;
                base = Node::MemberExpression(Box::new(base), property);
            } else if self.eat(Token::LeftBracket) {
                let property = self.parse_expression()?;
                self.expect(Token::RightBracket)?;
                base = Node::ComputedMemberExpression(Box::new(base), Box::new(property));
            } else if self.eat(Token::LeftParen) {
                let list = self.parse_expression_list(Token::RightParen)?;
                base = Node::CallExpression(Box::new(base), list);
            } else {
                return Ok(base);
            }
        }
    }

    fn parse_identifier(&mut self, allow_keyword: bool) -> Result<String, Error> {
        match self.lexer.next() {
            Some(Token::Identifier(name)) => Ok(name),
            Some(Token::Throw) if allow_keyword => Ok("throw".to_string()),
            Some(Token::Catch) if allow_keyword => Ok("catch".to_string()),
            Some(Token::True) if allow_keyword => Ok("true".to_string()),
            Some(Token::False) if allow_keyword => Ok("false".to_string()),
            Some(Token::Null) if allow_keyword => Ok("null".to_string()),
            Some(Token::This) if allow_keyword => Ok("this".to_string()),
            Some(Token::Class) if allow_keyword => Ok("class".to_string()),
            Some(Token::Extends) if allow_keyword => Ok("extends".to_string()),
            Some(Token::Finally) if allow_keyword => Ok("finally".to_string()),
            Some(Token::Function) if allow_keyword => Ok("function".to_string()),
            Some(Token::Let) if allow_keyword => Ok("let".to_string()),
            Some(Token::Const) if allow_keyword => Ok("const".to_string()),
            Some(Token::Throw) if allow_keyword => Ok("throw".to_string()),
            Some(Token::Return) if allow_keyword => Ok("return".to_string()),
            Some(Token::While) if allow_keyword => Ok("while".to_string()),
            Some(Token::For) if allow_keyword => Ok("for".to_string()),
            Some(Token::In) if allow_keyword => Ok("in".to_string()),
            Some(Token::Break) if allow_keyword => Ok("break".to_string()),
            Some(Token::Continue) if allow_keyword => Ok("continue".to_string()),
            Some(Token::Try) if allow_keyword => Ok("try".to_string()),
            Some(Token::Catch) if allow_keyword => Ok("catch".to_string()),
            Some(Token::If) if allow_keyword => Ok("if".to_string()),
            Some(Token::Else) if allow_keyword => Ok("else".to_string()),
            Some(Token::New) if allow_keyword => Ok("new".to_string()),
            Some(Token::Import) if allow_keyword => Ok("import".to_string()),
            Some(Token::Export) if allow_keyword => Ok("export".to_string()),
            Some(Token::Default) if allow_keyword => Ok("default".to_string()),
            Some(Token::From) if allow_keyword => Ok("from".to_string()),
            Some(Token::Async) if allow_keyword => Ok("async".to_string()),
            Some(Token::Await) if allow_keyword => Ok("await".to_string()),
            Some(Token::Gen) if allow_keyword => Ok("gen".to_string()),
            Some(Token::Yield) if allow_keyword => Ok("yield".to_string()),
            Some(Token::Operator(Operator::Typeof)) if allow_keyword => Ok("typeof".to_string()),
            Some(Token::Operator(Operator::Void)) if allow_keyword => Ok("void".to_string()),
            _ => Err(Error::UnexpectedToken),
        }
    }

    fn parse_primary_expression(&mut self) -> Result<Node, Error> {
        let token = self.lexer.next();
        match token {
            Some(Token::Null) => Ok(Node::NullLiteral),
            Some(Token::True) => Ok(Node::TrueLiteral),
            Some(Token::False) => Ok(Node::FalseLiteral),
            Some(Token::StringLiteral(v)) => Ok(Node::StringLiteral(v)),
            Some(Token::NumberLiteral(v)) => Ok(Node::NumberLiteral(v)),
            Some(Token::Colon) => {
                let name = self.parse_identifier(false)?;
                Ok(Node::SymbolLiteral(name))
            }
            Some(Token::Operator(Operator::Div)) => {
                let mut pattern = String::new();
                loop {
                    match self.lexer.chars.next() {
                        Some('/') => break,
                        Some('\\') => {
                            pattern.push('\\');
                            pattern.push(self.lexer.chars.next().unwrap());
                        }
                        Some(c) => {
                            pattern.push(c);
                        }
                        None => return Err(Error::UnexpectedEOF),
                    }
                }
                Ok(Node::RegexLiteral(pattern))
            }
            Some(Token::This) => Ok(Node::ThisExpression),
            Some(Token::New) => {
                let expr = self.parse_left_hand_side_expression()?;
                Ok(Node::NewExpression(Box::new(expr)))
            }
            Some(Token::Identifier(i)) => Ok(Node::Identifier(i)),
            Some(Token::LeftBracket) => {
                let exprs = self.parse_expression_list(Token::RightBracket)?;
                Ok(Node::ArrayLiteral(exprs))
            }
            Some(Token::LeftBrace) => {
                let mut fields = Vec::new();
                let mut first = true;
                while !self.eat(Token::RightBrace) {
                    if first {
                        first = false;
                    } else {
                        self.expect(Token::Comma)?;
                        if self.eat(Token::RightBrace) {
                            break;
                        }
                    }
                    let name = if self.eat(Token::LeftBracket) {
                        let name = self.parse_expression()?;
                        self.expect(Token::RightBracket)?;
                        name
                    } else {
                        Node::StringLiteral(self.parse_identifier(true)?)
                    };
                    let mut init;
                    if self.eat(Token::Colon) {
                        init = self.parse_expression()?;
                    } else {
                        init = self.parse_function(true, FunctionKind::Normal)?
                    }
                    fields.push(Node::Initializer(Box::new(name), Box::new(init)));
                }
                Ok(Node::ObjectLiteral(fields))
            }
            Some(Token::LeftParen) => {
                let mut list = self.parse_expression_list(Token::RightParen)?;
                if self.eat(Token::Arrow) {
                    // ( ... ) =>
                    self.parse_arrow_function(FunctionKind::Normal, list)
                } else if list.is_empty() {
                    // ( )
                    Err(Error::UnexpectedToken)
                } else if list.len() == 1 {
                    // ( expr )
                    Ok(Node::ParenthesizedExpression(Box::new(list.pop().unwrap())))
                } else {
                    // ( expr, expr )
                    Ok(Node::TupleLiteral(list))
                }
            }
            Some(Token::Async) => {
                self.expect(Token::LeftParen)?;
                let list = self.parse_parameters(Token::RightParen)?;
                self.expect(Token::Arrow)?;
                self.parse_arrow_function(FunctionKind::Async, list)
            }
            Some(Token::Class) => self.parse_class(true),
            _ => Err(Error::UnexpectedToken),
        }
    }

    fn parse_class(&mut self, expression: bool) -> Result<Node, Error> {
        if !expression {
            self.expect(Token::Class)?;
        }
        let name = self.parse_identifier(false)?;
        if !expression {
            self.declare(&name, false)?;
        }
        let extends = if self.eat(Token::Extends) {
            Some(Box::new(self.parse_left_hand_side_expression()?))
        } else {
            None
        };
        self.expect(Token::LeftBrace)?;
        let mut fields = Vec::new();
        while !self.eat(Token::RightBrace) {
            let name = self.parse_identifier(false)?;
            let f = self.parse_function(true, FunctionKind::Normal)?;
            fields.push(Node::Initializer(
                Box::new(Node::StringLiteral(name)),
                Box::new(f),
            ));
        }
        if expression {
            Ok(Node::ClassExpression(name, extends, fields))
        } else {
            Ok(Node::ClassDeclaration(name, extends, fields))
        }
    }

    fn parse_arrow_function(
        &mut self,
        kind: FunctionKind,
        mut args: Vec<Node>,
    ) -> Result<Node, Error> {
        for item in &mut args {
            match item {
                Node::Identifier(..) | Node::Initializer(..) => {}
                Node::BinaryExpression(op, left, right) if *op == Operator::Assign => {
                    if let Node::Identifier(..) = &**left {
                        let init = Node::Initializer(
                            Box::new(std::mem::replace(&mut **left, Node::NullLiteral)),
                            Box::new(std::mem::replace(&mut **right, Node::NullLiteral)),
                        );
                        std::mem::replace(item, init);
                    } else {
                        return Err(Error::UnexpectedToken);
                    }
                }
                _ => return Err(Error::UnexpectedToken),
            }
        }
        let body = if self.peek(Token::LeftBrace) {
            self.parse_block(match kind {
                FunctionKind::Normal => ParseScope::Function,
                FunctionKind::Async => ParseScope::AsyncFunction,
                FunctionKind::Generator => ParseScope::GeneratorFunction,
                _ => unreachable!(),
            })?
        } else {
            let expr = self.parse_assignment_expression()?;
            Node::Block(
                Scope::new(ParseScope::Function),
                vec![Node::ReturnStatement(Some(Box::new(expr)))],
            )
        };
        Ok(Node::ArrowFunctionExpression(
            kind | FunctionKind::Arrow,
            args,
            Box::new(body),
        ))
    }

    fn parse_expression_list(&mut self, close: Token) -> Result<Vec<Node>, Error> {
        let mut list = Vec::new();
        let mut first = true;
        while !self.eat(close.clone()) {
            if first {
                first = false;
            } else {
                self.expect(Token::Comma)?;
                if self.eat(close.clone()) {
                    break;
                }
            }
            list.push(self.parse_expression()?);
        }
        Ok(list)
    }

    fn parse_identifier_list(&mut self, close: Token) -> Result<Vec<String>, Error> {
        let mut identifiers = Vec::new();
        let mut first = true;
        while !self.eat(close.clone()) {
            if first {
                first = false;
            } else {
                self.expect(Token::Comma)?;
                if self.eat(close.clone()) {
                    break;
                }
            }
            identifiers.push(self.parse_identifier(false)?);
        }
        Ok(identifiers)
    }

    fn parse_parameters(&mut self, close: Token) -> Result<Vec<Node>, Error> {
        let mut parameters = Vec::new();
        let mut first = true;
        while !self.eat(close.clone()) {
            if first {
                first = false;
            } else {
                self.expect(Token::Comma)?;
                if self.eat(close.clone()) {
                    break;
                }
            }
            let ident = self.parse_identifier(false)?;
            if self.lexer.peek() == Some(&Token::Operator(Operator::Assign)) {
                self.lexer.next();
                let init = self.parse_expression()?;
                parameters.push(Node::Initializer(
                    Box::new(Node::Identifier(ident)),
                    Box::new(init),
                ));
            } else {
                parameters.push(Node::Identifier(ident));
            }
        }
        Ok(parameters)
    }
}
