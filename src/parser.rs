use crate::num_util::{f64_band, f64_bnot, f64_bor, f64_bxor, f64_shl, f64_shr};
use crate::{Agent, IntoValue, Value};
use indexmap::IndexMap;
use std::collections::VecDeque;
use std::iter::Peekable;
use std::ops::{Div, Mul, Rem, Sub};
use std::str::Chars;

include!(concat!(env!("OUT_DIR"), "/unicode_name_map_gen.rs"));

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
    Coalesce,
    Assign,
    Equal,
    NotEqual,
    Typeof,
    Void,
    Has,
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
    Generator,
    Import,
    Export,
    Default,
    From,
    Match,

    Operator(Operator),

    EOF,
}

impl Token {
    fn precedence(&self) -> u8 {
        match self {
            Token::Operator(op) => match op {
                Operator::Assign => 2,
                Operator::AddAssign => 2,
                Operator::SubAssign => 2,
                Operator::MulAssign => 2,
                Operator::PowAssign => 2,
                Operator::DivAssign => 2,
                Operator::ModAssign => 2,
                Operator::LogicalOR => 4,
                Operator::LogicalAND => 5,
                Operator::BitwiseOR => 6,
                Operator::BitwiseXOR => 7,
                Operator::BitwiseAND => 8,
                Operator::Equal => 9,
                Operator::NotEqual => 9,
                Operator::LessThan => 10,
                Operator::GreaterThan => 10,
                Operator::LessThanOrEqual => 10,
                Operator::GreaterThanOrEqual => 10,
                Operator::Has => 10,
                Operator::LeftShift => 11,
                Operator::RightShift => 11,
                Operator::Add => 12,
                Operator::Sub => 12,
                Operator::Mul => 13,
                Operator::Div => 13,
                Operator::Mod => 13,
                Operator::Pow => 14,
                _ => 0,
            },
            _ => 0,
        }
    }
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
pub(crate) enum ScopeKind {
    TopLevel,
    Block,
}

#[derive(Debug, PartialEq)]
pub struct Scope {
    pub(crate) kind: ScopeKind,
    pub(crate) bindings: IndexMap<String, bool>,
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Position(usize, usize);

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Span(Position, Position);

impl Span {
    fn combine(&self, other: &Span) -> Span {
        Span(self.0, other.1)
    }

    pub(crate) fn empty() -> Span {
        Span(Position(0, 0), Position(0, 0))
    }
}

#[derive(Debug, PartialEq)]
pub enum Node {
    NullLiteral(Span),
    TrueLiteral(Span),
    FalseLiteral(Span),
    NumberLiteral(f64, Span),
    StringLiteral(String, Span),
    SymbolLiteral(String, Span),
    RegexLiteral(String, Span),
    ObjectLiteral(Vec<Node>, Span),
    ArrayLiteral(Vec<Node>, Span),
    TupleLiteral(Vec<Node>, Span),
    TemplateLiteral(Vec<String>, Vec<Node>, Span),

    Identifier(String, Span),

    Block(Scope, Vec<Node>, Span),

    IfExpression(Box<Node>, Box<Node>, Option<Box<Node>>, Span),

    WhileLoop(Box<Node>, Box<Node>, Span),
    ForLoop(bool, String, Box<Node>, Box<Node>, Span),

    ExpressionStatement(Box<Node>, Span),
    UnaryExpression(Operator, Box<Node>, Span),
    BinaryExpression(Operator, Box<Node>, Box<Node>, Span),
    ParenthesizedExpression(Box<Node>, Span),

    YieldExpression(Option<Box<Node>>, Span),
    AwaitExpression(Box<Node>, Span),
    ThisExpression(Span),
    NewExpression(Box<Node>, Vec<Node>, Span),

    MatchExpression(Box<Node>, Vec<Node>, Span),
    MatchArm(Box<Node>, Box<Node>, Span),
    ObjectPattern(IndexMap<String, String>, bool, Span),
    ArrayPattern(Vec<String>, bool, Span),

    OptionalChain(Box<Node>),
    MemberExpression(Box<Node>, String, bool, Span),
    ComputedMemberExpression(Box<Node>, Box<Node>, bool, Span),
    CallExpression(Box<Node>, Vec<Node>, bool, Span),
    TailCallExpression(Box<Node>, Vec<Node>, bool, Span),

    FunctionExpression(FunctionKind, Option<String>, Vec<Node>, Box<Node>, Span),
    FunctionDeclaration(FunctionKind, String, Vec<Node>, Box<Node>, Span),
    ArrowFunctionExpression(FunctionKind, Vec<Node>, Box<Node>, Span),

    ClassExpression(String, Option<Box<Node>>, Vec<Node>, Span),
    ClassDeclaration(String, Option<Box<Node>>, Vec<Node>, Span),

    LexicalInitialization(String, Box<Node>, Span),

    ReturnStatement(Option<Box<Node>>, Span),
    ThrowStatement(Box<Node>, Span),
    BreakStatement(Span),
    ContinueStatement(Span),
    TryStatement(
        Box<Node>,
        Option<String>,
        Option<Box<Node>>,
        Option<Box<Node>>,
        Span,
    ),

    ImportDeclaration(String, Span),
    ImportNamedDeclaration(String, Vec<String>, Span),
    ImportDefaultDeclaration(String, String, Span),
    ImportStandardDeclaration(String, Vec<String>, Span),
    ExportDeclaration(Box<Node>, Span),

    Initializer(Box<Node>, Box<Node>, Span),
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Error {
    NormalEOF,
    UnexpectedEOF(Position),
    UnexpectedToken(Position),
    DuplicateBinding(Position),
    InvalidAssignmentTarget(Position),
}

impl IntoValue for Error {
    fn into_value(&self, agent: &Agent) -> Value {
        Value::new_error(agent, &format!("{:?}", self))
    }
}

struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    peeked: Option<(Position, Result<Token, Error>)>,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    fn new(code: &'a str) -> Lexer<'a> {
        Lexer {
            chars: code.chars().peekable(),
            peeked: None,
            line: 1,
            column: 0,
        }
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.chars.next();
        if let Some('\n') = c {
            self.column = 0;
            self.line += 1;
        } else if let Some(..) = c {
            self.column += 1;
        }
        c
    }

    fn inner_next(&mut self) -> Result<Token, Error> {
        Ok(match self.next_char() {
            Some(c) => match c {
                ' ' | '\t' | '\n' => self.inner_next()?,
                '0'...'9' => {
                    let start = self.position();
                    if c == '0' && self.chars.peek() != Some(&'.') {
                        let radix = match self.chars.peek() {
                            Some('b') | Some('B') => Some(2),
                            Some('o') | Some('O') => Some(8),
                            Some('x') | Some('X') => Some(16),
                            _ => None,
                        };
                        if let Some(radix) = radix {
                            self.next_char();
                            let mut str = String::new();
                            while let Some(c) = self.chars.peek() {
                                match c {
                                    '_' => {
                                        self.next_char().unwrap();
                                        if self.chars.peek() == Some(&'_') {
                                            return Err(Error::UnexpectedToken(self.position()));
                                        }
                                    }
                                    '0' | '1' => str.push(self.next_char().unwrap()),
                                    '2'...'7' if radix > 7 => str.push(self.next_char().unwrap()),
                                    '8' | '9' if radix > 15 => str.push(self.next_char().unwrap()),
                                    'a'...'f' | 'A'...'F' if radix > 15 => {
                                        str.push(self.next_char().unwrap())
                                    }
                                    _ => break,
                                }
                            }
                            match u64::from_str_radix(&str, radix) {
                                Ok(n) => Token::NumberLiteral(n as f64),
                                Err(_) => return Err(Error::UnexpectedToken(start)),
                            }
                        } else {
                            Token::NumberLiteral(0.0)
                        }
                    } else {
                        let mut str = c.to_string();
                        let mut exp_str = String::new();
                        let mut one_dot = false;
                        let mut in_exp = false;
                        while let Some(c) = self.chars.peek() {
                            match c {
                                '_' => {
                                    self.next_char().unwrap();
                                    if self.chars.peek() == Some(&'_') {
                                        return Err(Error::UnexpectedToken(self.position()));
                                    }
                                    continue;
                                }
                                '0'...'9' => {
                                    if in_exp {
                                        exp_str.push(self.next_char().unwrap());
                                    } else {
                                        str.push(self.next_char().unwrap());
                                    }
                                }
                                'e' if !in_exp => {
                                    self.next_char().unwrap();
                                    in_exp = true;
                                    match self.chars.peek() {
                                        Some('-') => {
                                            self.next_char().unwrap();
                                            exp_str.push('-');
                                        }
                                        Some('+') => {
                                            self.next_char().unwrap();
                                        }
                                        _ => {}
                                    }
                                }
                                '.' if !in_exp => {
                                    if !one_dot {
                                        one_dot = true;
                                        str.push(self.next_char().unwrap());
                                        if self.chars.peek() == Some(&'_') {
                                            return Err(Error::UnexpectedToken(self.position()));
                                        }
                                    } else {
                                        break;
                                    }
                                }
                                _ => break,
                            }
                        }
                        match str.parse::<f64>() {
                            Ok(n) => {
                                if in_exp {
                                    match exp_str.parse::<i32>() {
                                        Ok(e) => Token::NumberLiteral(n * (10f64.powi(e) as f64)),
                                        Err(_) => return Err(Error::UnexpectedToken(start)),
                                    }
                                } else {
                                    Token::NumberLiteral(n)
                                }
                            }
                            Err(_) => return Err(Error::UnexpectedToken(start)),
                        }
                    }
                }
                '"' | '\'' => {
                    let mut str = String::new();
                    while let Some(char) = self.chars.peek() {
                        if *char == c {
                            self.next_char();
                            break;
                        }
                        let c = self.next_char().unwrap();
                        match c {
                            '\\' => match self.next_char().unwrap() {
                                'n' => str.push('\n'),
                                't' => str.push('\t'),
                                '"' => str.push('"'),
                                '\'' => str.push('\''),
                                '\\' => str.push('\\'),
                                'u' => {
                                    if Some('{') != self.next_char() {
                                        return Err(Error::UnexpectedToken(self.position()));
                                    }
                                    let mut n = String::new();
                                    macro_rules! digit {
                                        () => {
                                            let next = self.next_char();
                                            match next {
                                                Some('0'...'9') | Some('a'...'f')
                                                | Some('A'...'F') => {
                                                    n.push(next.unwrap());
                                                }
                                                _ => {
                                                    return Err(Error::UnexpectedToken(
                                                        self.position(),
                                                    ))
                                                }
                                            }
                                        };
                                    }
                                    digit!();
                                    digit!();
                                    digit!();
                                    digit!();
                                    match u32::from_str_radix(n.as_str(), 16) {
                                        Ok(n) => match std::char::from_u32(n) {
                                            Some(c) => str.push(c),
                                            None => {
                                                return Err(Error::UnexpectedToken(self.position()))
                                            }
                                        },
                                        Err(_) => {
                                            return Err(Error::UnexpectedToken(self.position()))
                                        }
                                    }
                                    if Some('}') != self.next_char() {
                                        return Err(Error::UnexpectedToken(self.position()));
                                    }
                                }
                                'U' => {
                                    if Some('{') != self.next_char() {
                                        return Err(Error::UnexpectedToken(self.position()));
                                    }
                                    let mut name = String::new();
                                    loop {
                                        match self.next_char() {
                                            Some('}') => break,
                                            None => {
                                                return Err(Error::UnexpectedEOF(self.position()))
                                            }
                                            Some(c) => name.push(c),
                                        }
                                    }
                                    match UNICODE_NAME_MAP.get(name.as_str()) {
                                        Some(c) => str.push(*c),
                                        None => {
                                            return Err(Error::UnexpectedToken(self.position()))
                                        }
                                    };
                                }
                                _ => return Err(Error::UnexpectedToken(self.position())),
                            },
                            '\r' | '\n' => return Err(Error::UnexpectedToken(self.position())),
                            c => str.push(c),
                        }
                    }
                    Token::StringLiteral(str)
                }
                'a'...'z' | 'A'...'Z' | '_' => {
                    let mut ident = c.to_string();
                    while let Some(c) = self.chars.peek() {
                        match c {
                            'a'...'z' | 'A'...'Z' | '0'...'9' | '_' => {
                                ident.push(self.next_char().unwrap())
                            }
                            _ => break,
                        }
                    }
                    // UPDATE parse_identifier WHEN YOU ADD TO THIS LIST!
                    match ident.as_ref() {
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
                        "generator" => Token::Generator,
                        "yield" => Token::Yield,
                        "match" => Token::Match,
                        "typeof" => Token::Operator(Operator::Typeof),
                        "void" => Token::Operator(Operator::Void),
                        "has" => Token::Operator(Operator::Has),
                        _ => Token::Identifier(ident),
                    }
                }
                '{' => Token::LeftBrace,
                '}' => Token::RightBrace,
                '[' => Token::LeftBracket,
                ']' => Token::RightBracket,
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                ':' => Token::Colon,
                ';' => Token::Semicolon,
                '?' => match self.chars.peek() {
                    Some('?') => {
                        self.next_char();
                        Token::Operator(Operator::Coalesce)
                    }
                    _ => Token::Question,
                },
                '.' => match self.chars.peek() {
                    Some('.') => {
                        self.next_char();
                        if let Some('.') = self.chars.peek() {
                            self.next_char();
                            Token::Ellipsis
                        } else {
                            return Err(Error::UnexpectedToken(self.position()));
                        }
                    }
                    _ => Token::Dot,
                },
                ',' => Token::Comma,
                '`' => Token::BackQuote,
                '+' => match self.chars.peek() {
                    Some('=') => {
                        self.next_char();
                        Token::Operator(Operator::AddAssign)
                    }
                    _ => Token::Operator(Operator::Add),
                },
                '-' => match self.chars.peek() {
                    Some('=') => {
                        self.next_char();
                        Token::Operator(Operator::SubAssign)
                    }
                    _ => Token::Operator(Operator::Sub),
                },
                '*' => match self.chars.peek() {
                    Some('*') => {
                        self.next_char();
                        match self.chars.peek() {
                            Some('=') => {
                                self.next_char();
                                Token::Operator(Operator::PowAssign)
                            }
                            _ => Token::Operator(Operator::Pow),
                        }
                    }
                    _ => match self.chars.peek() {
                        Some('=') => {
                            self.next_char();
                            Token::Operator(Operator::MulAssign)
                        }
                        _ => Token::Operator(Operator::Mul),
                    },
                },
                '/' => match self.chars.peek() {
                    Some('=') => {
                        self.next_char();
                        Token::Operator(Operator::DivAssign)
                    }
                    Some('*') => {
                        self.next_char();
                        let mut depth = 1;
                        loop {
                            if self.chars.peek() == None {
                                return Err(Error::UnexpectedEOF(self.position()));
                            }
                            let c1 = self.next_char();
                            if let Some('/') = c1 {
                                if let Some('*') = self.next_char() {
                                    depth += 1;
                                }
                            } else if let Some('*') = c1 {
                                if let Some('/') = self.next_char() {
                                    depth -= 1;
                                    if depth == 0 {
                                        break;
                                    }
                                }
                            }
                        }
                        self.next()?
                    }
                    Some('/') => {
                        loop {
                            if self.chars.peek() == None {
                                return Err(Error::UnexpectedEOF(self.position()));
                            }
                            if let Some('\n') = self.next_char() {
                                break;
                            }
                        }
                        self.next()?
                    }
                    _ => Token::Operator(Operator::Div),
                },
                '%' => match self.chars.peek() {
                    Some('=') => {
                        self.next_char();
                        Token::Operator(Operator::ModAssign)
                    }
                    _ => Token::Operator(Operator::Mod),
                },
                '<' => match self.chars.peek() {
                    Some('<') => {
                        self.next_char();
                        Token::Operator(Operator::LeftShift)
                    }
                    Some('=') => {
                        self.next_char();
                        Token::Operator(Operator::LessThanOrEqual)
                    }
                    _ => Token::Operator(Operator::LessThan),
                },
                '!' => match self.chars.peek() {
                    Some('=') => {
                        self.next_char();
                        Token::Operator(Operator::NotEqual)
                    }
                    _ => Token::Operator(Operator::Not),
                },
                '>' => match self.chars.peek() {
                    Some('>') => {
                        self.next_char();
                        Token::Operator(Operator::RightShift)
                    }
                    Some('=') => {
                        self.next_char();
                        Token::Operator(Operator::GreaterThanOrEqual)
                    }
                    _ => Token::Operator(Operator::GreaterThan),
                },
                '&' => match self.chars.peek() {
                    Some('&') => {
                        self.next_char();
                        Token::Operator(Operator::LogicalAND)
                    }
                    _ => Token::Operator(Operator::BitwiseAND),
                },
                '|' => match self.chars.peek() {
                    Some('|') => {
                        self.next_char();
                        Token::Operator(Operator::LogicalOR)
                    }
                    _ => Token::Operator(Operator::BitwiseOR),
                },
                '^' => Token::Operator(Operator::BitwiseXOR),
                '~' => Token::Operator(Operator::BitwiseNOT),
                '=' => match self.chars.peek() {
                    Some('=') => {
                        self.next_char();
                        Token::Operator(Operator::Equal)
                    }
                    Some('>') => {
                        self.next_char();
                        Token::Arrow
                    }
                    _ => Token::Operator(Operator::Assign),
                },
                '@' => Token::At,
                _ => return Err(Error::UnexpectedToken(self.position())),
            },
            None => Token::EOF,
        })
    }

    fn next(&mut self) -> Result<Token, Error> {
        match self.peeked.take() {
            Some(v) => v.1,
            None => self.inner_next(),
        }
    }

    pub(crate) fn peek_immutable(&self) -> Result<&Token, Error> {
        match self.peeked {
            Some((_p, Ok(ref value))) => Ok(value),
            Some((_p, Err(e))) => Err(e),
            None => unreachable!(),
        }
    }

    pub(crate) fn peek(&mut self) -> Result<&Token, Error> {
        if self.peeked.is_none() {
            let position = self.position();
            self.peeked = Some((position, self.next()));
        }
        self.peek_immutable()
    }

    fn skip_hashbang(&mut self) {
        if self.chars.peek() == Some(&'#') {
            self.next_char();
            if self.chars.peek() == Some(&'!') {
                loop {
                    match self.next_char() {
                        Some('\n') | None => break,
                        _ => {}
                    }
                }
            }
        }
    }

    fn position(&self) -> Position {
        if let Some((p, ..)) = self.peeked {
            p
        } else {
            Position(self.line, self.column)
        }
    }
}

fn constant_fold(op: Operator, left: &Node, right: &Node) -> Option<Node> {
    macro_rules! num_binop_num {
        ($fn:expr) => {
            match left {
                Node::NumberLiteral(ln, ls) => match right {
                    Node::NumberLiteral(rn, rs) => {
                        Some(Node::NumberLiteral($fn(*ln, *rn), ls.combine(rs)))
                    }
                    _ => None,
                },
                _ => None,
            }
        };
    }

    macro_rules! num_binop_bool {
        ($fn:expr) => {
            match left {
                Node::NumberLiteral(ln, ls) => match right {
                    Node::NumberLiteral(rn, rs) => Some(if $fn(ln, rn) {
                        Node::TrueLiteral(ls.combine(rs))
                    } else {
                        Node::FalseLiteral(ls.combine(rs))
                    }),
                    _ => None,
                },
                _ => None,
            }
        };
    }

    if let Node::ParenthesizedExpression(e, ..) = left {
        return constant_fold(op, e, right);
    }

    if let Node::ParenthesizedExpression(e, ..) = right {
        return constant_fold(op, left, e);
    }

    if let Node::UnaryExpression(Operator::Typeof, _v, s) = left {
        if let Node::StringLiteral(check, ..) = right {
            match check.as_str() {
                "null" | "boolean" | "number" | "string" | "symbol" | "tuple" | "object"
                | "function" => {}
                _ => return Some(Node::FalseLiteral(*s)),
            }
        } else {
            return Some(Node::FalseLiteral(*s));
        }
    }

    match op {
        Operator::Add => match left {
            Node::StringLiteral(lhs, ls) => match right {
                Node::StringLiteral(rhs, rs) => Some(Node::StringLiteral(
                    format!("{}{}", lhs, rhs),
                    ls.combine(rs),
                )),
                _ => None,
            },
            Node::NumberLiteral(lhs, ls) => match right {
                Node::NumberLiteral(rhs, rs) => {
                    Some(Node::NumberLiteral(lhs + rhs, ls.combine(rs)))
                }
                _ => None,
            },
            _ => None,
        },
        Operator::Sub => num_binop_num!(f64::sub),
        Operator::Mul => num_binop_num!(f64::mul),
        Operator::Div => num_binop_num!(f64::div),
        Operator::Mod => num_binop_num!(f64::rem),
        Operator::Pow => num_binop_num!(f64::powf),
        Operator::BitwiseOR => num_binop_num!(f64_bor),
        Operator::BitwiseXOR => num_binop_num!(f64_bxor),
        Operator::BitwiseAND => num_binop_num!(f64_band),
        Operator::BitwiseNOT => match left {
            Node::NumberLiteral(n, s) => Some(Node::NumberLiteral(f64_bnot(*n), *s)),
            _ => None,
        },
        Operator::LeftShift => num_binop_num!(f64_shl),
        Operator::RightShift => num_binop_num!(f64_shr),
        Operator::GreaterThan => num_binop_bool!(f64::gt),
        Operator::LessThan => num_binop_bool!(f64::lt),
        Operator::GreaterThanOrEqual => num_binop_bool!(f64::ge),
        Operator::LessThanOrEqual => num_binop_bool!(f64::le),
        Operator::Not => match constant_truthy(left) {
            Some(true) => Some(Node::FalseLiteral(Span::empty())),
            Some(false) => Some(Node::TrueLiteral(Span::empty())),
            None => None,
        },
        Operator::Equal => {
            if left == right {
                Some(Node::TrueLiteral(Span::empty()))
            } else {
                None
            }
        }
        Operator::NotEqual => match constant_fold(Operator::Equal, left, right) {
            Some(Node::TrueLiteral(s)) => Some(Node::FalseLiteral(s)),
            Some(Node::FalseLiteral(s)) => Some(Node::TrueLiteral(s)),
            None => None,
            _ => unreachable!(),
        },
        Operator::Typeof => match left {
            Node::NullLiteral(s) => Some(Node::StringLiteral("null".to_string(), *s)),
            Node::TrueLiteral(s) | Node::FalseLiteral(s) => {
                Some(Node::StringLiteral("boolean".to_string(), *s))
            }
            Node::NumberLiteral(_, s) => Some(Node::StringLiteral("number".to_string(), *s)),
            Node::StringLiteral(_, s) => Some(Node::StringLiteral("string".to_string(), *s)),
            Node::SymbolLiteral(_, s) => Some(Node::StringLiteral("symbol".to_string(), *s)),
            Node::TupleLiteral(_, s) => Some(Node::StringLiteral("tuple".to_string(), *s)),
            Node::ObjectLiteral(_, s) | Node::ArrayLiteral(_, s) => {
                Some(Node::StringLiteral("object".to_string(), *s))
            }
            _ => None,
        },
        _ => None,
    }
}

fn constant_truthy(node: &Node) -> Option<bool> {
    match node {
        Node::ParenthesizedExpression(e, ..) => constant_truthy(e),
        Node::NullLiteral(..) => Some(false),
        Node::TrueLiteral(..) => Some(true),
        Node::FalseLiteral(..) => Some(false),
        Node::StringLiteral(s, ..) => Some(!s.is_empty()),
        Node::NumberLiteral(n, ..) => Some(*n != 0.0),
        Node::SymbolLiteral(..) => Some(true),
        Node::ArrayLiteral(..) | Node::TupleLiteral(..) | Node::ObjectLiteral(..) => Some(true),
        _ => None,
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

        if let Node::Block(scope, mut stmts, span) = parser.parse_block(ParseScope::TopLevel)? {
            if let Some(Node::ExpressionStatement(..)) = stmts.last() {
                // if the last item is an expression statement, replace it with the expression
                // so that the value will be left on the stack to inspect in tests
                if let Node::ExpressionStatement(expr, sp) = stmts.pop().unwrap() {
                    stmts.push(Node::ParenthesizedExpression(expr, sp));
                    Ok(Node::Block(scope, stmts, span))
                } else {
                    unreachable!();
                }
            } else {
                Ok(Node::Block(scope, stmts, span))
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
            Err(Error::DuplicateBinding(self.lexer.position()))
        }
    }

    fn peek(&mut self, token: Token) -> bool {
        self.lexer.peek() == Ok(&token)
    }

    #[inline]
    fn eat(&mut self, token: Token) -> bool {
        if self.peek(token) {
            self.lexer.next().unwrap();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, token: Token) -> Result<Token, Error> {
        let pos = self.lexer.position();
        match self.lexer.next()? {
            ref t if t == &token => Ok(token),
            Token::EOF => Err(Error::UnexpectedEOF(pos)),
            _ => Err(Error::UnexpectedToken(pos)),
        }
    }

    fn build_binary(&self, op: Operator, left: Node, right: Node, span: Span) -> Node {
        if let Some(node) = constant_fold(op, &left, &right) {
            node
        } else {
            Node::BinaryExpression(op, Box::new(left), Box::new(right), span)
        }
    }

    fn build_unary(&self, op: Operator, node: Node, span: Span) -> Node {
        if let Some(node) = constant_fold(op, &node, &Node::NullLiteral(Span::empty())) {
            node
        } else {
            Node::UnaryExpression(op, Box::new(node), span)
        }
    }

    fn parse_statement(&mut self) -> Result<Node, Error> {
        self.lexer.peek()?;
        match self.lexer.peek_immutable()? {
            Token::EOF => Err(Error::NormalEOF),
            Token::LeftBrace => self.parse_block(ParseScope::Block),
            Token::Let | Token::Const => self.parse_lexical_declaration(),
            Token::Function => {
                let start = self.lexer.position();
                self.lexer.next()?;
                self.parse_function(false, FunctionKind::Normal, start)
            }
            Token::Async => {
                let start = self.lexer.position();
                self.lexer.next()?;
                self.expect(Token::Function)?;
                self.parse_function(false, FunctionKind::Async, start)
            }
            Token::Generator => {
                let start = self.lexer.position();
                self.lexer.next()?;
                self.parse_function(false, FunctionKind::Generator, start)
            }
            Token::Class => self.parse_class(false),
            Token::While => self.parse_while(),
            Token::For => self.parse_for(),
            Token::Continue if self.scope(ParseScope::Loop) => {
                let start = self.lexer.position();
                self.lexer.next()?;
                self.expect(Token::Semicolon)?;
                Ok(Node::ContinueStatement(Span(start, self.lexer.position())))
            }
            Token::Break if self.scope(ParseScope::Loop) => {
                let start = self.lexer.position();
                self.lexer.next()?;
                self.expect(Token::Semicolon)?;
                Ok(Node::BreakStatement(Span(start, self.lexer.position())))
            }
            Token::Return if self.scope(ParseScope::Function) => self.parse_return(),
            Token::Throw => self.parse_throw(),
            Token::Try => self.parse_try(),
            Token::At => self.parse_decorators(),
            Token::Import if self.scope(ParseScope::TopLevel) => self.parse_import(),
            Token::Export if self.scope(ParseScope::TopLevel) => self.parse_export(),
            Token::If => {
                let start = self.lexer.position();
                self.lexer.next()?;
                let expr = self.parse_if_expression(start)?;
                Ok(Node::ExpressionStatement(
                    Box::new(expr),
                    Span(start, self.lexer.position()),
                ))
            }
            _ => {
                let start = self.lexer.position();
                let expr = self.parse_expression()?;
                self.expect(Token::Semicolon)?;
                Ok(Node::ExpressionStatement(
                    Box::new(expr),
                    Span(start, self.lexer.position()),
                ))
            }
        }
    }

    fn parse_block(&mut self, scope: ParseScope) -> Result<Node, Error> {
        let start = self.lexer.position();
        if scope != ParseScope::TopLevel {
            self.expect(Token::LeftBrace)?;
        }
        let saved = self.scope_bits;
        self.scope_bits = if ((scope as u8) & (ParseScope::Function as u8)) != 0 {
            scope as u8
        } else {
            self.scope_bits | (scope as u8)
        };
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
        Ok(Node::Block(
            scope,
            statements,
            Span(start, self.lexer.position()),
        ))
    }

    fn parse_lexical_declaration(&mut self) -> Result<Node, Error> {
        let start = self.lexer.position();
        let mutable = if self.eat(Token::Let) {
            true
        } else if self.eat(Token::Const) {
            false
        } else {
            return Err(Error::UnexpectedToken(start));
        };
        let name = self.parse_identifier(false)?;
        self.declare(name.as_str(), mutable)?;
        self.expect(Token::Operator(Operator::Assign))?;
        let init = self.parse_expression()?;
        self.expect(Token::Semicolon)?;
        Ok(Node::LexicalInitialization(
            name,
            Box::new(init),
            Span(start, self.lexer.position()),
        ))
    }

    fn parse_function(
        &mut self,
        expression: bool,
        kind: FunctionKind,
        pos: Position,
    ) -> Result<Node, Error> {
        let name = if expression {
            if let Ok(Token::Identifier(..)) = self.lexer.peek() {
                Some(self.parse_identifier(false)?)
            } else {
                None
            }
        } else {
            Some(self.parse_identifier(false)?)
        };
        let args = self.parse_parameters()?;
        let body = self.parse_block(match kind {
            FunctionKind::Normal => ParseScope::Function,
            FunctionKind::Async => ParseScope::AsyncFunction,
            FunctionKind::Generator => ParseScope::GeneratorFunction,
            _ => unreachable!(),
        })?;
        Ok(if expression {
            Node::FunctionExpression(
                kind,
                name,
                args,
                Box::new(body),
                Span(pos, self.lexer.position()),
            )
        } else {
            let name = name.unwrap();
            self.declare(name.as_str(), false)?;
            Node::FunctionDeclaration(
                kind,
                name,
                args,
                Box::new(body),
                Span(pos, self.lexer.position()),
            )
        })
    }

    fn parse_while(&mut self) -> Result<Node, Error> {
        let start = self.lexer.position();
        self.expect(Token::While)?;
        let test = self.parse_expression()?;
        let body = self.parse_block(ParseScope::Loop)?;
        let end = Span(start, self.lexer.position());
        match constant_truthy(&test) {
            None => Ok(Node::WhileLoop(Box::new(test), Box::new(body), end)),
            Some(true) => Ok(Node::WhileLoop(
                Box::new(Node::TrueLiteral(Span::empty())),
                Box::new(body),
                end,
            )),
            Some(false) => Ok(Node::NullLiteral(end)),
        }
    }

    fn parse_for(&mut self) -> Result<Node, Error> {
        let start = self.lexer.position();
        self.expect(Token::For)?;
        let r#async = if self.scope(ParseScope::AsyncFunction) {
            self.eat(Token::Await)
        } else {
            false
        };
        let binding = self.parse_identifier(false)?;
        self.expect(Token::In)?;
        let target = self.parse_assignment_expression()?;
        let body = self.parse_block(ParseScope::Loop)?;
        Ok(Node::ForLoop(
            r#async,
            binding,
            Box::new(target),
            Box::new(body),
            Span(start, self.lexer.position()),
        ))
    }

    fn parse_return(&mut self) -> Result<Node, Error> {
        let start = self.lexer.position();
        self.expect(Token::Return)?;
        if self.eat(Token::Semicolon) {
            Ok(Node::ReturnStatement(
                None,
                Span(start, self.lexer.position()),
            ))
        } else if self.scope(ParseScope::GeneratorFunction) {
            Err(Error::UnexpectedToken(self.lexer.position()))
        } else {
            let expr = self.parse_expression()?;
            self.expect(Token::Semicolon)?;
            Ok(Node::ReturnStatement(
                Some(Box::new(
                    if let Node::CallExpression(callee, arguments, optional, s) = expr {
                        Node::TailCallExpression(callee, arguments, optional, s)
                    } else {
                        expr
                    },
                )),
                Span(start, self.lexer.position()),
            ))
        }
    }

    fn parse_throw(&mut self) -> Result<Node, Error> {
        let start = self.lexer.position();
        self.expect(Token::Throw)?;
        let expr = self.parse_expression()?;
        self.expect(Token::Semicolon)?;
        Ok(Node::ThrowStatement(
            Box::new(expr),
            Span(start, self.lexer.position()),
        ))
    }

    fn parse_try(&mut self) -> Result<Node, Error> {
        let start = self.lexer.position();
        self.expect(Token::Try)?;
        let try_clause = Box::new(self.parse_block(ParseScope::Block)?);
        if self.eat(Token::Finally) {
            let finally_clause = self.parse_block(ParseScope::Block)?;
            Ok(Node::TryStatement(
                try_clause,
                None,
                None,
                Some(Box::new(finally_clause)),
                Span(start, self.lexer.position()),
            ))
        } else {
            self.expect(Token::Catch)?;
            let binding = if let Ok(Token::Identifier(..)) = self.lexer.peek() {
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
                Span(start, self.lexer.position()),
            ))
        }
    }

    fn parse_decorators(&mut self) -> Result<Node, Error> {
        let mut decorators = VecDeque::new();
        while self.eat(Token::At) {
            let d = self.parse_left_hand_side_expression(true)?;
            decorators.push_front(d);
        }
        let start = self.lexer.position();
        let kind = if self.eat(Token::Async) {
            FunctionKind::Async
        } else if self.eat(Token::Generator) {
            FunctionKind::Generator
        } else if self.eat(Token::Function) {
            FunctionKind::Normal
        } else {
            return Err(Error::UnexpectedToken(start));
        };
        if let Node::FunctionDeclaration(kind, name, args, body, span) =
            self.parse_function(false, kind, start)?
        {
            let mut top = Node::FunctionExpression(kind, None, args, body, span);
            for d in decorators {
                top = Node::CallExpression(Box::new(d), vec![top], false, Span::empty());
            }
            Ok(Node::LexicalInitialization(
                name,
                Box::new(top),
                Span(start, self.lexer.position()),
            ))
        } else {
            unreachable!();
        }
    }

    fn parse_import(&mut self) -> Result<Node, Error> {
        let start = self.lexer.position();
        self.expect(Token::Import)?;
        self.lexer.peek()?;
        match self.lexer.peek_immutable()? {
            // import "specifier";
            Token::StringLiteral(s) => {
                let specifier = s.to_string();
                self.lexer.next()?;
                self.expect(Token::Semicolon)?;
                Ok(Node::ImportDeclaration(
                    specifier,
                    Span(start, self.lexer.position()),
                ))
            }

            // import { x, y } from "specifier";
            // import { x, y } from standard:xy;
            Token::LeftBrace => {
                self.lexer.next()?;
                let bindings = self.parse_identifier_list(Token::RightBrace)?;
                self.expect(Token::From)?;
                match self.lexer.next()? {
                    Token::StringLiteral(c) => {
                        let specifier = c;
                        self.expect(Token::Semicolon)?;
                        Ok(Node::ImportNamedDeclaration(
                            specifier,
                            bindings,
                            Span(start, self.lexer.position()),
                        ))
                    }
                    Token::Identifier(ref s) if s == "standard" => {
                        self.expect(Token::Colon)?;
                        let namespace = self.parse_identifier(true)?;
                        self.expect(Token::Semicolon)?;
                        Ok(Node::ImportStandardDeclaration(
                            namespace,
                            bindings,
                            Span(start, self.lexer.position()),
                        ))
                    }
                    _ => Err(Error::UnexpectedToken(self.lexer.position())),
                }
            }

            // import x from "specifier";
            Token::Identifier(..) => {
                let binding = self.parse_identifier(false)?;
                self.expect(Token::From)?;
                let specifier = match self.lexer.next()? {
                    Token::StringLiteral(s) => s,
                    _ => unreachable!(),
                };
                self.expect(Token::Semicolon)?;
                Ok(Node::ImportDefaultDeclaration(
                    specifier,
                    binding,
                    Span(start, self.lexer.position()),
                ))
            }

            _ => Err(Error::UnexpectedToken(self.lexer.position())),
        }
    }

    fn parse_export(&mut self) -> Result<Node, Error> {
        let start = self.lexer.position();
        self.expect(Token::Export)?;
        let decl = match self.lexer.peek()? {
            Token::Let | Token::Const => self.parse_lexical_declaration(),
            Token::Function => {
                let start = self.lexer.position();
                self.lexer.next()?;
                self.parse_function(false, FunctionKind::Normal, start)
            }
            Token::Generator => {
                let start = self.lexer.position();
                self.lexer.next()?;
                self.parse_function(false, FunctionKind::Generator, start)
            }
            _ => Err(Error::UnexpectedToken(self.lexer.position())),
        }?;
        Ok(Node::ExportDeclaration(
            Box::new(decl),
            Span(start, self.lexer.position()),
        ))
    }

    fn parse_expression(&mut self) -> Result<Node, Error> {
        self.parse_assignment_expression()
    }

    fn parse_assignment_expression(&mut self) -> Result<Node, Error> {
        let start = self.lexer.position();
        if self.eat(Token::Yield) && self.scope(ParseScope::GeneratorFunction) {
            match self.lexer.peek()? {
                Token::Semicolon
                | Token::RightBrace
                | Token::RightBracket
                | Token::RightParen
                | Token::Colon
                | Token::Comma => {
                    return Ok(Node::YieldExpression(
                        None,
                        Span(start, self.lexer.position()),
                    ));
                }
                _ => {
                    let exp = self.parse_assignment_expression()?;
                    return Ok(Node::YieldExpression(
                        Some(Box::new(exp)),
                        Span(start, self.lexer.position()),
                    ));
                }
            }
        }
        let mut lhs =
            self.parse_binary_expression(Token::Operator(Operator::LogicalOR).precedence())?;

        macro_rules! op_assign {
            ($op:expr) => {{
                self.lexer.next()?;
                self.check_assignment_target(&lhs)?;
                let rhs = self.parse_assignment_expression()?;
                lhs = self.build_binary($op, lhs, rhs, Span(start, self.lexer.position()));
            }};
        }

        self.lexer.peek()?;
        match self.lexer.peek_immutable()? {
            Token::Operator(Operator::Assign) => op_assign!(Operator::Assign),
            Token::Operator(Operator::AddAssign) => op_assign!(Operator::AddAssign),
            Token::Operator(Operator::SubAssign) => op_assign!(Operator::SubAssign),
            Token::Operator(Operator::MulAssign) => op_assign!(Operator::MulAssign),
            Token::Operator(Operator::PowAssign) => op_assign!(Operator::PowAssign),
            Token::Operator(Operator::DivAssign) => op_assign!(Operator::DivAssign),
            Token::Operator(Operator::ModAssign) => op_assign!(Operator::ModAssign),
            _ => {}
        }

        Ok(lhs)
    }

    fn parse_binary_expression(&mut self, precedence: u8) -> Result<Node, Error> {
        let start = self.lexer.position();
        let mut x = self.parse_unary_expression()?;
        let mut p = self.lexer.peek()?.precedence();
        if p >= precedence {
            loop {
                while self.lexer.peek()?.precedence() == p {
                    if let Token::Operator(op) = self.lexer.next()? {
                        let next_p = if op == Operator::Pow { p } else { p + 1 };
                        let y = self.parse_binary_expression(next_p)?;
                        x = self.build_binary(op, x, y, Span(start, self.lexer.position()));
                    } else {
                        return Err(Error::UnexpectedToken(self.lexer.position()));
                    }
                }
                p -= 1;
                if p < precedence {
                    break;
                }
            }
        }
        Ok(x)
    }

    fn check_assignment_target(&self, node: &Node) -> Result<(), Error> {
        match node {
            Node::Identifier(..) => Ok(()),
            Node::MemberExpression(..) => Ok(()),
            Node::ComputedMemberExpression(..) => Ok(()),
            _ => Err(Error::InvalidAssignmentTarget(self.lexer.position())),
        }
    }

    fn parse_unary_expression(&mut self) -> Result<Node, Error> {
        let start = self.lexer.position();
        self.lexer.peek()?;
        match self.lexer.peek_immutable()? {
            Token::Operator(Operator::Add) => {
                self.lexer.next()?;
                let expr = self.parse_unary_expression()?;
                Ok(self.build_unary(Operator::Add, expr, Span(start, self.lexer.position())))
            }
            Token::Operator(Operator::Sub) => {
                self.lexer.next()?;
                let expr = self.parse_unary_expression()?;
                Ok(self.build_unary(Operator::Sub, expr, Span(start, self.lexer.position())))
            }
            Token::Operator(Operator::BitwiseNOT) => {
                self.lexer.next()?;
                let expr = self.parse_unary_expression()?;
                Ok(self.build_unary(
                    Operator::BitwiseNOT,
                    expr,
                    Span(start, self.lexer.position()),
                ))
            }
            Token::Operator(Operator::Not) => {
                self.lexer.next()?;
                let expr = self.parse_unary_expression()?;
                Ok(self.build_unary(Operator::Not, expr, Span(start, self.lexer.position())))
            }
            Token::Operator(Operator::Typeof) => {
                self.lexer.next()?;
                let expr = self.parse_unary_expression()?;
                Ok(self.build_unary(Operator::Typeof, expr, Span(start, self.lexer.position())))
            }
            Token::Operator(Operator::Void) => {
                self.lexer.next()?;
                let expr = self.parse_unary_expression()?;
                Ok(self.build_unary(Operator::Void, expr, Span(start, self.lexer.position())))
            }
            Token::Await if self.scope(ParseScope::AsyncFunction) => {
                self.lexer.next()?;
                let expr = self.parse_unary_expression()?;
                Ok(Node::AwaitExpression(
                    Box::new(expr),
                    Span(start, self.lexer.position()),
                ))
            }
            _ => self.parse_left_hand_side_expression(true),
        }
    }

    fn parse_left_hand_side_expression(&mut self, calls: bool) -> Result<Node, Error> {
        let start = self.lexer.position();
        let mut base = if self.eat(Token::New) {
            let callee = self.parse_left_hand_side_expression(false)?;
            self.expect(Token::LeftParen)?;
            let (list, ..) = self.parse_expression_list(Token::RightParen)?;
            Node::NewExpression(Box::new(callee), list, Span(start, self.lexer.position()))
        } else {
            self.parse_primary_expression()?
        };
        let mut optional_chain = false;
        loop {
            let optional = self.eat(Token::Question);
            if optional {
                optional_chain = true;
            }
            if self.eat(Token::Dot) {
                let property = self.parse_identifier(true)?;
                base = Node::MemberExpression(
                    Box::new(base),
                    property,
                    optional,
                    Span(start, self.lexer.position()),
                );
            } else if self.eat(Token::LeftBracket) {
                let property = self.parse_expression()?;
                self.expect(Token::RightBracket)?;
                base = Node::ComputedMemberExpression(
                    Box::new(base),
                    Box::new(property),
                    optional,
                    Span(start, self.lexer.position()),
                );
            } else if calls && self.eat(Token::LeftParen) {
                let (list, ..) = self.parse_expression_list(Token::RightParen)?;
                base = Node::CallExpression(
                    Box::new(base),
                    list,
                    optional,
                    Span(start, self.lexer.position()),
                );
            } else if optional {
                return Err(Error::UnexpectedToken(self.lexer.position()));
            } else if optional_chain {
                return Ok(Node::OptionalChain(Box::new(base)));
            } else {
                return Ok(base);
            }
        }
    }

    fn parse_identifier(&mut self, allow_keyword: bool) -> Result<String, Error> {
        match self.lexer.next()? {
            Token::Identifier(name) => Ok(name),
            Token::Throw if allow_keyword => Ok("throw".to_string()),
            Token::Catch if allow_keyword => Ok("catch".to_string()),
            Token::True if allow_keyword => Ok("true".to_string()),
            Token::False if allow_keyword => Ok("false".to_string()),
            Token::Null if allow_keyword => Ok("null".to_string()),
            Token::This if allow_keyword => Ok("this".to_string()),
            Token::Class if allow_keyword => Ok("class".to_string()),
            Token::Extends if allow_keyword => Ok("extends".to_string()),
            Token::Finally if allow_keyword => Ok("finally".to_string()),
            Token::Function if allow_keyword => Ok("function".to_string()),
            Token::Let if allow_keyword => Ok("let".to_string()),
            Token::Const if allow_keyword => Ok("const".to_string()),
            Token::Throw if allow_keyword => Ok("throw".to_string()),
            Token::Return if allow_keyword => Ok("return".to_string()),
            Token::While if allow_keyword => Ok("while".to_string()),
            Token::For if allow_keyword => Ok("for".to_string()),
            Token::In if allow_keyword => Ok("in".to_string()),
            Token::Break if allow_keyword => Ok("break".to_string()),
            Token::Continue if allow_keyword => Ok("continue".to_string()),
            Token::Try if allow_keyword => Ok("try".to_string()),
            Token::Catch if allow_keyword => Ok("catch".to_string()),
            Token::If if allow_keyword => Ok("if".to_string()),
            Token::Else if allow_keyword => Ok("else".to_string()),
            Token::New if allow_keyword => Ok("new".to_string()),
            Token::Import if allow_keyword => Ok("import".to_string()),
            Token::Export if allow_keyword => Ok("export".to_string()),
            Token::Default if allow_keyword => Ok("default".to_string()),
            Token::From if allow_keyword => Ok("from".to_string()),
            Token::Async if allow_keyword => Ok("async".to_string()),
            Token::Await if allow_keyword => Ok("await".to_string()),
            Token::Generator if allow_keyword => Ok("generator".to_string()),
            Token::Yield if allow_keyword => Ok("yield".to_string()),
            Token::Match if allow_keyword => Ok("match".to_string()),
            Token::Operator(Operator::Typeof) if allow_keyword => Ok("typeof".to_string()),
            Token::Operator(Operator::Void) if allow_keyword => Ok("void".to_string()),
            Token::Operator(Operator::Has) if allow_keyword => Ok("has".to_string()),
            _ => Err(Error::UnexpectedToken(self.lexer.position())),
        }
    }

    fn parse_primary_expression(&mut self) -> Result<Node, Error> {
        let start = self.lexer.position();
        let token = self.lexer.next()?;
        match token {
            Token::Null => Ok(Node::NullLiteral(Span(start, self.lexer.position()))),
            Token::True => Ok(Node::TrueLiteral(Span(start, self.lexer.position()))),
            Token::False => Ok(Node::FalseLiteral(Span(start, self.lexer.position()))),
            Token::StringLiteral(s) => {
                Ok(Node::StringLiteral(s, Span(start, self.lexer.position())))
            }
            Token::NumberLiteral(n) => {
                Ok(Node::NumberLiteral(n, Span(start, self.lexer.position())))
            }
            Token::Colon => {
                let name = self.parse_identifier(false)?;
                Ok(Node::SymbolLiteral(
                    name,
                    Span(start, self.lexer.position()),
                ))
            }
            Token::Operator(Operator::Div) => {
                let mut pattern = String::new();
                loop {
                    match self.lexer.next_char() {
                        Some('/') => break,
                        Some('\\') => {
                            pattern.push('\\');
                            pattern.push(self.lexer.next_char().unwrap());
                        }
                        Some(c) => {
                            pattern.push(c);
                        }
                        None => return Err(Error::UnexpectedEOF(self.lexer.position())),
                    }
                }
                Ok(Node::RegexLiteral(
                    pattern,
                    Span(start, self.lexer.position()),
                ))
            }
            Token::This => Ok(Node::ThisExpression(Span(start, self.lexer.position()))),
            Token::Identifier(i) => Ok(Node::Identifier(i, Span(start, self.lexer.position()))),
            Token::LeftBracket => {
                let (exprs, ..) = self.parse_expression_list(Token::RightBracket)?;
                Ok(Node::ArrayLiteral(
                    exprs,
                    Span(start, self.lexer.position()),
                ))
            }
            Token::LeftBrace => {
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
                    let start = self.lexer.position();
                    let name = if self.eat(Token::LeftBracket) {
                        let name = self.parse_expression()?;
                        self.expect(Token::RightBracket)?;
                        name
                    } else {
                        Node::StringLiteral(
                            self.parse_identifier(true)?,
                            Span(start, self.lexer.position()),
                        )
                    };
                    let init = if self.eat(Token::Colon) {
                        self.parse_expression()?
                    } else if self.peek(Token::LeftParen) {
                        self.parse_function(true, FunctionKind::Normal, start)?
                    } else if let Node::StringLiteral(n, s) = &name {
                        Node::Identifier(n.to_string(), *s)
                    } else {
                        return Err(Error::UnexpectedToken(self.lexer.position()));
                    };
                    fields.push(Node::Initializer(
                        Box::new(name),
                        Box::new(init),
                        Span(start, self.lexer.position()),
                    ));
                }
                Ok(Node::ObjectLiteral(
                    fields,
                    Span(start, self.lexer.position()),
                ))
            }
            Token::LeftParen => {
                let (mut list, trailing) = self.parse_expression_list(Token::RightParen)?;
                if self.eat(Token::Arrow) {
                    // ( ... ) =>
                    self.parse_arrow_function(FunctionKind::Normal, list, start)
                } else if list.is_empty() {
                    // ( )
                    Err(Error::UnexpectedToken(self.lexer.position()))
                } else if list.len() == 1 && !trailing {
                    // ( expr )
                    Ok(Node::ParenthesizedExpression(
                        Box::new(list.pop().unwrap()),
                        Span(start, self.lexer.position()),
                    ))
                } else {
                    // ( expr , )
                    // ( expr , expr )
                    Ok(Node::TupleLiteral(list, Span(start, self.lexer.position())))
                }
            }
            Token::If => self.parse_if_expression(start),
            Token::Function => self.parse_function(true, FunctionKind::Normal, start),
            Token::Generator => self.parse_function(true, FunctionKind::Generator, start),
            Token::Async => {
                let list = self.parse_parameters()?;
                self.expect(Token::Arrow)?;
                self.parse_arrow_function(FunctionKind::Async, list, start)
            }
            Token::Class => self.parse_class(true),
            Token::BackQuote => {
                let mut quasis = Vec::new();
                let mut expressions = Vec::new();
                let mut current = String::new();
                loop {
                    match self.lexer.next_char() {
                        Some('$') => {
                            if self.lexer.chars.peek() == Some(&'{') {
                                quasis.push(current);
                                current = String::new();
                                self.lexer.next_char();
                                let expr = self.parse_expression()?;
                                expressions.push(expr);
                                self.expect(Token::RightBrace)?;
                            } else {
                                current.push('$');
                            }
                        }
                        Some('`') => break,
                        Some(c) => {
                            if c == '\\' {
                                match self.lexer.next_char() {
                                    Some('n') => current.push('\n'),
                                    Some('t') => current.push('\t'),
                                    Some('\\') => current.push('\\'),
                                    Some('u') => {
                                        if Some('{') != self.lexer.next_char() {
                                            return Err(Error::UnexpectedToken(
                                                self.lexer.position(),
                                            ));
                                        }
                                        let mut n = String::new();
                                        macro_rules! digit {
                                            () => {
                                                let next = self.lexer.next_char();
                                                match next {
                                                    Some('0'...'9') | Some('a'...'f')
                                                    | Some('A'...'F') => {
                                                        n.push(next.unwrap());
                                                    }
                                                    _ => {
                                                        return Err(Error::UnexpectedToken(
                                                            self.lexer.position(),
                                                        ))
                                                    }
                                                }
                                            };
                                        }
                                        digit!();
                                        digit!();
                                        digit!();
                                        digit!();
                                        match u32::from_str_radix(n.as_str(), 16) {
                                            Ok(n) => match std::char::from_u32(n) {
                                                Some(c) => current.push(c),
                                                None => {
                                                    return Err(Error::UnexpectedToken(
                                                        self.lexer.position(),
                                                    ))
                                                }
                                            },
                                            Err(_) => {
                                                return Err(Error::UnexpectedToken(
                                                    self.lexer.position(),
                                                ))
                                            }
                                        }
                                        if Some('}') != self.lexer.next_char() {
                                            return Err(Error::UnexpectedToken(
                                                self.lexer.position(),
                                            ));
                                        }
                                    }
                                    Some('U') => {
                                        if Some('{') != self.lexer.next_char() {
                                            return Err(Error::UnexpectedToken(
                                                self.lexer.position(),
                                            ));
                                        }
                                        let mut name = String::new();
                                        loop {
                                            match self.lexer.next_char() {
                                                Some('}') => break,
                                                None => {
                                                    return Err(Error::UnexpectedEOF(
                                                        self.lexer.position(),
                                                    ))
                                                }
                                                Some(c) => name.push(c),
                                            }
                                        }
                                        match UNICODE_NAME_MAP.get(name.as_str()) {
                                            Some(c) => current.push(*c),
                                            None => {
                                                return Err(Error::UnexpectedToken(
                                                    self.lexer.position(),
                                                ))
                                            }
                                        };
                                    }
                                    None | _ => {
                                        return Err(Error::UnexpectedEOF(self.lexer.position()))
                                    }
                                }
                            } else {
                                current.push(c);
                            }
                        }
                        None => return Err(Error::UnexpectedEOF(self.lexer.position())),
                    }
                }
                quasis.push(current);
                Ok(Node::TemplateLiteral(
                    quasis,
                    expressions,
                    Span(start, self.lexer.position()),
                ))
            }
            Token::Match => {
                let expr = self.parse_expression()?;
                self.expect(Token::LeftBrace)?;
                let mut arms = Vec::new();
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
                    let start = self.lexer.position();
                    self.scope.push(Scope::new(ParseScope::Block));
                    let pattern = self.parse_pattern()?;
                    self.expect(Token::Arrow)?;
                    let consequent = if self.peek(Token::LeftBrace) {
                        self.parse_block(ParseScope::Block)?
                    } else {
                        self.parse_expression()?
                    };
                    self.scope.pop().unwrap();
                    arms.push(Node::MatchArm(
                        Box::new(pattern),
                        Box::new(consequent),
                        Span(start, self.lexer.position()),
                    ));
                }
                Ok(Node::MatchExpression(
                    Box::new(expr),
                    arms,
                    Span(start, self.lexer.position()),
                ))
            }
            _ => Err(Error::UnexpectedToken(start)),
        }
    }

    fn parse_if_expression(&mut self, start: Position) -> Result<Node, Error> {
        let test = self.parse_expression()?;
        let consequent = self.parse_block(ParseScope::Block)?;
        if self.eat(Token::Else) {
            let alternative = if self.lexer.peek() == Ok(&Token::If) {
                let start = self.lexer.position();
                self.lexer.next()?;
                self.parse_if_expression(start)?
            } else {
                self.parse_block(ParseScope::Block)?
            };
            match constant_truthy(&test) {
                Some(true) => Ok(consequent),
                Some(false) => Ok(alternative),
                None => Ok(Node::IfExpression(
                    Box::new(test),
                    Box::new(consequent),
                    Some(Box::new(alternative)),
                    Span(start, self.lexer.position()),
                )),
            }
        } else {
            match constant_truthy(&test) {
                Some(true) => Ok(consequent),
                Some(false) => Ok(Node::NullLiteral(Span(start, self.lexer.position()))),
                None => Ok(Node::IfExpression(
                    Box::new(test),
                    Box::new(consequent),
                    None,
                    Span(start, self.lexer.position()),
                )),
            }
        }
    }

    fn parse_pattern(&mut self) -> Result<Node, Error> {
        match self.lexer.peek()? {
            // 1
            // "hi"
            // a
            Token::NumberLiteral(..) | Token::StringLiteral(..) | Token::Identifier(..) => {
                self.parse_expression()
            }
            // { a }
            // { a: b }
            // { a: b, ... }
            Token::LeftBrace => {
                let start = self.lexer.position();
                self.lexer.next()?;
                let mut bindings = IndexMap::new();
                let mut first = true;
                let mut wildcard = false;
                while !self.eat(Token::RightBrace) {
                    if first {
                        first = false;
                    } else {
                        self.expect(Token::Comma)?;
                        if self.eat(Token::RightBrace) {
                            break;
                        }
                        if self.eat(Token::Ellipsis) {
                            wildcard = true;
                            self.expect(Token::RightBrace)?;
                            break;
                        }
                    }
                    let name = self.parse_identifier(false)?;
                    if self.eat(Token::Colon) {
                        let rename = self.parse_identifier(false)?;
                        self.declare(&rename, false)?;
                        bindings.insert(name, rename);
                    } else {
                        self.declare(&name, false)?;
                        bindings.insert(name.to_string(), name.to_string());
                    }
                }
                Ok(Node::ObjectPattern(
                    bindings,
                    wildcard,
                    Span(start, self.lexer.position()),
                ))
            }
            // [a]
            // [a, ...]
            Token::LeftBracket => {
                let start = self.lexer.position();
                self.lexer.next()?;
                let mut bindings = Vec::new();
                let mut first = true;
                let mut wildcard = false;
                while !self.eat(Token::RightBracket) {
                    if first {
                        first = false;
                    } else {
                        self.expect(Token::Comma)?;
                        if self.eat(Token::RightBracket) {
                            break;
                        }
                        if self.eat(Token::Ellipsis) {
                            wildcard = true;
                            self.expect(Token::RightBracket)?;
                            break;
                        }
                    }
                    let name = self.parse_identifier(false)?;
                    self.declare(&name, false)?;
                    bindings.push(name);
                }
                Ok(Node::ArrayPattern(
                    bindings,
                    wildcard,
                    Span(start, self.lexer.position()),
                ))
            }
            _ => Err(Error::UnexpectedToken(self.lexer.position())),
        }
    }

    fn parse_class(&mut self, expression: bool) -> Result<Node, Error> {
        let start = self.lexer.position();
        if !expression {
            self.expect(Token::Class)?;
        }
        let name = self.parse_identifier(false)?;
        if !expression {
            self.declare(&name, false)?;
        }
        let extends = if self.eat(Token::Extends) {
            Some(Box::new(self.parse_left_hand_side_expression(true)?))
        } else {
            None
        };
        self.expect(Token::LeftBrace)?;
        let mut fields = Vec::new();
        while !self.eat(Token::RightBrace) {
            let start = self.lexer.position();
            let name = Node::StringLiteral(self.parse_identifier(false)?, Span::empty());
            let f = self.parse_function(true, FunctionKind::Normal, start)?;
            fields.push(Node::Initializer(
                Box::new(name),
                Box::new(f),
                Span(start, self.lexer.position()),
            ));
        }
        if expression {
            Ok(Node::ClassExpression(
                name,
                extends,
                fields,
                Span(start, self.lexer.position()),
            ))
        } else {
            Ok(Node::ClassDeclaration(
                name,
                extends,
                fields,
                Span(start, self.lexer.position()),
            ))
        }
    }

    fn parse_arrow_function(
        &mut self,
        kind: FunctionKind,
        mut args: Vec<Node>,
        start: Position,
    ) -> Result<Node, Error> {
        for item in &mut args {
            match item {
                Node::Identifier(..) | Node::Initializer(..) => {}
                Node::BinaryExpression(op, left, right, s) if *op == Operator::Assign => {
                    if let Node::Identifier(..) = &**left {
                        let init = Node::Initializer(
                            Box::new(std::mem::replace(
                                &mut **left,
                                Node::NullLiteral(Span::empty()),
                            )),
                            Box::new(std::mem::replace(
                                &mut **right,
                                Node::NullLiteral(Span::empty()),
                            )),
                            *s,
                        );
                        std::mem::replace(item, init);
                    } else {
                        return Err(Error::UnexpectedToken(s.0));
                    }
                }
                _ => return Err(Error::UnexpectedToken(self.lexer.position())),
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
                vec![Node::ReturnStatement(Some(Box::new(expr)), Span::empty())],
                Span::empty(),
            )
        };
        Ok(Node::ArrowFunctionExpression(
            kind | FunctionKind::Arrow,
            args,
            Box::new(body),
            Span(start, self.lexer.position()),
        ))
    }

    fn parse_expression_list(&mut self, close: Token) -> Result<(Vec<Node>, bool), Error> {
        let mut list = Vec::new();
        let mut first = true;
        let mut trailing = false;
        while !self.eat(close.clone()) {
            if first {
                first = false;
            } else {
                self.expect(Token::Comma)?;
                trailing = true;
                if self.eat(close.clone()) {
                    break;
                }
                trailing = false;
            }
            list.push(self.parse_expression()?);
        }
        Ok((list, trailing))
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

    fn parse_parameters(&mut self) -> Result<Vec<Node>, Error> {
        self.expect(Token::LeftParen)?;
        let mut parameters = Vec::new();
        let mut first = true;
        while !self.eat(Token::RightParen) {
            if first {
                first = false;
            } else {
                self.expect(Token::Comma)?;
                if self.eat(Token::RightParen) {
                    break;
                }
            }
            let start = self.lexer.position();
            let ident = self.parse_identifier(false)?;
            if self.eat(Token::Operator(Operator::Assign)) {
                let init = self.parse_expression()?;
                parameters.push(Node::Initializer(
                    Box::new(Node::Identifier(ident, Span::empty())),
                    Box::new(init),
                    Span(start, self.lexer.position()),
                ));
            } else {
                parameters.push(Node::Identifier(ident, Span::empty()));
            }
        }
        Ok(parameters)
    }
}
