use rust_decimal::Decimal;
use std::collections::HashMap;
use std::iter::Peekable;
use std::ops::{Div, Mul, Rem, Sub};
use std::str::Chars;

#[derive(Debug, PartialEq, Clone)]
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
    NumberLiteral(Decimal),
    StringLiteral(String),
    Identifier(String),
    Operator(Operator),
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    LeftParen,
    RightParen,
    Null,
    True,
    False,
    This,
    Function,
    Arrow,
    Class,
    New,
    Let,
    Const,
    Semicolon,
    Colon,
    Question,
    Dot,
    BackQuote,
    Comma,
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
    Return,
    Import,
    Export,
    Default,
    From,
    Async,
    Await,
    Gen,
    Yield,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    NullLiteral,
    TrueLiteral,
    FalseLiteral,
    NumberLiteral(Decimal),
    StringLiteral(String),
    SymbolLiteral(String),
    RegexLiteral(String),
    TemplateLiteral(Vec<String>, Vec<Node>), // quasis, expressions
    Initializer(String, Box<Node>),          // name, value
    ObjectLiteral(Vec<Node>),                // initialiers
    ObjectInitializer(Box<Node>, Box<Node>), // name, value
    ArrayLiteral(Vec<Node>),
    Identifier(String),
    BlockStatement(Vec<Node>, HashMap<String, bool>, bool), // nodes, declarations, top
    ReturnStatement(Box<Node>),
    ThrowStatement(Box<Node>),
    IfStatement(Box<Node>, Box<Node>), // test, consequent
    IfElseStatement(Box<Node>, Box<Node>, Box<Node>), // test, consequent, alternative
    WhileStatement(Box<Node>, Box<Node>), // test, body
    ForStatement(String, Box<Node>, Box<Node>), // binding, target, body
    BreakStatement,
    ContinueStatement,
    TryStatement(
        Box<Node>,         // try clause
        Option<String>,    // catch binding
        Option<Box<Node>>, // catch clause
        Option<Box<Node>>, // finally clause
    ),
    ExpressionStatement(Box<Node>),
    NewExpression(Box<Node>),
    MemberExpression(Box<Node>, String), // base, property
    ComputedMemberExpression(Box<Node>, Box<Node>), // base, property expression
    ThisExpression,
    CallExpression(Box<Node>, Vec<Node>), // callee, arguments
    TailCallExpression(Box<Node>, Vec<Node>), // callee, arguments
    UnaryExpression(Operator, Box<Node>), // op x
    BinaryExpression(Box<Node>, Operator, Box<Node>), // x op y
    ConditionalExpression(Box<Node>, Box<Node>, Box<Node>), // test, consequent, alternative
    FunctionDeclaration(String, Vec<Node>, Box<Node>, FunctionKind), // name, args, body
    FunctionExpression(Option<String>, Vec<Node>, Box<Node>, FunctionKind), // name, args, body
    ArrowFunctionExpression(Vec<Node>, Box<Node>, FunctionKind), // args, body
    ParenthesizedExpression(Box<Node>),   // expr
    AwaitExpression(Box<Node>),
    YieldExpression(Option<Box<Node>>),
    LexicalInitialization(String, Box<Node>), // identifier, initial value
    ImportDeclaration(String),                // specifier
    ImportNamedDeclaration(String, Vec<String>), // specifier, bindings
    ImportDefaultDeclaration(String, String), // specifier, binding
    ImportStandardDeclaration(String, Vec<String>), // namespace, bindings
    ExportDeclaration(Box<Node>),
}

#[derive(Debug)]
pub enum Error {
    NormalEOF,
    UnexpectedEOF,
    UnexpectedToken,
    DuplicateBinding,
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
                            .parse::<Decimal>()
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
                    '.' => Some(Token::Dot),
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
                    '!' => Some(Token::Operator(Operator::Not)),
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
}

#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum FunctionKind {
    Normal,
    Async,
    Generator,
}

impl From<u8> for FunctionKind {
    fn from(n: u8) -> Self {
        unsafe { std::mem::transmute::<u8, FunctionKind>(n) }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u8)]
enum ParseScope {
    TopLevel = 0b0000_0001,
    Block = 0b0000_0010,
    Loop = 0b0000_0100,
    Function = 0b0000_1000,
    AsyncFunction = 0b0001_1000,
    GeneratorFunction = 0b0010_1000,
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    scope_bits: u8,
    lex_stack: Vec<HashMap<String, bool>>,
}

macro_rules! binop_production {
    ( $name:ident, $lower:ident, [ $( $op:path ),* ] ) => {
        fn $name(&mut self) -> Result<Node, Error> {
            let mut lhs = self.$lower()?;
            match self.lexer.peek() {
                Some(Token::Operator(op)) if $( op == &$op )||* => {
                    let op = op.clone();
                    self.lexer.next();
                    let rhs = self.$lower()?;
                    lhs = self.build_binary_expression(lhs, op, rhs)?;
                }
                _ => {},
            }
            Ok(lhs)
        }
    }
}

impl<'a> Parser<'a> {
    pub fn parse(code: &'a str) -> Result<Node, Error> {
        let mut parser = Parser {
            lexer: Lexer::new(code),
            scope_bits: 0,
            lex_stack: Vec::new(),
        };

        if let Node::BlockStatement(items, decls, top) =
            parser.parse_block_statement(ParseScope::TopLevel)?
        {
            if let Node::ExpressionStatement(expr) = items.last().unwrap() {
                // if the last item is an expression statement, replace it with the expression
                // so that the value will be left on the stack to inspect in tests
                let mut sliced = items[0..items.len() - 1].to_vec();
                sliced.push(Node::ParenthesizedExpression((*expr).clone()));
                Ok(Node::BlockStatement(sliced, decls, top))
            } else {
                Ok(Node::BlockStatement(items, decls, top))
            }
        } else {
            unreachable!();
        }
    }

    fn scope(&self, scope: ParseScope) -> bool {
        (self.scope_bits & scope as u8) == scope as u8
    }

    fn eat(&mut self, token: Token) -> bool {
        match self.lexer.peek() {
            Some(t) if t == &token => {
                self.lexer.next();
                true
            }
            _ => false,
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

    fn parse_identifier_list(
        &mut self,
        close: Token,
        initializers: bool,
    ) -> Result<Vec<Node>, Error> {
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
            let ident = self.parse_identifier(false)?;
            if self.lexer.peek() == Some(&Token::Operator(Operator::Assign)) && initializers {
                self.lexer.next();
                let init = self.parse_expression()?;
                identifiers.push(Node::Initializer(ident, Box::new(init)));
            } else {
                identifiers.push(Node::Identifier(ident));
            }
        }
        Ok(identifiers)
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
        let args = self.parse_identifier_list(Token::RightParen, true)?;
        let body = self.parse_block_statement(match kind {
            FunctionKind::Normal => ParseScope::Function,
            FunctionKind::Async => ParseScope::AsyncFunction,
            FunctionKind::Generator => ParseScope::GeneratorFunction,
        })?;
        Ok(if expression {
            Node::FunctionExpression(name, args, Box::new(body), kind)
        } else {
            let name = name.unwrap();
            let scope = self.lex_stack.last_mut().unwrap();
            if scope.contains_key(&name) {
                return Err(Error::DuplicateBinding);
            } else {
                scope.insert(name.clone(), false);
            }
            Node::FunctionDeclaration(name, args, Box::new(body), kind)
        })
    }

    fn parse_statement_list_item(&mut self) -> Result<Node, Error> {
        self.lexer.peek();
        match self.lexer.peek_immutable() {
            None => Err(Error::NormalEOF),
            Some(Token::LeftBrace) => self.parse_block_statement(ParseScope::Block),
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
            Some(Token::Return) if self.scope(ParseScope::Function) => {
                self.lexer.next();
                if self.eat(Token::Semicolon) {
                    Ok(Node::ReturnStatement(Box::new(Node::NullLiteral)))
                } else {
                    let mut expr = self.parse_expression()?;
                    self.expect(Token::Semicolon)?;
                    if let Node::CallExpression(callee, arguments) = expr {
                        expr = Node::TailCallExpression(callee, arguments);
                    }
                    Ok(Node::ReturnStatement(Box::new(expr)))
                }
            }
            Some(Token::Throw) => {
                self.lexer.next();
                let expr = self.parse_expression()?;
                self.expect(Token::Semicolon)?;
                Ok(Node::ThrowStatement(Box::new(expr)))
            }
            Some(Token::Try) => {
                self.lexer.next();
                let try_clause = Box::new(self.parse_block_statement(ParseScope::Block)?);
                if self.eat(Token::Finally) {
                    let finally_clause = Box::new(self.parse_block_statement(ParseScope::Block)?);
                    Ok(Node::TryStatement(
                        try_clause,
                        None,
                        None,
                        Some(finally_clause),
                    ))
                } else {
                    self.expect(Token::Catch)?;
                    let mut binding = None;
                    if let Some(Token::Identifier(..)) = self.lexer.peek() {
                        binding = Some(self.parse_identifier(false)?);
                    }
                    let catch_clause = Box::new(self.parse_block_statement(ParseScope::Block)?);
                    if self.eat(Token::Finally) {
                        let finally_clause =
                            Box::new(self.parse_block_statement(ParseScope::Block)?);
                        Ok(Node::TryStatement(
                            try_clause,
                            binding,
                            Some(catch_clause),
                            Some(finally_clause),
                        ))
                    } else {
                        Ok(Node::TryStatement(
                            try_clause,
                            binding,
                            Some(catch_clause),
                            None,
                        ))
                    }
                }
            }
            Some(Token::If) => {
                self.lexer.next();
                let test = self.parse_expression()?;
                let consequent = self.parse_block_statement(ParseScope::Block)?;
                if self.eat(Token::Else) {
                    let alternative = if self.lexer.peek() == Some(&Token::If) {
                        self.parse_statement_list_item()?
                    } else {
                        self.parse_block_statement(ParseScope::Block)?
                    };
                    if let Some(n) =
                        self.fold_conditional(test.clone(), consequent.clone(), alternative.clone())
                    {
                        return Ok(n);
                    }
                    Ok(Node::IfElseStatement(
                        Box::new(test),
                        Box::new(consequent),
                        Box::new(alternative),
                    ))
                } else {
                    if let Some(n) = self.fold_conditional(
                        test.clone(),
                        consequent.clone(),
                        Node::ExpressionStatement(Box::new(Node::NullLiteral)),
                    ) {
                        return Ok(n);
                    }
                    Ok(Node::IfStatement(Box::new(test), Box::new(consequent)))
                }
            }
            Some(Token::While) => {
                self.lexer.next();
                let test = self.parse_expression()?;
                let body = self.parse_block_statement(ParseScope::Loop)?;
                if let Some(n) = self.fold_while_loop(test.clone()) {
                    Ok(n)
                } else {
                    Ok(Node::WhileStatement(Box::new(test), Box::new(body)))
                }
            }
            Some(Token::For) => {
                self.lexer.next();
                let binding = self.parse_identifier(false)?;
                self.expect(Token::In)?;
                let target = self.parse_assignment_expression()?;
                let body = self.parse_block_statement(ParseScope::Loop)?;
                Ok(Node::ForStatement(
                    binding,
                    Box::new(target),
                    Box::new(body),
                ))
            }
            Some(Token::Break) if self.scope(ParseScope::Loop) => {
                self.lexer.next();
                self.expect(Token::Semicolon)?;
                Ok(Node::BreakStatement)
            }
            Some(Token::Continue) if self.scope(ParseScope::Loop) => {
                self.lexer.next();
                self.expect(Token::Semicolon)?;
                Ok(Node::ContinueStatement)
            }
            Some(Token::Export) if self.scope(ParseScope::TopLevel) => {
                self.lexer.next();
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
            Some(Token::Import) if self.scope(ParseScope::TopLevel) => {
                self.lexer.next();
                match self.lexer.peek() {
                    // import "specifier";
                    Some(Token::StringLiteral(..)) => {
                        let specifier = match self.lexer.next() {
                            Some(Token::StringLiteral(s)) => s,
                            _ => unreachable!(),
                        };
                        self.expect(Token::Semicolon)?;
                        Ok(Node::ImportDeclaration(specifier))
                    }
                    // import { bindings } from "specifier";
                    Some(Token::LeftBrace) => {
                        self.lexer.next();
                        let bindings = self
                            .parse_identifier_list(Token::RightBrace, false)?
                            .iter()
                            .map(|n| match n {
                                Node::Identifier(n) => n.to_string(),
                                _ => unreachable!(),
                            })
                            .collect();
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
                    // import binding from "specifier";
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
            _ => {
                let expr = self.parse_expression_statement()?;
                self.expect(Token::Semicolon)?;
                Ok(expr)
            }
        }
    }

    fn parse_lexical_declaration(&mut self) -> Result<Node, Error> {
        match self.lexer.peek() {
            Some(Token::Let) | Some(Token::Const) => {
                let decl = self.lexer.next().unwrap();
                let name = self.parse_identifier(false)?;
                self.expect(Token::Operator(Operator::Assign))?;
                let value = self.parse_assignment_expression()?;
                self.expect(Token::Semicolon)?;
                let scope = self.lex_stack.last_mut().unwrap();
                if scope.contains_key(&name) {
                    return Err(Error::DuplicateBinding);
                } else {
                    scope.insert(
                        name.clone(),
                        match decl {
                            Token::Let => true,
                            Token::Const => false,
                            _ => unreachable!(),
                        },
                    );
                }
                Ok(Node::LexicalInitialization(name, Box::new(value)))
            }
            _ => Err(Error::UnexpectedToken),
        }
    }

    fn parse_block_statement(&mut self, scope: ParseScope) -> Result<Node, Error> {
        if scope != ParseScope::TopLevel {
            self.expect(Token::LeftBrace)?;
        }
        let saved = self.scope_bits;
        self.scope_bits |= scope as u8;
        self.lex_stack.push(HashMap::new());
        let mut nodes = Vec::new();
        while !self.eat(Token::RightBrace) {
            match self.parse_statement_list_item() {
                Ok(node) => nodes.push(node),
                Err(Error::NormalEOF) if scope == ParseScope::TopLevel => break,
                Err(e) => {
                    self.scope_bits = saved;
                    self.lex_stack.pop();
                    return Err(e);
                }
            }
        }
        self.scope_bits = saved;
        let declarations = self.lex_stack.pop().unwrap();
        Ok(Node::BlockStatement(
            nodes,
            declarations,
            scope == ParseScope::TopLevel,
        ))
    }

    fn parse_expression_statement(&mut self) -> Result<Node, Error> {
        let expression = self.parse_expression()?;
        Ok(Node::ExpressionStatement(Box::new(expression)))
    }

    fn parse_expression(&mut self) -> Result<Node, Error> {
        self.parse_assignment_expression()
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
                // lhs @= rhs;
                // lhs = lhs @ rhs;
                self.lexer.next();
                let rhs = self.parse_assignment_expression()?;
                let rhs = self.build_binary_expression(lhs.clone(), $op, rhs)?;
                lhs = self.build_binary_expression(lhs, Operator::Assign, rhs)?;
            }};
        }

        self.lexer.peek();
        match self.lexer.peek_immutable() {
            Some(Token::Operator(Operator::Assign)) => {
                self.lexer.next();
                let rhs = self.parse_assignment_expression()?;
                lhs = self.build_binary_expression(lhs, Operator::Assign, rhs)?;
            }
            Some(Token::Operator(Operator::AddAssign)) => op_assign!(Operator::Add),
            Some(Token::Operator(Operator::SubAssign)) => op_assign!(Operator::Sub),
            Some(Token::Operator(Operator::MulAssign)) => op_assign!(Operator::Mul),
            Some(Token::Operator(Operator::PowAssign)) => op_assign!(Operator::Pow),
            Some(Token::Operator(Operator::DivAssign)) => op_assign!(Operator::Div),
            Some(Token::Operator(Operator::ModAssign)) => op_assign!(Operator::Mod),
            _ => {}
        }

        Ok(lhs)
    }

    fn build_binary_expression(
        &self,
        left: Node,
        op: Operator,
        right: Node,
    ) -> Result<Node, Error> {
        match op {
            Operator::Assign
            | Operator::AddAssign
            | Operator::SubAssign
            | Operator::MulAssign
            | Operator::DivAssign
            | Operator::PowAssign => match left {
                Node::CallExpression(..)
                | Node::UnaryExpression(..)
                | Node::NullLiteral
                | Node::TrueLiteral
                | Node::FalseLiteral
                | Node::ArrayLiteral(..)
                | Node::ObjectLiteral(..)
                | Node::NumberLiteral(..)
                | Node::StringLiteral(..) => {
                    return Err(Error::UnexpectedToken);
                }
                _ => {}
            },
            _ => {}
        };

        macro_rules! num_binop_num {
            ($op:expr) => {
                if let Node::NumberLiteral(lnum) = left {
                    if let Node::NumberLiteral(rnum) = right {
                        return Ok(Node::NumberLiteral($op(lnum, rnum)));
                    }
                }
            };
        }

        macro_rules! num_binop_bool {
            ($op:expr) => {
                if let Node::NumberLiteral(lnum) = left {
                    if let Node::NumberLiteral(rnum) = right {
                        if $op(&lnum, &rnum) {
                            return Ok(Node::TrueLiteral);
                        } else {
                            return Ok(Node::FalseLiteral);
                        }
                    }
                }
            };
        }

        match op {
            Operator::Add => match &left {
                Node::NumberLiteral(lnum) => {
                    if let Node::NumberLiteral(rnum) = right {
                        return Ok(Node::NumberLiteral(lnum + rnum));
                    }
                }
                Node::StringLiteral(lstr) => {
                    if let Node::StringLiteral(rstr) = right {
                        return Ok(Node::StringLiteral(format!("{}{}", lstr, rstr)));
                    }
                }
                _ => {}
            },
            Operator::Sub => num_binop_num!(Decimal::sub),
            Operator::Mul => num_binop_num!(Decimal::mul),
            Operator::Div => num_binop_num!(Decimal::div),
            Operator::Mod => num_binop_num!(Decimal::rem),
            // Operator::Pow => num_binop_num!(Decimal::pow),
            // Operator::LeftShift => num_binop_num!(Decimal::shl),
            // Operator::RightShift => num_binop_num!(Decimal::shr),
            Operator::LessThan => num_binop_bool!(Decimal::lt),
            Operator::GreaterThan => num_binop_bool!(Decimal::gt),
            Operator::LessThanOrEqual => num_binop_bool!(Decimal::le),
            Operator::GreaterThanOrEqual => num_binop_bool!(Decimal::ge),
            _ => {}
        }

        Ok(Node::BinaryExpression(Box::new(left), op, Box::new(right)))
    }

    fn fold_conditional(&self, test: Node, consequent: Node, alternative: Node) -> Option<Node> {
        match test {
            Node::NumberLiteral(n) => {
                if n != 0.into() {
                    Some(consequent)
                } else {
                    Some(alternative)
                }
            }
            Node::StringLiteral(s) => {
                if s.chars().count() > 0 {
                    Some(consequent)
                } else {
                    Some(alternative)
                }
            }
            Node::FalseLiteral | Node::NullLiteral | Node::UnaryExpression(Operator::Void, ..) => {
                Some(alternative)
            }
            Node::TrueLiteral | Node::ArrayLiteral(..) | Node::ObjectLiteral(..) => {
                Some(consequent)
            }
            _ => None,
        }
    }

    fn fold_while_loop(&self, test: Node) -> Option<Node> {
        match test {
            Node::NullLiteral | Node::FalseLiteral | Node::UnaryExpression(Operator::Void, ..) => {
                Some(Node::ExpressionStatement(Box::new(test)))
            }
            Node::NumberLiteral(n) => {
                if n == 0.into() {
                    Some(Node::ExpressionStatement(Box::new(test)))
                } else {
                    None
                }
            }
            Node::StringLiteral(ref s) => {
                if s.chars().count() == 0 {
                    Some(Node::ExpressionStatement(Box::new(test)))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn parse_conditional_expression(&mut self) -> Result<Node, Error> {
        let lhs = self.parse_logical_or_expression()?;

        if self.eat(Token::Question) {
            let consequent = self.parse_assignment_expression()?;
            self.expect(Token::Colon)?;
            let alternative = self.parse_assignment_expression()?;
            if let Some(n) =
                self.fold_conditional(lhs.clone(), consequent.clone(), alternative.clone())
            {
                return Ok(n);
            }
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
                Ok(Node::UnaryExpression(
                    Operator::Add,
                    Box::new(self.parse_unary_expression()?),
                ))
            }
            Some(Token::Operator(Operator::Sub)) => {
                self.lexer.next();
                Ok(Node::UnaryExpression(
                    Operator::Sub,
                    Box::new(self.parse_unary_expression()?),
                ))
            }
            Some(Token::Operator(Operator::BitwiseNOT)) => {
                self.lexer.next();
                Ok(Node::UnaryExpression(
                    Operator::BitwiseNOT,
                    Box::new(self.parse_unary_expression()?),
                ))
            }
            Some(Token::Operator(Operator::Not)) => {
                self.lexer.next();
                Ok(Node::UnaryExpression(
                    Operator::Not,
                    Box::new(self.parse_unary_expression()?),
                ))
            }
            Some(Token::Await) if self.scope(ParseScope::AsyncFunction) => {
                self.lexer.next();
                Ok(Node::AwaitExpression(Box::new(
                    self.parse_unary_expression()?,
                )))
            }
            _ => self.parse_left_hand_side_expression(),
        }
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
            list.push(self.parse_assignment_expression()?);
        }
        Ok(list)
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

    fn parse_arrow_function(
        &mut self,
        first_arg: Option<Node>,
        more_args: bool,
        kind: FunctionKind,
    ) -> Result<Node, Error> {
        let mut args = match first_arg {
            Some(expr) => {
                if let Node::Identifier(..) = expr {
                    Ok(vec![expr])
                } else {
                    Err(Error::UnexpectedToken)
                }
            }
            None => Ok(Vec::new()),
        }?;
        if more_args {
            args.append(&mut self.parse_identifier_list(Token::RightParen, true)?);
        }
        self.expect(Token::Arrow)?;
        let body = self.parse_block_statement(match kind {
            FunctionKind::Normal => ParseScope::Function,
            FunctionKind::Async => ParseScope::AsyncFunction,
            FunctionKind::Generator => ParseScope::GeneratorFunction,
        })?;
        Ok(Node::ArrowFunctionExpression(args, Box::new(body), kind))
    }

    fn parse_primary_expression(&mut self) -> Result<Node, Error> {
        let token = self.lexer.next();
        match token {
            Some(t) => match t {
                Token::This => Ok(Node::ThisExpression),
                Token::New => Ok(Node::NewExpression(Box::new(
                    self.parse_left_hand_side_expression()?,
                ))),
                Token::Null => Ok(Node::NullLiteral),
                Token::True => Ok(Node::TrueLiteral),
                Token::False => Ok(Node::FalseLiteral),
                Token::Colon => {
                    let name = self.parse_identifier(false)?;
                    Ok(Node::SymbolLiteral(name))
                }
                Token::Operator(Operator::Typeof) => Ok(Node::UnaryExpression(
                    Operator::Typeof,
                    Box::new(self.parse_unary_expression()?),
                )),
                Token::Operator(Operator::Void) => Ok(Node::UnaryExpression(
                    Operator::Void,
                    Box::new(self.parse_unary_expression()?),
                )),
                Token::StringLiteral(v) => Ok(Node::StringLiteral(v)),
                Token::NumberLiteral(v) => Ok(Node::NumberLiteral(v)),
                Token::BackQuote => {
                    let mut quasis = Vec::new();
                    let mut expressions = Vec::new();

                    let mut current = String::new();

                    loop {
                        match self.lexer.chars.next() {
                            Some('$') => {
                                if self.lexer.chars.peek() == Some(&'(') {
                                    quasis.push(current);
                                    current = String::new();
                                    self.lexer.chars.next();
                                    let expr = self.parse_expression()?;
                                    expressions.push(expr);
                                    self.expect(Token::RightParen)?;
                                } else {
                                    current.push('$');
                                }
                            }
                            Some('`') => break,
                            Some(c) => {
                                current.push(c);
                            }
                            None => return Err(Error::UnexpectedEOF),
                        }
                    }

                    quasis.push(current);

                    Ok(Node::TemplateLiteral(quasis, expressions))
                }
                Token::Identifier(v) => Ok(Node::Identifier(v)),
                Token::Function => self.parse_function(true, FunctionKind::Normal),
                Token::Async => {
                    if self.eat(Token::Function) {
                        self.parse_function(true, FunctionKind::Async)
                    } else {
                        self.expect(Token::LeftParen)?;
                        self.parse_arrow_function(None, true, FunctionKind::Async)
                    }
                }
                Token::LeftParen => match self.lexer.peek() {
                    Some(Token::RightParen) => {
                        self.lexer.next();
                        self.parse_arrow_function(None, false, FunctionKind::Normal)
                    }
                    _ => {
                        let expr = self.parse_expression()?;
                        match self.lexer.peek() {
                            Some(Token::Comma) => {
                                self.lexer.next();
                                self.parse_arrow_function(Some(expr), true, FunctionKind::Normal)
                            }
                            Some(Token::RightParen) => {
                                self.lexer.next();
                                if let Some(Token::Arrow) = self.lexer.peek() {
                                    self.parse_arrow_function(
                                        Some(expr),
                                        false,
                                        FunctionKind::Normal,
                                    )
                                } else {
                                    Ok(Node::ParenthesizedExpression(Box::new(expr)))
                                }
                            }
                            _ => Err(Error::UnexpectedToken),
                        }
                    }
                },
                Token::LeftBracket => Ok(Node::ArrayLiteral(
                    self.parse_expression_list(Token::RightBracket)?,
                )),
                Token::LeftBrace => {
                    let mut fields = Vec::new();
                    let mut start = true;
                    while !self.eat(Token::RightBrace) {
                        if start {
                            start = false;
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
                        fields.push(Node::ObjectInitializer(Box::new(name), Box::new(init)));
                    }
                    Ok(Node::ObjectLiteral(fields))
                }
                Token::Operator(Operator::Div) => {
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
                _ => Err(Error::UnexpectedToken),
            },
            None => Err(Error::UnexpectedEOF),
        }
    }
}

#[test]
fn test_parser() {
    macro_rules! hashmap(
        { $($key:expr => $value:expr),+ } => {
            {
                let mut m = ::std::collections::HashMap::new();
                $(
                    m.insert($key.to_string(), $value);
                )+
                m
            }
         };
    );

    assert_eq!(
        Parser::parse(
            r#"
             const a = 1;
             if a { a += 2; }
             if 1 { a += 3; }
             "#
        )
        .unwrap(),
        Node::BlockStatement(
            vec![
                Node::LexicalInitialization(
                    "a".to_string(),
                    Box::new(Node::NumberLiteral(1.into()))
                ),
                Node::IfStatement(
                    Box::new(Node::Identifier("a".to_string())),
                    Box::new(Node::BlockStatement(
                        vec![Node::ExpressionStatement(Box::new(Node::BinaryExpression(
                            Box::new(Node::Identifier("a".to_string())),
                            Operator::Assign,
                            Box::new(Node::BinaryExpression(
                                Box::new(Node::Identifier("a".to_string())),
                                Operator::Add,
                                Box::new(Node::NumberLiteral(2.into())),
                            )),
                        )))],
                        HashMap::new(),
                        false,
                    )),
                ),
                Node::BlockStatement(
                    vec![Node::ExpressionStatement(Box::new(Node::BinaryExpression(
                        Box::new(Node::Identifier("a".to_string())),
                        Operator::Assign,
                        Box::new(Node::BinaryExpression(
                            Box::new(Node::Identifier("a".to_string())),
                            Operator::Add,
                            Box::new(Node::NumberLiteral(3.into())),
                        )),
                    )))],
                    HashMap::new(),
                    false,
                ),
            ],
            hashmap! {
                "a" => false
            },
            true,
        ),
    );

    assert_eq!(
        Parser::parse("while false { 1; }").unwrap(),
        Node::BlockStatement(
            vec![Node::ParenthesizedExpression(Box::new(Node::FalseLiteral))],
            HashMap::new(),
            true,
        ),
    );
}
