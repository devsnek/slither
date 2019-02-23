use num::BigInt;
use std::collections::HashMap;
use std::iter::Peekable;
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
    FloatLiteral(f64),
    IntegerLiteral(BigInt),
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
    Comma,
    Throw,
    Try,
    Catch,
    Finally,
    If,
    Else,
    While,
    Return,
    Import,
    Export,
    Default,
    From,
    Async,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    NullLiteral,
    TrueLiteral,
    FalseLiteral,
    FloatLiteral(f64),
    IntegerLiteral(BigInt),
    StringLiteral(String),
    Initializer(String, Box<Node>), // Name, value
    ObjectLiteral(Vec<Node>),       // initialiers
    ArrayLiteral(Vec<Node>),
    Identifier(String),
    BlockStatement(Vec<Node>, HashMap<String, bool>), // nodes, declarations
    ReturnStatement(Box<Node>),
    ThrowStatement(Box<Node>),
    IfStatement(Box<Node>, Box<Node>), // test, consequent
    IfElseStatement(Box<Node>, Box<Node>, Box<Node>), // test, consequent, alternative
    WhileStatement(Box<Node>, Box<Node>), // test, body
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
    FunctionDeclaration(String, Vec<Node>, Box<Node>), // name, args, body
    FunctionExpression(Option<String>, Vec<Node>, Box<Node>), // name, args, body
    ArrowFunctionExpression(Vec<Node>, Box<Node>), // args, body
    ParenthesizedExpression(Box<Node>),   // expr
    LexicalInitialization(String, Box<Node>), // identifier, initial value
    ImportDeclaration(String),            // specifier
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
                        let mut float = false;
                        while let Some(c) = self.chars.peek() {
                            match c {
                                '0'...'9' => {
                                    str.push(self.chars.next().unwrap());
                                }
                                '.' => {
                                    float = true;
                                    str.push(self.chars.next().unwrap());
                                }
                                _ => break,
                            }
                        }
                        if float {
                            let num = str
                                .parse::<f64>()
                                .unwrap_or_else(|_| panic!("Invalid float {}", str));
                            Some(Token::FloatLiteral(num))
                        } else {
                            let num = str
                                .parse::<BigInt>()
                                .unwrap_or_else(|_| panic!("Invalid integer {}", str));
                            Some(Token::IntegerLiteral(num))
                        }
                    }
                    '"' | '\'' => {
                        let mut str = String::new();
                        while let Some(c) = self.chars.peek() {
                            if c == &char {
                                self.chars.next();
                                break;
                            }
                            str.push(self.chars.next().unwrap());
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
                            "if" => Token::If,
                            "else" => Token::Else,
                            "while" => Token::While,
                            "new" => Token::New,
                            "import" => Token::Import,
                            "export" => Token::Export,
                            "default" => Token::Default,
                            "from" => Token::From,
                            "async" => Token::Async,
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
}

#[derive(PartialEq, Clone, Copy)]
enum ParseScope {
    TopLevel,
    Block,
    Function,
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    scope_stack: Vec<ParseScope>,
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
            scope_stack: Vec::new(),
            lex_stack: Vec::new(),
        };
        parser.parse_block_statement(ParseScope::TopLevel)
    }

    fn scope(&self, scope: ParseScope) -> bool {
        match scope {
            ParseScope::TopLevel => self.scope_stack.last().unwrap() == &ParseScope::TopLevel,
            ParseScope::Block => self.scope_stack.last().unwrap() == &ParseScope::Block,
            ParseScope::Function => self.scope_stack.contains(&ParseScope::Function),
        }
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

    fn parse_function(&mut self, expression: bool) -> Result<Node, Error> {
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
        let body = self.parse_block_statement(ParseScope::Function)?;
        Ok(if expression {
            Node::FunctionExpression(name, args, Box::new(body))
        } else {
            let name = name.unwrap();
            let scope = self.lex_stack.last_mut().unwrap();
            if scope.contains_key(&name) {
                return Err(Error::DuplicateBinding);
            } else {
                scope.insert(name.clone(), false);
            }
            Node::FunctionDeclaration(name, args, Box::new(body))
        })
    }

    fn parse_statement_list_item(&mut self) -> Result<Node, Error> {
        match self.lexer.peek() {
            None => Err(Error::NormalEOF),
            Some(Token::LeftBrace) => self.parse_block_statement(ParseScope::Block),
            Some(Token::Let) | Some(Token::Const) => self.parse_lexical_declaration(),
            Some(Token::Function) => {
                self.lexer.next();
                self.parse_function(false)
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
                    if let Ok(n) =
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
                    if let Ok(n) = self.fold_conditional(
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
                let body = self.parse_block_statement(ParseScope::Block)?;
                Ok(Node::WhileStatement(Box::new(test), Box::new(body)))
            }
            Some(Token::Export) if self.scope(ParseScope::TopLevel) => {
                self.lexer.next();
                let decl = match self.lexer.peek() {
                    Some(Token::Let) | Some(Token::Const) => self.parse_lexical_declaration(),
                    Some(Token::Function) => {
                        self.lexer.next();
                        self.parse_function(false)
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
        self.scope_stack.push(scope);
        self.lex_stack.push(HashMap::new());
        let mut nodes = Vec::new();
        while !self.eat(Token::RightBrace) {
            match self.parse_statement_list_item() {
                Ok(node) => nodes.push(node),
                Err(Error::NormalEOF) if scope == ParseScope::TopLevel => break,
                Err(e) => {
                    self.scope_stack.pop();
                    self.lex_stack.pop();
                    return Err(e);
                }
            }
        }
        self.scope_stack.pop();
        let declarations = self.lex_stack.pop().unwrap();
        Ok(Node::BlockStatement(nodes, declarations))
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
            Some(Token::Async) if allow_keyword => Ok("async".to_string()),
            Some(Token::Throw) if allow_keyword => Ok("throw".to_string()),
            Some(Token::Catch) if allow_keyword => Ok("catch".to_string()),
            Some(Token::True) if allow_keyword => Ok("true".to_string()),
            Some(Token::False) if allow_keyword => Ok("false".to_string()),
            Some(Token::Null) if allow_keyword => Ok("null".to_string()),
            Some(Token::This) if allow_keyword => Ok("this".to_string()),
            Some(Token::Class) if allow_keyword => Ok("class".to_string()),
            /*
            "function" => Token::Function,
            "let" => Token::Let,
            "const" => Token::Const,
            "throw" => Token::Throw,
            "return" => Token::Return,
            "try" => Token::Try,
            "catch" => Token::Catch,
            "finally" => Token::Finally,
            "if" => Token::If,
            "else" => Token::Else,
            "new" => Token::New,
            "import" => Token::Import,
            "export" => Token::Export,
            "default" => Token::Default,
            "from" => Token::From,
            "async" => Token::Async,
            "typeof" => Token::Operator(Operator::Typeof),
            "void" => Token::Operator(Operator::Void),
            */
            _ => Err(Error::UnexpectedToken),
        }
    }

    fn parse_assignment_expression(&mut self) -> Result<Node, Error> {
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

        match self.lexer.peek() {
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
                | Node::FloatLiteral(..)
                | Node::StringLiteral(..) => {
                    return Err(Error::UnexpectedToken);
                }
                _ => {}
            },
            _ => {}
        };

        match &left {
            Node::FloatLiteral(lnum) => {
                if let Node::FloatLiteral(rnum) = right {
                    match op {
                        Operator::Add => return Ok(Node::FloatLiteral(lnum + rnum)),
                        Operator::Sub => return Ok(Node::FloatLiteral(lnum - rnum)),
                        Operator::Mul => return Ok(Node::FloatLiteral(lnum * rnum)),
                        Operator::Div => return Ok(Node::FloatLiteral(lnum / rnum)),
                        Operator::BitwiseOR => {
                            return Ok(Node::FloatLiteral(
                                (lnum.round() as i64 | rnum.round() as i64) as f64,
                            ));
                        }
                        Operator::BitwiseAND => {
                            return Ok(Node::FloatLiteral(
                                (lnum.round() as i64 & rnum.round() as i64) as f64,
                            ));
                        }
                        Operator::BitwiseXOR => {
                            return Ok(Node::FloatLiteral(
                                (lnum.round() as i64 ^ rnum.round() as i64) as f64,
                            ));
                        }
                        Operator::LeftShift => {
                            return Ok(Node::FloatLiteral(
                                ((lnum.round() as i64) << rnum.round() as i64) as f64,
                            ));
                        }
                        Operator::RightShift => {
                            return Ok(Node::FloatLiteral(
                                ((lnum.round() as i64) >> rnum.round() as i64) as f64,
                            ));
                        }
                        Operator::Pow => return Ok(Node::FloatLiteral(lnum.powf(rnum))),
                        Operator::LessThan => {
                            if *lnum < rnum {
                                return Ok(Node::TrueLiteral);
                            } else {
                                return Ok(Node::FalseLiteral);
                            }
                        }
                        Operator::GreaterThan => {
                            if *lnum > rnum {
                                return Ok(Node::TrueLiteral);
                            } else {
                                return Ok(Node::FalseLiteral);
                            }
                        }
                        Operator::LessThanOrEqual => {
                            if *lnum <= rnum {
                                return Ok(Node::TrueLiteral);
                            } else {
                                return Ok(Node::FalseLiteral);
                            }
                        }
                        Operator::GreaterThanOrEqual => {
                            if *lnum >= rnum {
                                return Ok(Node::TrueLiteral);
                            } else {
                                return Ok(Node::FalseLiteral);
                            }
                        }
                        Operator::Equal => {
                            if *lnum == rnum {
                                return Ok(Node::TrueLiteral);
                            } else {
                                return Ok(Node::FalseLiteral);
                            }
                        }
                        Operator::NotEqual => {
                            if *lnum == rnum {
                                return Ok(Node::FalseLiteral);
                            } else {
                                return Ok(Node::TrueLiteral);
                            }
                        }
                        _ => {}
                    }
                }
            }
            Node::StringLiteral(lstr) => match &right {
                Node::StringLiteral(rstr) if Operator::Add == op => {
                    return Ok(Node::StringLiteral(format!("{}{}", lstr, rstr)));
                }
                _ => {}
            },
            _ => {}
        };

        Ok(Node::BinaryExpression(Box::new(left), op, Box::new(right)))
    }

    fn fold_conditional(
        &self,
        test: Node,
        consequent: Node,
        alternative: Node,
    ) -> Result<Node, ()> {
        match test {
            Node::TrueLiteral => Ok(consequent),
            Node::FloatLiteral(n) => {
                if n > 0f64 {
                    Ok(consequent)
                } else {
                    Ok(alternative)
                }
            }
            Node::StringLiteral(s) => {
                if s.chars().count() > 0 {
                    Ok(consequent)
                } else {
                    Ok(alternative)
                }
            }
            Node::FalseLiteral => Ok(alternative),
            Node::NullLiteral => Ok(alternative),
            Node::ArrayLiteral(..) => Ok(consequent),
            Node::ObjectLiteral(..) => Ok(consequent),
            Node::UnaryExpression(Operator::Void, ..) => Ok(alternative),
            _ => Err(()),
        }
    }

    fn parse_conditional_expression(&mut self) -> Result<Node, Error> {
        let lhs = self.parse_logical_or_expression()?;

        if self.eat(Token::Question) {
            let consequent = self.parse_assignment_expression()?;
            self.expect(Token::Colon)?;
            let alternative = self.parse_assignment_expression()?;
            if let Ok(n) =
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
        match self.lexer.peek() {
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
        no_args: bool,
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
        if !no_args {
            args.append(&mut self.parse_identifier_list(Token::RightParen, true)?);
        }
        self.expect(Token::Arrow)?;
        let body = self.parse_block_statement(ParseScope::Function)?;
        Ok(Node::ArrowFunctionExpression(args, Box::new(body)))
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
                Token::Operator(Operator::Typeof) => Ok(Node::UnaryExpression(
                    Operator::Typeof,
                    Box::new(self.parse_unary_expression()?),
                )),
                Token::Operator(Operator::Void) => Ok(Node::UnaryExpression(
                    Operator::Void,
                    Box::new(self.parse_unary_expression()?),
                )),
                Token::StringLiteral(v) => Ok(Node::StringLiteral(v)),
                Token::FloatLiteral(v) => Ok(Node::FloatLiteral(v)),
                Token::IntegerLiteral(v) => Ok(Node::IntegerLiteral(v)),
                Token::Identifier(v) => Ok(Node::Identifier(v)),
                Token::Function => self.parse_function(true),
                Token::LeftParen => {
                    match self.lexer.peek() {
                        Some(Token::RightParen) => {
                            // arrow function
                            self.lexer.next();
                            self.parse_arrow_function(None, true)
                        }
                        _ => {
                            let expr = self.parse_expression()?;
                            let mut no_args = false;
                            if self.eat(Token::Comma) || {
                                no_args = self.eat(Token::RightParen);
                                no_args
                            } {
                                // arrow function
                                self.parse_arrow_function(Some(expr), no_args)
                            } else {
                                Ok(Node::ParenthesizedExpression(Box::new(expr)))
                            }
                        }
                    }
                }
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
                        let name = self.parse_identifier(true)?;
                        let mut init;
                        if self.eat(Token::Colon) {
                            init = self.parse_expression()?;
                        } else {
                            init = self.parse_function(true)?
                        }
                        fields.push(Node::Initializer(name, Box::new(init)));
                    }
                    Ok(Node::ObjectLiteral(fields))
                }
                _ => Err(Error::UnexpectedToken),
            },
            None => Err(Error::UnexpectedEOF),
        }
    }
}
