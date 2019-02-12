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
    NumberLiteral(f64),
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
    If,
    Else,
    Return,
    Import,
    Export,
    Default,
    From,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    NullLiteral,
    TrueLiteral,
    FalseLiteral,
    NumberLiteral(f64),
    StringLiteral(String),
    StatementList(Vec<Node>),
    PropertyInitializer(String, Box<Node>), // key, value
    ObjectLiteral(Vec<Node>),
    ArrayLiteral(Vec<Node>),
    Identifier(String),
    BlockStatement(Vec<Node>),
    ReturnStatement(Box<Node>),
    ThrowStatement(Box<Node>),
    TryStatement(Box<Node>, Box<Node>),               // try, catch
    IfStatement(Box<Node>, Box<Node>),                // test, consequent
    IfElseStatement(Box<Node>, Box<Node>, Box<Node>), // test, consequent, alternative
    BoundTryStatement(Box<Node>, String, Box<Node>),  // try, binding, catch
    ExpressionStatement(Box<Node>),
    NewExpression(Box<Node>),
    MemberExpression(Box<Node>, String), // base, property
    ComputedMemberExpression(Box<Node>, Box<Node>), // base, property expression
    ThisExpression,
    CallExpression(Box<Node>, Vec<Node>), // callee, arguments
    UnaryExpression(Operator, Box<Node>), // op x
    BinaryExpression(Box<Node>, Operator, Box<Node>), // x op y
    ConditionalExpression(Box<Node>, Box<Node>, Box<Node>), // test, consequent, alternative
    FunctionDeclaration(String, Vec<String>, Box<Node>), // name, args, body
    FunctionExpression(Option<String>, Vec<String>, Box<Node>), // name, args, body
    LexicalDeclaration(String, Box<Node>, bool), // identifier, initial value, mutable
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
                        while let Some(c) = self.chars.peek() {
                            match c {
                                '0'...'9' | '.' => {
                                    str.push(self.chars.next().unwrap());
                                }
                                _ => break,
                            }
                        }
                        let num = str.parse::<f64>().expect("Invalid number");
                        Some(Token::NumberLiteral(num))
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
                            "if" => Token::If,
                            "else" => Token::Else,
                            "new" => Token::New,
                            "import" => Token::Import,
                            "export" => Token::Export,
                            "default" => Token::Default,
                            "from" => Token::From,
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
                    '/' => Some(match self.chars.peek() {
                        Some('=') => {
                            self.chars.next();
                            Token::Operator(Operator::DivAssign)
                        }
                        _ => Token::Operator(Operator::Div),
                    }),
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

#[derive(PartialEq)]
enum ParseScope {
    TopLevel,
    Block,
    Function,
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    scope: Vec<ParseScope>,
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
            scope: vec![ParseScope::TopLevel],
        };
        let mut nodes = Vec::new();
        loop {
            match parser.parse_statement_list_item() {
                Ok(node) => nodes.push(node),
                Err(Error::NormalEOF) => break,
                Err(e) => {
                    return Err(e);
                }
            }
        }
        Ok(Node::StatementList(nodes))
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
            identifiers.push(self.parse_identifier()?);
        }
        Ok(identifiers)
    }

    fn parse_function(&mut self, expression: bool) -> Result<Node, Error> {
        let name = if expression {
            if let Some(Token::Identifier(_)) = self.lexer.peek() {
                Some(self.parse_identifier()?)
            } else {
                None
            }
        } else {
            Some(self.parse_identifier()?)
        };
        self.expect(Token::LeftParen)?;
        let args = self.parse_identifier_list(Token::RightParen)?;
        self.scope.push(ParseScope::Function);
        let body = self.parse_block_statement()?;
        self.scope.pop();
        Ok(if expression {
            Node::FunctionExpression(name, args, Box::new(body))
        } else {
            Node::FunctionDeclaration(name.unwrap(), args, Box::new(body))
        })
    }

    fn parse_statement_list_item(&mut self) -> Result<Node, Error> {
        match self.lexer.peek() {
            None => Err(Error::NormalEOF),
            Some(Token::LeftBrace) => {
                self.scope.push(ParseScope::Block);
                let b = self.parse_block_statement();
                self.scope.pop();
                b
            }
            Some(Token::Let) | Some(Token::Const) => self.parse_lexical_declaration(),
            Some(Token::Function) => {
                self.lexer.next();
                self.parse_function(false)
            }
            Some(Token::Return) if self.scope.last().unwrap() == &ParseScope::Function => {
                self.lexer.next();
                if self.eat(Token::Semicolon) {
                    Ok(Node::ReturnStatement(Box::new(Node::NullLiteral)))
                } else {
                    let expr = self.parse_expression()?;
                    self.expect(Token::Semicolon)?;
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
                let try_clause = self.parse_block_statement()?;
                self.expect(Token::Catch)?;
                let mut binding = None;
                if self.eat(Token::LeftParen) {
                    binding = Some(self.parse_identifier()?);
                    self.expect(Token::RightParen)?;
                }
                let catch_clause = self.parse_block_statement()?;
                match binding {
                    Some(b) => Ok(Node::BoundTryStatement(
                        Box::new(try_clause),
                        b,
                        Box::new(catch_clause),
                    )),
                    None => Ok(Node::TryStatement(
                        Box::new(try_clause),
                        Box::new(catch_clause),
                    )),
                }
            }
            Some(Token::If) => {
                self.lexer.next();
                self.expect(Token::LeftParen)?;
                let test = self.parse_expression()?;
                self.expect(Token::RightParen)?;
                let consequent = self.parse_block_statement()?;
                if self.eat(Token::Else) {
                    let alternative = self.parse_block_statement()?;
                    match self.fold_conditional(test.clone(), consequent.clone(), alternative.clone()) {
                        Ok(n) => return Ok(n),
                        Err(_) => {}
                    }
                    Ok(Node::IfElseStatement(
                        Box::new(test),
                        Box::new(consequent),
                        Box::new(alternative),
                    ))
                } else {
                    match self.fold_conditional(test.clone(), consequent.clone(), Node::ExpressionStatement(Box::new(Node::NullLiteral))) {
                        Ok(n) => return Ok(n),
                        Err(_) => {}
                    }
                    Ok(Node::IfStatement(Box::new(test), Box::new(consequent)))
                }
            }
            Some(Token::Export) if self.scope.last().unwrap() == &ParseScope::TopLevel => {
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
            Some(Token::Import) if self.scope.last().unwrap() == &ParseScope::TopLevel => {
                self.lexer.next();
                match self.lexer.peek() {
                    // import "specifier";
                    Some(Token::StringLiteral(_)) => {
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
                        let bindings = self.parse_identifier_list(Token::RightBrace)?;
                        self.expect(Token::From)?;
                        match self.lexer.next() {
                            Some(Token::StringLiteral(s)) => {
                                self.expect(Token::Semicolon)?;
                                Ok(Node::ImportNamedDeclaration(s, bindings))
                            }
                            Some(Token::Identifier(ref s)) if s == "standard" => {
                                self.expect(Token::Colon)?;
                                let namespace = self.parse_identifier()?;
                                self.expect(Token::Semicolon)?;
                                Ok(Node::ImportStandardDeclaration(namespace, bindings))
                            }
                            _ => Err(Error::UnexpectedToken),
                        }
                    }
                    // import binding from "specifier";
                    Some(Token::Identifier(_)) => {
                        let binding = self.parse_identifier()?;
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
                let name = self.parse_identifier()?;
                self.expect(Token::Operator(Operator::Assign))?;
                let value = self.parse_assignment_expression()?;
                self.expect(Token::Semicolon)?;
                Ok(Node::LexicalDeclaration(
                    name,
                    Box::new(value),
                    match decl {
                        Token::Let => true,
                        Token::Const => false,
                        _ => unreachable!(),
                    },
                ))
            }
            _ => Err(Error::UnexpectedToken),
        }
    }

    fn parse_block_statement(&mut self) -> Result<Node, Error> {
        self.expect(Token::LeftBrace)?;
        let mut nodes = Vec::new();
        while !self.eat(Token::RightBrace) {
            match self.parse_statement_list_item() {
                Ok(node) => nodes.push(node),
                Err(Error::NormalEOF) => break,
                Err(e) => {
                    return Err(e);
                }
            }
        }
        Ok(Node::BlockStatement(nodes))
    }

    fn parse_expression_statement(&mut self) -> Result<Node, Error> {
        let expression = self.parse_expression()?;
        Ok(Node::ExpressionStatement(Box::new(expression)))
    }

    fn parse_expression(&mut self) -> Result<Node, Error> {
        self.parse_assignment_expression()
    }

    fn parse_identifier(&mut self) -> Result<String, Error> {
        match self.lexer.next() {
            Some(Token::Identifier(name)) => Ok(name),
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
                Node::CallExpression(_, _)
                | Node::UnaryExpression(_, _)
                | Node::NullLiteral
                | Node::TrueLiteral
                | Node::FalseLiteral
                | Node::ArrayLiteral(_)
                | Node::ObjectLiteral(_)
                | Node::NumberLiteral(_)
                | Node::StringLiteral(_) => {
                    return Err(Error::UnexpectedToken);
                }
                _ => {}
            },
            _ => {}
        };

        match &left {
            Node::NumberLiteral(lnum) => match right {
                Node::NumberLiteral(rnum) => match op {
                    Operator::Add => return Ok(Node::NumberLiteral(lnum + rnum)),
                    Operator::Sub => return Ok(Node::NumberLiteral(lnum - rnum)),
                    Operator::Mul => return Ok(Node::NumberLiteral(lnum * rnum)),
                    Operator::Div => return Ok(Node::NumberLiteral(lnum / rnum)),
                    Operator::BitwiseOR => {
                        return Ok(Node::NumberLiteral(
                            (lnum.round() as i64 | rnum.round() as i64) as f64,
                        ));
                    }
                    Operator::BitwiseAND => {
                        return Ok(Node::NumberLiteral(
                            (lnum.round() as i64 & rnum.round() as i64) as f64,
                        ));
                    }
                    Operator::BitwiseXOR => {
                        return Ok(Node::NumberLiteral(
                            (lnum.round() as i64 ^ rnum.round() as i64) as f64,
                        ));
                    }
                    Operator::LeftShift => {
                        return Ok(Node::NumberLiteral(
                            ((lnum.round() as i64) << rnum.round() as i64) as f64,
                        ));
                    }
                    Operator::RightShift => {
                        return Ok(Node::NumberLiteral(
                            ((lnum.round() as i64) >> rnum.round() as i64) as f64,
                        ));
                    }
                    Operator::Pow => return Ok(Node::NumberLiteral(lnum.powf(rnum))),
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
                },
                _ => {}
            },
            Node::StringLiteral(lstr) => match &right {
                Node::StringLiteral(rstr) => match op {
                    Operator::Add => {
                        return Ok(Node::StringLiteral(format!("{}{}", lstr, rstr)));
                    }
                    _ => {}
                },
                _ => {}
            },
            _ => {}
        };

        Ok(Node::BinaryExpression(Box::new(left), op, Box::new(right)))
    }

    fn fold_conditional(&self, test: Node, consequent: Node, alternative: Node) -> Result<Node, ()> {
        match test {
            Node::TrueLiteral => return Ok(consequent),
            Node::NumberLiteral(n) => return if n > 0f64 { Ok(consequent) } else { Ok(alternative) },
            Node::StringLiteral(s) => return if s.chars().count() > 0 { Ok(consequent) } else { Ok(alternative) },
            Node::FalseLiteral => return Ok(alternative),
            Node::NullLiteral => return Ok(alternative),
            Node::ArrayLiteral(_) => return Ok(consequent),
            Node::ObjectLiteral(_) => return Ok(consequent),
            Node::UnaryExpression(Operator::Void, _) => return Ok(alternative),
            _ => Err(()),
        }
    }

    fn parse_conditional_expression(&mut self) -> Result<Node, Error> {
        let lhs = self.parse_logical_or_expression()?;

        if self.eat(Token::Question) {
            let consequent = self.parse_assignment_expression()?;
            self.expect(Token::Colon)?;
            let alternative = self.parse_assignment_expression()?;
            match self.fold_conditional(lhs.clone(), consequent.clone(), alternative.clone()) {
                Ok(n) => return Ok(n),
                Err(_) => {}
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
                let property = self.parse_identifier()?;
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
                Token::NumberLiteral(v) => Ok(Node::NumberLiteral(v)),
                Token::Identifier(v) => Ok(Node::Identifier(v)),
                Token::Function => {
                    self.lexer.next();
                    self.parse_function(true)
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
                        let name = self.parse_identifier()?;
                        let mut init;
                        if self.eat(Token::Colon) {
                            init = self.parse_expression()?;
                        } else {
                            init = self.parse_function(true)?
                        }
                        fields.push(Node::PropertyInitializer(name, Box::new(init)));
                    }
                    Ok(Node::ObjectLiteral(fields))
                }
                _ => Err(Error::UnexpectedToken),
            },
            None => Err(Error::UnexpectedEOF),
        }
    }
}
