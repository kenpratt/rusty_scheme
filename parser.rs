use lexer;
use lexer::Token;

use std::fmt;
use std::slice;

pub fn parse(tokens: &Vec<Token>) -> Result<Vec<Node>, ParseError> {
    Parser::parse(tokens)
}

#[deriving(Show, PartialEq, Clone)]
pub enum Node {
    Identifier(String),
    Integer(int),
    Boolean(bool),
    String(String),
    List(Vec<Node>),
}

pub struct ParseError {
    message: String,
}

impl fmt::Show for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ParseError: {}", self.message)
    }
}

macro_rules! parse_error(
    ($($arg:tt)*) => (
        return Err(ParseError { message: format!($($arg)*)})
    )
)

struct Parser<'a> {
    tokens: slice::Items<'a, Token>
}

impl<'a> Parser<'a> {
    fn parse(tokens: &Vec<Token>) -> Result<Vec<Node>, ParseError> {
        let mut parser = Parser { tokens: tokens.iter() };
        parser.run(0)
    }

    fn run(&mut self, depth: uint) -> Result<Vec<Node>, ParseError> {
        let mut vec = Vec::new();
        loop {
            match self.tokens.next() {
                Some(token) => {
                    match *token {
                        lexer::OpenParen => {
                            let inner = try!(self.run(depth + 1));
                            vec.push(List(inner));
                        },
                        lexer::CloseParen => {
                            if depth > 0 {
                                return Ok(vec);
                            } else {
                                parse_error!("Unexpected close paren, depth: {}", depth);
                            }
                        },
                        lexer::Identifier(ref val) => {
                            vec.push(Identifier(val.clone()));
                        },
                        lexer::Integer(ref val) => {
                            vec.push(Integer(val.clone()));
                        },
                        lexer::Boolean(ref val) => {
                            vec.push(Boolean(val.clone()));
                        },
                        lexer::String(ref val) => {
                            vec.push(String(val.clone()));
                        }
                    };
                },
                None => {
                    if depth == 0 {
                        return Ok(vec);
                    } else {
                        parse_error!("Unexpected end of input, depth: {}", depth);
                    }
                }
            }
        }
    }
}

#[test]
fn test_simple() {
    assert_eq!(parse(&vec![lexer::OpenParen, lexer::Identifier("+".to_str()), lexer::CloseParen]).unwrap(),
               vec![List(vec![Identifier("+".to_str())])]);
}

#[test]
fn test_nested() {
    assert_eq!(parse(&vec![lexer::OpenParen, lexer::Identifier("+".to_str()), lexer::OpenParen, lexer::Identifier("+".to_str()), lexer::Integer(1), lexer::OpenParen, lexer::Identifier("+".to_str()), lexer::Integer(3), lexer::Integer(4), lexer::CloseParen, lexer::CloseParen, lexer::Integer(5), lexer::CloseParen]).unwrap(),
               vec![List(vec![Identifier("+".to_str()), List(vec![Identifier("+".to_str()), Integer(1), List(vec![Identifier("+".to_str()), Integer(3), Integer(4)])]), Integer(5)])]);
}

#[test]
fn test_bad_syntax() {
    assert_eq!(parse(&vec![lexer::CloseParen]).err().unwrap().to_str().as_slice(),
               "ParseError: Unexpected close paren, depth: 0");
    assert_eq!(parse(&vec![lexer::OpenParen, lexer::OpenParen, lexer::CloseParen]).err().unwrap().to_str().as_slice(),
               "ParseError: Unexpected end of input, depth: 1");
    assert_eq!(parse(&vec![lexer::OpenParen, lexer::CloseParen, lexer::CloseParen]).err().unwrap().to_str().as_slice(),
               "ParseError: Unexpected close paren, depth: 0");
    assert_eq!(parse(&vec![lexer::OpenParen, lexer::OpenParen, lexer::CloseParen, lexer::OpenParen, lexer::OpenParen, lexer::CloseParen]).err().unwrap().to_str().as_slice(),
               "ParseError: Unexpected end of input, depth: 2");
}
