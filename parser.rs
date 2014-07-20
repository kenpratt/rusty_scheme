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
        parser.parse_nodes(0)
    }

    fn parse_nodes(&mut self, depth: uint) -> Result<Vec<Node>, ParseError> {
        let mut vec = Vec::new();
        loop {
            match try!(self.parse_node(depth)) {
                Some(node) => {
                    vec.push(node);
                },
                None => {
                    return Ok(vec);
                }
            }
        }
    }

    fn parse_node(&mut self, depth: uint) -> Result<Option<Node>, ParseError> {
        match self.tokens.next() {
            Some(token) => {
                match *token {
                    lexer::OpenParen => {
                        let inner = try!(self.parse_nodes(depth + 1));
                        Ok(Some(List(inner)))
                    },
                    lexer::CloseParen => {
                        if depth > 0 {
                            Ok(None)
                        } else {
                            parse_error!("Unexpected close paren, depth: {}", depth)
                        }
                    },
                    lexer::Quote => {
                        match try!(self.parse_node(depth)) {
                            Some(inner) => {
                                let quoted = List(vec![Identifier("quote".to_str()), inner]);
                                Ok(Some(quoted))
                            },
                            None => parse_error!("Missing quoted value, depth: {}", depth)
                        }
                    },
                    lexer::Quasiquote => {
                        match try!(self.parse_node(depth)) {
                            Some(inner) => {
                                let quoted = List(vec![Identifier("quasiquote".to_str()), inner]);
                                Ok(Some(quoted))
                            },
                            None => parse_error!("Missing quasiquoted value, depth: {}", depth)
                        }
                    }
                    lexer::Unquote => {
                        match try!(self.parse_node(depth)) {
                            Some(inner) => {
                                let quoted = List(vec![Identifier("unquote".to_str()), inner]);
                                Ok(Some(quoted))
                            },
                            None => parse_error!("Missing unquoted value, depth: {}", depth)
                        }
                    }
                    lexer::Identifier(ref val) => {
                        Ok(Some(Identifier(val.clone())))
                    },
                    lexer::Integer(ref val) => {
                        Ok(Some(Integer(val.clone())))
                    },
                    lexer::Boolean(ref val) => {
                        Ok(Some(Boolean(val.clone())))
                    },
                    lexer::String(ref val) => {
                        Ok(Some(String(val.clone())))
                    }
                }
            },
            None => {
                if depth == 0 {
                    Ok(None)
                } else {
                    parse_error!("Unexpected end of input, depth: {}", depth)
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
fn test_quoting() {
    assert_eq!(parse(&vec![lexer::Quote, lexer::OpenParen, lexer::Identifier("a".to_str()), lexer::CloseParen]).unwrap(),
               vec![List(vec![Identifier("quote".to_str()), List(vec![Identifier("a".to_str())])])]);
    assert_eq!(parse(&vec![lexer::OpenParen, lexer::Identifier("list".to_str()), lexer::Quote, lexer::Identifier("a".to_str()), lexer::Identifier("b".to_str()), lexer::CloseParen]).unwrap(),
               vec![List(vec![Identifier("list".to_str()), List(vec![Identifier("quote".to_str()), Identifier("a".to_str())]), Identifier("b".to_str())])]);
}

#[test]
fn test_quasiquoting() {
    assert_eq!(parse(&vec![lexer::Quasiquote, lexer::OpenParen, lexer::Unquote, lexer::Identifier("a".to_str()), lexer::CloseParen]).unwrap(),
               vec![List(vec![Identifier("quasiquote".to_str()), List(vec![List(vec![Identifier("unquote".to_str()), Identifier("a".to_str())])])])]);
    assert_eq!(parse(&vec![lexer::Quasiquote, lexer::OpenParen, lexer::Unquote, lexer::Identifier("a".to_str()), lexer::Identifier("b".to_str()), lexer::Unquote, lexer::Identifier("c".to_str()), lexer::CloseParen]).unwrap(),
               vec![List(vec![Identifier("quasiquote".to_str()), List(vec![List(vec![Identifier("unquote".to_str()), Identifier("a".to_str())]), Identifier("b".to_str()), List(vec![Identifier("unquote".to_str()), Identifier("c".to_str())])])])]);
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
