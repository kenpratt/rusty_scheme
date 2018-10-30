use crate::reader::lexer::*;

use std::fmt;
use std::slice;

pub fn parse(tokens: &Vec<Token>) -> Result<Vec<Node>, ParseError> {
    Parser::parse(tokens)
}

#[derive(PartialEq, Clone, Debug)]
pub enum Node {
    Identifier(String),
    Integer(i64),
    Boolean(bool),
    String(String),
    List(Vec<Node>),
}

pub struct ParseError {
    message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ParseError: {}", self.message)
    }
}
impl fmt::Debug for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ParseError: {}", self.message)
    }
}

macro_rules! parse_error {
    ($($arg:tt)*) => (
        return Err(ParseError { message: format!($($arg)*)})
    )
}

struct Parser<'a> {
    tokens: slice::Iter<'a, Token>,
}

impl<'a> Parser<'a> {
    fn parse(tokens: &Vec<Token>) -> Result<Vec<Node>, ParseError> {
        let mut parser = Parser { tokens: tokens.iter() };
        parser.parse_nodes(0)
    }

    fn parse_nodes(&mut self, depth: u32) -> Result<Vec<Node>, ParseError> {
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

    fn parse_node(&mut self, depth: u32) -> Result<Option<Node>, ParseError> {
        match self.tokens.next() {
            Some(token) => {
                match *token {
                    Token::OpenParen => {
                        let inner = try!(self.parse_nodes(depth + 1));
                        Ok(Some(Node::List(inner)))
                    },
                    Token::CloseParen => {
                        if depth > 0 {
                            Ok(None)
                        } else {
                            parse_error!("Unexpected close paren, depth: {}", depth)
                        }
                    },
                    Token::Quote => {
                        match try!(self.parse_node(depth)) {
                            Some(inner) => {
                                let quoted = Node::List(vec![Node::Identifier("quote".to_string()), inner]);
                                Ok(Some(quoted))
                            },
                            None => parse_error!("Missing quoted value, depth: {}", depth)
                        }
                    },
                    Token::Quasiquote => {
                        match try!(self.parse_node(depth)) {
                            Some(inner) => {
                                let quoted = Node::List(vec![Node::Identifier("quasiquote".to_string()), inner]);
                                Ok(Some(quoted))
                            },
                            None => parse_error!("Missing quasiquoted value, depth: {}", depth)
                        }
                    }
                    Token::Unquote => {
                        match try!(self.parse_node(depth)) {
                            Some(inner) => {
                                let quoted = Node::List(vec![Node::Identifier("unquote".to_string()), inner]);
                                Ok(Some(quoted))
                            },
                            None => parse_error!("Missing unquoted value, depth: {}", depth)
                        }
                    }
                    Token::Identifier(ref val) => {
                        Ok(Some(Node::Identifier(val.clone())))
                    },
                    Token::Integer(ref val) => {
                        Ok(Some(Node::Integer(val.clone())))
                    },
                    Token::Boolean(ref val) => {
                        Ok(Some(Node::Boolean(val.clone())))
                    },
                    Token::String(ref val) => {
                        Ok(Some(Node::String(val.clone())))
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
fn test_parser_simple() {
    assert_eq!(parse(&vec![Token::OpenParen, Token::Identifier("+".to_string()), Token::CloseParen]).unwrap(),
               vec![Node::List(vec![Node::Identifier("+".to_string())])]);
}

#[test]
fn test_parser_nested() {
    assert_eq!(parse(&vec![Token::OpenParen, Token::Identifier("+".to_string()), Token::OpenParen, Token::Identifier("+".to_string()), Token::Integer(1), Token::OpenParen, Token::Identifier("+".to_string()), Token::Integer(3), Token::Integer(4), Token::CloseParen, Token::CloseParen, Token::Integer(5), Token::CloseParen]).unwrap(),
               vec![Node::List(vec![Node::Identifier("+".to_string()), Node::List(vec![Node::Identifier("+".to_string()), Node::Integer(1), Node::List(vec![Node::Identifier("+".to_string()), Node::Integer(3), Node::Integer(4)])]), Node::Integer(5)])]);
}

#[test]
fn test_parser_quoting() {
    assert_eq!(parse(&vec![Token::Quote, Token::OpenParen, Token::Identifier("a".to_string()), Token::CloseParen]).unwrap(),
               vec![Node::List(vec![Node::Identifier("quote".to_string()), Node::List(vec![Node::Identifier("a".to_string())])])]);
    assert_eq!(parse(&vec![Token::OpenParen, Token::Identifier("list".to_string()), Token::Quote, Token::Identifier("a".to_string()), Token::Identifier("b".to_string()), Token::CloseParen]).unwrap(),
               vec![Node::List(vec![Node::Identifier("list".to_string()), Node::List(vec![Node::Identifier("quote".to_string()), Node::Identifier("a".to_string())]), Node::Identifier("b".to_string())])]);
}

#[test]
fn test_parser_quasiquoting() {
    assert_eq!(parse(&vec![Token::Quasiquote, Token::OpenParen, Token::Unquote, Token::Identifier("a".to_string()), Token::CloseParen]).unwrap(),
               vec![Node::List(vec![Node::Identifier("quasiquote".to_string()), Node::List(vec![Node::List(vec![Node::Identifier("unquote".to_string()), Node::Identifier("a".to_string())])])])]);
    assert_eq!(parse(&vec![Token::Quasiquote, Token::OpenParen, Token::Unquote, Token::Identifier("a".to_string()), Token::Identifier("b".to_string()), Token::Unquote, Token::Identifier("c".to_string()), Token::CloseParen]).unwrap(),
               vec![Node::List(vec![Node::Identifier("quasiquote".to_string()), Node::List(vec![Node::List(vec![Node::Identifier("unquote".to_string()), Node::Identifier("a".to_string())]), Node::Identifier("b".to_string()), Node::List(vec![Node::Identifier("unquote".to_string()), Node::Identifier("c".to_string())])])])]);
}

#[test]
fn test_parser_bad_syntax() {
    assert_eq!(parse(&vec![Token::CloseParen]).err().unwrap().to_string(),
               "ParseError: Unexpected close paren, depth: 0");
    assert_eq!(parse(&vec![Token::OpenParen, Token::OpenParen, Token::CloseParen]).err().unwrap().to_string(),
               "ParseError: Unexpected end of input, depth: 1");
    assert_eq!(parse(&vec![Token::OpenParen, Token::CloseParen, Token::CloseParen]).err().unwrap().to_string(),
               "ParseError: Unexpected close paren, depth: 0");
    assert_eq!(parse(&vec![Token::OpenParen, Token::OpenParen, Token::CloseParen, Token::OpenParen, Token::OpenParen, Token::CloseParen]).err().unwrap().to_string(),
               "ParseError: Unexpected end of input, depth: 2");
}
