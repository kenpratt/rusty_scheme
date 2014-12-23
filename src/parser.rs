extern crate core;

use lexer::*;

use std::fmt;

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

macro_rules! parse_error {
    ($($arg:tt)*) => (
        return Err(ParseError { message: format!($($arg)*)})
    )
}

struct Parser<'a> {
    tokens: core::slice::Iter<'a, Token>,
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
fn test_simple() {
    assert_eq!(parse(&vec![Token::OpenParen, TIdentifier("+".to_string()), TCloseParen]).unwrap(),
               vec![Node::List(vec![Node::Identifier("+".to_string())])]);
}

#[test]
fn test_nested() {
    assert_eq!(parse(&vec![Token::OpenParen, TIdentifier("+".to_string()), Token::OpenParen, TIdentifier("+".to_string()), TInteger(1), Token::OpenParen, TIdentifier("+".to_string()), TInteger(3), TInteger(4), TCloseParen, TCloseParen, TInteger(5), TCloseParen]).unwrap(),
               vec![Node::List(vec![Node::Identifier("+".to_string()), Node::List(vec![Node::Identifier("+".to_string()), Node::Integer(1), Node::List(vec![Node::Identifier("+".to_string()), Node::Integer(3), Node::Integer(4)])]), Node::Integer(5)])]);
}

#[test]
fn test_quoting() {
    assert_eq!(parse(&vec![TQuote, Token::OpenParen, TIdentifier("a".to_string()), TCloseParen]).unwrap(),
               vec![Node::List(vec![Node::Identifier("quote".to_string()), Node::List(vec![Node::Identifier("a".to_string())])])]);
    assert_eq!(parse(&vec![Token::OpenParen, TIdentifier("list".to_string()), TQuote, TIdentifier("a".to_string()), TIdentifier("b".to_string()), TCloseParen]).unwrap(),
               vec![Node::List(vec![Node::Identifier("list".to_string()), Node::List(vec![Node::Identifier("quote".to_string()), Node::Identifier("a".to_string())]), Node::Identifier("b".to_string())])]);
}

#[test]
fn test_quasiquoting() {
    assert_eq!(parse(&vec![TQuasiquote, Token::OpenParen, TUnquote, TIdentifier("a".to_string()), TCloseParen]).unwrap(),
               vec![Node::List(vec![Node::Identifier("quasiquote".to_string()), Node::List(vec![Node::List(vec![Node::Identifier("unquote".to_string()), Node::Identifier("a".to_string())])])])]);
    assert_eq!(parse(&vec![TQuasiquote, Token::OpenParen, TUnquote, TIdentifier("a".to_string()), TIdentifier("b".to_string()), TUnquote, TIdentifier("c".to_string()), TCloseParen]).unwrap(),
               vec![Node::List(vec![Node::Identifier("quasiquote".to_string()), Node::List(vec![Node::List(vec![Node::Identifier("unquote".to_string()), Node::Identifier("a".to_string())]), Node::Identifier("b".to_string()), Node::List(vec![Node::Identifier("unquote".to_string()), Node::Identifier("c".to_string())])])])]);
}

#[test]
fn test_bad_syntax() {
    assert_eq!(parse(&vec![TCloseParen]).err().unwrap().to_string().as_slice(),
               "ParseError: Unexpected close paren, depth: 0");
    assert_eq!(parse(&vec![Token::OpenParen, Token::OpenParen, TCloseParen]).err().unwrap().to_string().as_slice(),
               "ParseError: Unexpected end of input, depth: 1");
    assert_eq!(parse(&vec![Token::OpenParen, TCloseParen, TCloseParen]).err().unwrap().to_string().as_slice(),
               "ParseError: Unexpected close paren, depth: 0");
    assert_eq!(parse(&vec![Token::OpenParen, Token::OpenParen, TCloseParen, Token::OpenParen, Token::OpenParen, TCloseParen]).err().unwrap().to_string().as_slice(),
               "ParseError: Unexpected end of input, depth: 2");
}
