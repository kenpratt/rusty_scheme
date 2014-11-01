use lexer::*;

use std::fmt;
use std::slice;

pub fn parse(tokens: &Vec<Token>) -> Result<Vec<Node>, ParseError> {
    Parser::parse(tokens)
}

#[deriving(Show, PartialEq, Clone)]
pub enum Node {
    NIdentifier(String),
    NInteger(int),
    NBoolean(bool),
    NString(String),
    NList(Vec<Node>),
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
                    TOpenParen => {
                        let inner = try!(self.parse_nodes(depth + 1));
                        Ok(Some(NList(inner)))
                    },
                    TCloseParen => {
                        if depth > 0 {
                            Ok(None)
                        } else {
                            parse_error!("Unexpected close paren, depth: {}", depth)
                        }
                    },
                    TQuote => {
                        match try!(self.parse_node(depth)) {
                            Some(inner) => {
                                let quoted = NList(vec![NIdentifier("quote".to_string()), inner]);
                                Ok(Some(quoted))
                            },
                            None => parse_error!("Missing quoted value, depth: {}", depth)
                        }
                    },
                    TQuasiquote => {
                        match try!(self.parse_node(depth)) {
                            Some(inner) => {
                                let quoted = NList(vec![NIdentifier("quasiquote".to_string()), inner]);
                                Ok(Some(quoted))
                            },
                            None => parse_error!("Missing quasiquoted value, depth: {}", depth)
                        }
                    }
                    TUnquote => {
                        match try!(self.parse_node(depth)) {
                            Some(inner) => {
                                let quoted = NList(vec![NIdentifier("unquote".to_string()), inner]);
                                Ok(Some(quoted))
                            },
                            None => parse_error!("Missing unquoted value, depth: {}", depth)
                        }
                    }
                    TIdentifier(ref val) => {
                        Ok(Some(NIdentifier(val.clone())))
                    },
                    TInteger(ref val) => {
                        Ok(Some(NInteger(val.clone())))
                    },
                    TBoolean(ref val) => {
                        Ok(Some(NBoolean(val.clone())))
                    },
                    TString(ref val) => {
                        Ok(Some(NString(val.clone())))
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
    assert_eq!(parse(&vec![TOpenParen, TIdentifier("+".to_string()), TCloseParen]).unwrap(),
               vec![NList(vec![NIdentifier("+".to_string())])]);
}

#[test]
fn test_nested() {
    assert_eq!(parse(&vec![TOpenParen, TIdentifier("+".to_string()), TOpenParen, TIdentifier("+".to_string()), TInteger(1), TOpenParen, TIdentifier("+".to_string()), TInteger(3), TInteger(4), TCloseParen, TCloseParen, TInteger(5), TCloseParen]).unwrap(),
               vec![NList(vec![NIdentifier("+".to_string()), NList(vec![NIdentifier("+".to_string()), NInteger(1), NList(vec![NIdentifier("+".to_string()), NInteger(3), NInteger(4)])]), NInteger(5)])]);
}

#[test]
fn test_quoting() {
    assert_eq!(parse(&vec![TQuote, TOpenParen, TIdentifier("a".to_string()), TCloseParen]).unwrap(),
               vec![NList(vec![NIdentifier("quote".to_string()), NList(vec![NIdentifier("a".to_string())])])]);
    assert_eq!(parse(&vec![TOpenParen, TIdentifier("list".to_string()), TQuote, TIdentifier("a".to_string()), TIdentifier("b".to_string()), TCloseParen]).unwrap(),
               vec![NList(vec![NIdentifier("list".to_string()), NList(vec![NIdentifier("quote".to_string()), NIdentifier("a".to_string())]), NIdentifier("b".to_string())])]);
}

#[test]
fn test_quasiquoting() {
    assert_eq!(parse(&vec![TQuasiquote, TOpenParen, TUnquote, TIdentifier("a".to_string()), TCloseParen]).unwrap(),
               vec![NList(vec![NIdentifier("quasiquote".to_string()), NList(vec![NList(vec![NIdentifier("unquote".to_string()), NIdentifier("a".to_string())])])])]);
    assert_eq!(parse(&vec![TQuasiquote, TOpenParen, TUnquote, TIdentifier("a".to_string()), TIdentifier("b".to_string()), TUnquote, TIdentifier("c".to_string()), TCloseParen]).unwrap(),
               vec![NList(vec![NIdentifier("quasiquote".to_string()), NList(vec![NList(vec![NIdentifier("unquote".to_string()), NIdentifier("a".to_string())]), NIdentifier("b".to_string()), NList(vec![NIdentifier("unquote".to_string()), NIdentifier("c".to_string())])])])]);
}

#[test]
fn test_bad_syntax() {
    assert_eq!(parse(&vec![TCloseParen]).err().unwrap().to_string().as_slice(),
               "ParseError: Unexpected close paren, depth: 0");
    assert_eq!(parse(&vec![TOpenParen, TOpenParen, TCloseParen]).err().unwrap().to_string().as_slice(),
               "ParseError: Unexpected end of input, depth: 1");
    assert_eq!(parse(&vec![TOpenParen, TCloseParen, TCloseParen]).err().unwrap().to_string().as_slice(),
               "ParseError: Unexpected close paren, depth: 0");
    assert_eq!(parse(&vec![TOpenParen, TOpenParen, TCloseParen, TOpenParen, TOpenParen, TCloseParen]).err().unwrap().to_string().as_slice(),
               "ParseError: Unexpected end of input, depth: 2");
}
