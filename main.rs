#![feature(macro_rules)]

use std::str;
use std::fmt;
use std::iter;
use std::from_str;

fn main() {
    run("(+ 2 3)");
    run("(22+)");
}

fn run(s: &str) {
    println!("str: \"{}\"", s);
    let tokens = Lexer::tokenize(s);
    println!("tokens: {}", tokens);
}

#[deriving(Show, PartialEq)]
enum Token {
    OpenParen,
    CloseParen,
    Identifier(String),
    Integer(int),
}

struct ParseError {
    message: String,
}

impl fmt::Show for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ParseError: {}", self.message)
    }
}

macro_rules! parse_error(
    ($($arg:tt)*) => (
        return Err(ParseError { message: format!($($arg)*) })
    )
)

struct Lexer<'a> {
    chars: iter::Peekable<char, str::Chars<'a>>,
    current: Option<char>,
    tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    fn tokenize(s: &str) -> Result<Vec<Token>, ParseError> {
        let mut lexer = Lexer { chars: s.chars().peekable(), current: None, tokens: Vec::new() };
        try!(lexer.run());
        Ok(lexer.tokens)
    }

    fn current(&self) -> Option<char> {
        self.current
    }

    fn advance(&mut self) {
        self.current = self.chars.next()
    }

    fn peek(&mut self) -> Option<char> {
        match self.chars.peek() {
            Some(c) => Some(*c),
            None => None
        }
    }

    fn run(&mut self) -> Result<(), ParseError> {
        self.advance();
        loop {
            match self.current() {
                Some(c) => {
                    match c {
                        '(' => {
                            self.tokens.push(OpenParen);
                            self.advance();
                        },
                        ')' => {
                            self.tokens.push(CloseParen);
                            self.advance();
                        },
                        '+' | '-' => {
                            match self.peek() {
                                Some('0'..'9') => {
                                    // skip past the +/- symbol and parse the number
                                    self.advance();
                                    let val = try!(self.parse_number());
                                    self.tokens.push(Integer(if c == '-' { -1 * val } else { val }));
                                    try!(self.parse_delimiter());
                                },
                                _ => {
                                    // not followed by a digit, must be an identifier
                                    self.tokens.push(Identifier(str::from_char(c)));
                                    self.advance();
                                    try!(self.parse_delimiter());
                                }
                            }
                        },
                        '0'..'9' => {
                            // don't advance -- let parse_number advance as needed
                            let val = try!(self.parse_number());
                            self.tokens.push(Integer(val));
                            try!(self.parse_delimiter());
                        },
                        ' ' => self.advance(),
                        _   => parse_error!("Unexpected character: {}", c),
                    }
                },
                None => break
            }
        };
        Ok(())
    }

    fn parse_number(&mut self) -> Result<int, ParseError> {
        let mut s = String::new();
        loop {
            match self.current() {
                Some(c) => {
                    match c {
                        '0'..'9' => {
                            s.push_char(c);
                            self.advance();
                        },
                        _ => break
                    }
                },
                None => break
            }
        }
        Ok(from_str::from_str(s.as_slice()).unwrap())
    }

    fn parse_delimiter(&mut self) -> Result<(), ParseError> {
        match self.current() {
            Some(c) => {
                match c {
                    ')' => {
                        self.tokens.push(CloseParen);
                        self.advance();
                    },
                    ' ' => (),
                    _ => parse_error!("Unexpected character when looking for a delimiter: {}", c),
                }
            },
            None => ()
        };
        Ok(())
    }
}

#[test]
fn test_simple_lexing() {
    assert_eq!(Lexer::tokenize("(+ 2 3)").unwrap(),
               vec![OpenParen, Identifier("+".to_str()), Integer(2), Integer(3), CloseParen]);
}

#[test]
fn test_multi_digit_integers() {
    assert_eq!(Lexer::tokenize("(+ 21 325)").unwrap(),
               vec![OpenParen, Identifier("+".to_str()), Integer(21), Integer(325), CloseParen]);
}

#[test]
fn test_subtraction() {
    assert_eq!(Lexer::tokenize("(- 7 42)").unwrap(),
               vec![OpenParen, Identifier("-".to_str()), Integer(7), Integer(42), CloseParen]);
}

#[test]
fn test_negative_integers() {
    assert_eq!(Lexer::tokenize("(+ -8 +2 -33)").unwrap(),
               vec![OpenParen, Identifier("+".to_str()), Integer(-8), Integer(2), Integer(-33), CloseParen]);
}

#[test]
fn test_bad_syntax() {
    assert!(Lexer::tokenize("(&&)").is_err())
}

#[test]
fn test_delimiter_checking() {
    assert!(Lexer::tokenize("(+-)").is_err())
    assert!(Lexer::tokenize("(-22+)").is_err())
    assert!(Lexer::tokenize("(22+)").is_err())
}
