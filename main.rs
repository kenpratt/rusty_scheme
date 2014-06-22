#![feature(macro_rules)]

use std::str;
use std::fmt;
use std::iter;
use std::from_str;

fn main() {
    run("(+ 2 3)");
    run("(22+)");
    run("(+ 2 3)\n(+ 1 2-)");
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
    Boolean(bool),
    String(String),
}

struct ParseError {
    message: String,
    line: uint,
    column: uint,
}

impl fmt::Show for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ParseError: {} (line: {}, column: {})", self.message, self.line, self.column)
    }
}

macro_rules! parse_error(
    ($($arg:tt)*) => (
        return Err(ParseError { message: format!($($arg)*), line: self.line, column: self.column })
    )
)

struct Lexer<'a> {
    chars: iter::Peekable<char, str::Chars<'a>>,
    current: Option<char>,
    tokens: Vec<Token>,
    line: uint,
    column: uint,
}

impl<'a> Lexer<'a> {
    fn tokenize(s: &str) -> Result<Vec<Token>, ParseError> {
        let mut lexer = Lexer { chars: s.chars().peekable(), current: None, tokens: Vec::new(), line: 1, column: 0 };
        try!(lexer.run());
        Ok(lexer.tokens)
    }

    fn current(&self) -> Option<char> {
        self.current
    }

    fn advance(&mut self) {
        if self.current() == Some('\x0a') {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        self.current = self.chars.next();
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
                        '#' => {
                            let val = try!(self.parse_boolean());
                            self.tokens.push(Boolean(val));
                            try!(self.parse_delimiter());
                        },
                        'A'..'Z' | 'a'..'z' | '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '_' | '^' => {
                            let val = try!(self.parse_identifier());
                            self.tokens.push(Identifier(val));
                            try!(self.parse_delimiter());
                        },
                        '0'..'9' => {
                            // don't advance -- let parse_number advance as needed
                            let val = try!(self.parse_number());
                            self.tokens.push(Integer(val));
                            try!(self.parse_delimiter());
                        },
                        '\"' => {
                            let val = try!(self.parse_string());
                            self.tokens.push(String(val));
                            try!(self.parse_delimiter());
                        },
                        ' ' | '\x09' | '\x0a' | '\x0d' => self.advance(),
                        _  => parse_error!("Unexpected character: {}", c),
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

    fn parse_boolean(&mut self) -> Result<bool, ParseError> {
        if self.current() != Some('#') { parse_error!("Unexpected character: {}", self.current()) };
        self.advance();

        match self.current() {
            Some('t') => {
                self.advance();
                Ok(true)
            },
            Some('f') => {
                self.advance();
                Ok(false)
            },
            _ => {
                parse_error!("Unexpected character when looking for t/f: {}", self.current())
            }
        }
    }

    fn parse_identifier(&mut self) -> Result<String, ParseError> {
        let mut s = String::new();
        loop {
            match self.current() {
                Some(c) => {
                    match c {
                        'A'..'Z' | 'a'..'z' | '0'..'9' | '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '_' | '^' | '+' | '-' | '#' => {
                            s.push_char(c);
                            self.advance();
                        },
                        _ => break
                    }
                },
                None => break
            }
        }
        Ok(s)
    }

    fn parse_string(&mut self) -> Result<String, ParseError> {
        if self.current() != Some('\"') { parse_error!("Unexpected character: {}", self.current()) };
        self.advance();

        let mut s = String::new();
        loop {
            match self.current() {
                Some(c) => {
                    match c {
                        '\"' => {
                            self.advance();
                            break;
                        },
                        _ => {
                            s.push_char(c);
                            self.advance();
                        }
                    }
                },
                None => parse_error!("Expected end quote, but found EOF instead")
            }
        }
        Ok(s)
    }

    fn parse_delimiter(&mut self) -> Result<(), ParseError> {
        match self.current() {
            Some(c) => {
                match c {
                    ')' => {
                        self.tokens.push(CloseParen);
                        self.advance();
                    },
                    ' ' | '\x09'| '\x0a' | '\x0d' => (),
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
fn test_booleans() {
    assert_eq!(Lexer::tokenize("#t").unwrap(),
               vec![Boolean(true)]);
    assert_eq!(Lexer::tokenize("#f").unwrap(),
               vec![Boolean(false)]);
}

#[test]
fn test_identifiers() {
    for identifier in ["*", "<", "<=", "if", "while", "$t$%*=:t059s"].iter() {
        assert_eq!(Lexer::tokenize(*identifier).unwrap(),
                   vec![Identifier(identifier.to_str())]);
    }
}

#[test]
fn test_strings() {
    assert_eq!(Lexer::tokenize("\"hello\"").unwrap(),
               vec![String("hello".to_str())]);
    assert_eq!(Lexer::tokenize("\"a _ $ snthoeau(*&G#$()*^!\"").unwrap(),
               vec![String("a _ $ snthoeau(*&G#$()*^!".to_str())]);
    assert_eq!(Lexer::tokenize("\"truncated").err().unwrap().to_str().as_slice(),
               "ParseError: Expected end quote, but found EOF instead (line: 1, column: 11)");
}

#[test]
fn test_whitespace() {
    assert_eq!(Lexer::tokenize("(+ 1 1)\n(+\n    2\t2 \n )\r\n  \n").unwrap(),
               vec![OpenParen, Identifier("+".to_str()), Integer(1), Integer(1), CloseParen,
                    OpenParen, Identifier("+".to_str()), Integer(2), Integer(2), CloseParen]);
}

#[test]
fn test_bad_syntax() {
    assert_eq!(Lexer::tokenize("(\\)").err().unwrap().to_str().as_slice(),
               "ParseError: Unexpected character: \\ (line: 1, column: 2)");
}

#[test]
fn test_delimiter_checking() {
    assert_eq!(Lexer::tokenize("(+-)").err().unwrap().to_str().as_slice(),
               "ParseError: Unexpected character when looking for a delimiter: - (line: 1, column: 3)");

    assert_eq!(Lexer::tokenize("(-22+)").err().unwrap().to_str().as_slice(),
               "ParseError: Unexpected character when looking for a delimiter: + (line: 1, column: 5)");

    assert_eq!(Lexer::tokenize("(22+)").err().unwrap().to_str().as_slice(),
               "ParseError: Unexpected character when looking for a delimiter: + (line: 1, column: 4)");

    assert_eq!(Lexer::tokenize("(+ 2 3)\n(+ 1 2-)").err().unwrap().to_str().as_slice(),
               "ParseError: Unexpected character when looking for a delimiter: - (line: 2, column: 7)");
}

#[test]
fn test_complex_code_block() {
    assert_eq!(Lexer::tokenize("(define (list-of-squares n)\n  (let loop ((i n) (res (list)))\n    (if (< i 0)\n        res\n        (loop (- i 1) (cons (* i i) res)))))").unwrap(),
               vec![OpenParen, Identifier("define".to_str()), OpenParen, Identifier("list-of-squares".to_str()), Identifier("n".to_str()), CloseParen, OpenParen, Identifier("let".to_str()), Identifier("loop".to_str()), OpenParen, OpenParen, Identifier("i".to_str()), Identifier("n".to_str()), CloseParen, OpenParen, Identifier("res".to_str()), OpenParen, Identifier("list".to_str()), CloseParen, CloseParen, CloseParen, OpenParen, Identifier("if".to_str()), OpenParen, Identifier("<".to_str()), Identifier("i".to_str()), Integer(0), CloseParen, Identifier("res".to_str()), OpenParen, Identifier("loop".to_str()), OpenParen, Identifier("-".to_str()), Identifier("i".to_str()), Integer(1), CloseParen, OpenParen, Identifier("cons".to_str()), OpenParen, Identifier("*".to_str()), Identifier("i".to_str()), Identifier("i".to_str()), CloseParen, Identifier("res".to_str()), CloseParen, CloseParen, CloseParen, CloseParen, CloseParen]);
}
