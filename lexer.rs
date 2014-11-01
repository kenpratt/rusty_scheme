use std::str;
use std::fmt;
use std::iter;
use std::from_str;

pub fn tokenize(s: &str) -> Result<Vec<Token>, SyntaxError> {
    Lexer::tokenize(s)
}

#[deriving(Show, PartialEq)]
pub enum Token {
    TOpenParen,
    TCloseParen,
    TQuote,
    TQuasiquote,
    TUnquote,
    TIdentifier(String),
    TInteger(int),
    TBoolean(bool),
    TString(String),
}

pub struct SyntaxError {
    message: String,
    line: uint,
    column: uint,
}

impl fmt::Show for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "SyntaxError: {} (line: {}, column: {})", self.message, self.line, self.column)
    }
}

macro_rules! syntax_error(
    ($lexer:ident, $($arg:tt)*) => (
        return Err(SyntaxError { message: format!($($arg)*), line: $lexer.line, column: $lexer.column })
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
    fn tokenize(s: &str) -> Result<Vec<Token>, SyntaxError> {
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

    fn run(&mut self) -> Result<(), SyntaxError> {
        self.advance();
        loop {
            match self.current() {
                Some(c) => {
                    match c {
                        _ if c.is_whitespace() => {
                            self.advance();
                        },
                        '(' => {
                            self.tokens.push(TOpenParen);
                            self.advance();
                        },
                        ')' => {
                            self.tokens.push(TCloseParen);
                            self.advance();
                        },
                        '\'' => {
                            self.tokens.push(TQuote);
                            self.advance();
                        },
                        '`' => {
                            self.tokens.push(TQuasiquote);
                            self.advance();
                        },
                        ',' => {
                            self.tokens.push(TUnquote);
                            self.advance();
                        },
                        '+' | '-' => {
                            match self.peek() {
                                Some('0'...'9') => {
                                    // skip past the +/- symbol and parse the number
                                    self.advance();
                                    let val = try!(self.parse_number());
                                    self.tokens.push(TInteger(if c == '-' { -1 * val } else { val }));
                                    try!(self.parse_delimiter());
                                },
                                _ => {
                                    // not followed by a digit, must be an identifier
                                    self.tokens.push(TIdentifier(c.to_string()));
                                    self.advance();
                                    try!(self.parse_delimiter());
                                }
                            }
                        },
                        '#' => {
                            let val = try!(self.parse_boolean());
                            self.tokens.push(TBoolean(val));
                            try!(self.parse_delimiter());
                        },
                        '0'...'9' => {
                            // don't advance -- let parse_number advance as needed
                            let val = try!(self.parse_number());
                            self.tokens.push(TInteger(val));
                            try!(self.parse_delimiter());
                        },
                        '\"' => {
                            let val = try!(self.parse_string());
                            self.tokens.push(TString(val));
                            try!(self.parse_delimiter());
                        },
                        '[' | ']' | '{' | '}' | ';' | '|' | '\\' => {
                            syntax_error!(self, "Unexpected character: {}", c);
                        },
                        _ => {
                            let val = try!(self.parse_identifier());
                            self.tokens.push(TIdentifier(val));
                            try!(self.parse_delimiter());
                        }
                    }
                },
                None => break
            }
        };
        Ok(())
    }

    fn parse_number(&mut self) -> Result<int, SyntaxError> {
        let mut s = String::new();
        loop {
            match self.current() {
                Some(c) => {
                    match c {
                        '0'...'9' => {
                            s.push(c);
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

    fn parse_boolean(&mut self) -> Result<bool, SyntaxError> {
        if self.current() != Some('#') { syntax_error!(self, "Unexpected character: {}", self.current()) };
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
                syntax_error!(self, "Unexpected character when looking for t/f: {}", self.current())
            }
        }
    }

    fn parse_identifier(&mut self) -> Result<String, SyntaxError> {
        let mut s = String::new();
        loop {
            match self.current() {
                Some(c) => {
                    match c {
                        _ if c.is_whitespace() => {
                            break;
                        },
                        '(' | ')' | '[' | ']' | '{' | '}' | '\"' | ',' | '\'' | '`' | ';' | '|' | '\\' => {
                            break;
                        },
                        _ => {
                            s.push(c);
                            self.advance();
                        },
                    }
                },
                None => break
            }
        }
        Ok(s)
    }

    fn parse_string(&mut self) -> Result<String, SyntaxError> {
        if self.current() != Some('\"') { syntax_error!(self, "Unexpected character: {}", self.current()) };
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
                            s.push(c);
                            self.advance();
                        }
                    }
                },
                None => syntax_error!(self, "Expected end quote, but found EOF instead")
            }
        }
        Ok(s)
    }

    fn parse_delimiter(&mut self) -> Result<(), SyntaxError> {
        match self.current() {
            Some(c) => {
                match c {
                    _ if c.is_whitespace() => (),
                    ')' => {
                        self.tokens.push(TCloseParen);
                        self.advance();
                    },
                    _ => syntax_error!(self, "Unexpected character when looking for a delimiter: {}", c),
                }
            },
            None => ()
        };
        Ok(())
    }
}

#[test]
fn test_simple_lexing() {
    assert_eq!(tokenize("(+ 2 3)").unwrap(),
               vec![TOpenParen, TIdentifier("+".to_string()), TInteger(2), TInteger(3), TCloseParen]);
}

#[test]
fn test_multi_digit_integers() {
    assert_eq!(tokenize("(+ 21 325)").unwrap(),
               vec![TOpenParen, TIdentifier("+".to_string()), TInteger(21), TInteger(325), TCloseParen]);
}

#[test]
fn test_subtraction() {
    assert_eq!(tokenize("(- 7 42)").unwrap(),
               vec![TOpenParen, TIdentifier("-".to_string()), TInteger(7), TInteger(42), TCloseParen]);
}

#[test]
fn test_negative_integers() {
    assert_eq!(tokenize("(+ -8 +2 -33)").unwrap(),
               vec![TOpenParen, TIdentifier("+".to_string()), TInteger(-8), TInteger(2), TInteger(-33), TCloseParen]);
}

#[test]
fn test_booleans() {
    assert_eq!(tokenize("#t").unwrap(),
               vec![TBoolean(true)]);
    assert_eq!(tokenize("#f").unwrap(),
               vec![TBoolean(false)]);
}

#[test]
fn test_identifiers() {
    for identifier in ["*", "<", "<=", "if", "while", "$t$%*=:t059s"].iter() {
        assert_eq!(tokenize(*identifier).unwrap(),
                   vec![TIdentifier(identifier.to_string())]);
    }
}

#[test]
fn test_strings() {
    assert_eq!(tokenize("\"hello\"").unwrap(),
               vec![TString("hello".to_string())]);
    assert_eq!(tokenize("\"a _ $ snthoeau(*&G#$()*^!\"").unwrap(),
               vec![TString("a _ $ snthoeau(*&G#$()*^!".to_string())]);
    assert_eq!(tokenize("\"truncated").err().unwrap().to_string().as_slice(),
               "SyntaxError: Expected end quote, but found EOF instead (line: 1, column: 11)");
}

#[test]
fn test_whitespace() {
    assert_eq!(tokenize("(+ 1 1)\n(+\n    2\t2 \n )\r\n  \n").unwrap(),
               vec![TOpenParen, TIdentifier("+".to_string()), TInteger(1), TInteger(1), TCloseParen,
                    TOpenParen, TIdentifier("+".to_string()), TInteger(2), TInteger(2), TCloseParen]);
}

#[test]
fn test_bad_syntax() {
    assert_eq!(tokenize("([)").err().unwrap().to_string().as_slice(),
               "SyntaxError: Unexpected character: [ (line: 1, column: 2)");
}

#[test]
fn test_delimiter_checking() {
    assert_eq!(tokenize("(+-)").err().unwrap().to_string().as_slice(),
               "SyntaxError: Unexpected character when looking for a delimiter: - (line: 1, column: 3)");

    assert_eq!(tokenize("(-22+)").err().unwrap().to_string().as_slice(),
               "SyntaxError: Unexpected character when looking for a delimiter: + (line: 1, column: 5)");

    assert_eq!(tokenize("(22+)").err().unwrap().to_string().as_slice(),
               "SyntaxError: Unexpected character when looking for a delimiter: + (line: 1, column: 4)");

    assert_eq!(tokenize("(+ 2 3)\n(+ 1 2-)").err().unwrap().to_string().as_slice(),
               "SyntaxError: Unexpected character when looking for a delimiter: - (line: 2, column: 7)");
}

#[test]
fn test_quoting() {
    assert_eq!(tokenize("'(a)").unwrap(),
               vec![TQuote, TOpenParen, TIdentifier("a".to_string()), TCloseParen]);
    assert_eq!(tokenize("'('a 'b)").unwrap(),
               vec![TQuote, TOpenParen, TQuote, TIdentifier("a".to_string()), TQuote, TIdentifier("b".to_string()), TCloseParen]);
    assert_eq!(tokenize("(list 'a b)").unwrap(),
               vec![TOpenParen, TIdentifier("list".to_string()), TQuote, TIdentifier("a".to_string()), TIdentifier("b".to_string()), TCloseParen]);
}

#[test]
fn test_quasiquoting() {
    assert_eq!(tokenize("`(,a)").unwrap(),
               vec![TQuasiquote, TOpenParen, TUnquote, TIdentifier("a".to_string()), TCloseParen]);
    assert_eq!(tokenize("`(,a b ,c)").unwrap(),
               vec![TQuasiquote, TOpenParen, TUnquote, TIdentifier("a".to_string()), TIdentifier("b".to_string()), TUnquote, TIdentifier("c".to_string()), TCloseParen]);
}

#[test]
fn test_complex_code_block() {
    assert_eq!(tokenize("(define (list-of-squares n)\n  (let loop ((i n) (res (list)))\n    (if (< i 0)\n        res\n        (loop (- i 1) (cons (* i i) res)))))").unwrap(),
               vec![TOpenParen, TIdentifier("define".to_string()), TOpenParen, TIdentifier("list-of-squares".to_string()), TIdentifier("n".to_string()), TCloseParen, TOpenParen, TIdentifier("let".to_string()), TIdentifier("loop".to_string()), TOpenParen, TOpenParen, TIdentifier("i".to_string()), TIdentifier("n".to_string()), TCloseParen, TOpenParen, TIdentifier("res".to_string()), TOpenParen, TIdentifier("list".to_string()), TCloseParen, TCloseParen, TCloseParen, TOpenParen, TIdentifier("if".to_string()), TOpenParen, TIdentifier("<".to_string()), TIdentifier("i".to_string()), TInteger(0), TCloseParen, TIdentifier("res".to_string()), TOpenParen, TIdentifier("loop".to_string()), TOpenParen, TIdentifier("-".to_string()), TIdentifier("i".to_string()), TInteger(1), TCloseParen, TOpenParen, TIdentifier("cons".to_string()), TOpenParen, TIdentifier("*".to_string()), TIdentifier("i".to_string()), TIdentifier("i".to_string()), TCloseParen, TIdentifier("res".to_string()), TCloseParen, TCloseParen, TCloseParen, TCloseParen, TCloseParen]);
}

#[test]
fn test_unicode_identifiers() {
    assert_eq!(tokenize("λ").unwrap(),
               vec![TIdentifier("λ".to_string())]);
    assert_eq!(tokenize("★☎♫✂").unwrap(),
               vec![TIdentifier("★☎♫✂".to_string())]);
    assert_eq!(tokenize("日本国").unwrap(),
               vec![TIdentifier("日本国".to_string())]);
}
