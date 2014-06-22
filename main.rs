fn main() {
    run("(+ 2 3)");
}

fn run(s: &str) {
    println!("str: \"{}\"", s);
    let tokens = tokenize(s);
    println!("tokens: {}", tokens);
}

enum Token {
    OpenParen,
    CloseParen,
    Identifier(String),
    Integer(uint),
}

impl std::fmt::Show for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            OpenParen => write!(f, "OpenParen"),
            CloseParen => write!(f, "CloseParen"),
            Identifier(ref v) => write!(f, "Identifier({})", v),
            Integer(ref v) => write!(f, "Integer({})", v),
        }
    }
}

fn tokenize(s: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    for c in s.chars() {
        match c {
            '(' => tokens.push(OpenParen),
            ')' => tokens.push(CloseParen),
            '+' => tokens.push(Identifier(String::from_char(1, c))),
            '0'..'9' => tokens.push(Integer(std::char::to_digit(c, 10).unwrap())),
            ' ' => (),
            _   => println!("other"),
        };
    }
    tokens
}
