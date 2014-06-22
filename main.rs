fn main() {
    run("(+ 2 3)");
    run("(+ 21 325)");
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
    Integer(int),
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
    let mut iter = s.chars();
    loop {
        match iter.next() {
            Some(c) =>
                match c {
                    '(' => tokens.push(OpenParen),
                    ')' => tokens.push(CloseParen),
                    '+' => tokens.push(Identifier(String::from_char(1, c))),
                    '0'..'9' => {
                        let extra_chars: Vec<char> = iter.by_ref().take_while(|cc| cc.is_digit()).collect();
                        let chars = vec![c].append(extra_chars.as_slice());
                        let val = std::from_str::from_str(std::str::from_chars(chars.as_slice()).as_slice()).unwrap();
                        tokens.push(Integer(val))
                    },
                    ' ' => (),
                    _   => println!("other"),
                },
            None =>
                return tokens
        }
    };
}
