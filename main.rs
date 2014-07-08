#![feature(macro_rules)]

mod lexer;

fn main() {
    run("(+ 2 3)");
    run("(22+)");
    run("(+ 2 3)\n(+ 1 2-)");
}

fn run(s: &str) {
    println!("str: \"{}\"", s);
    let tokens = lexer::tokenize(s);
    println!("tokens: {}", tokens);
}
