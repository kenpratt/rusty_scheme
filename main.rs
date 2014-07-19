#![feature(macro_rules)]

mod lexer;
mod parser;
mod interpreter;

macro_rules! try_or_err_to_str(
    ($inp:expr) => (
        match $inp {
            Ok(v) => v,
            Err(e) => return Err(e.to_str())
        }
    )
)

fn main() {
    run("(+ 2 3)");
}

fn run(input: &str) {
    println!("input: \"{}\"", input);
    println!("result: \"{}\"", execute(input));
}

fn execute(input: &str) -> Result<String, String> {
    let tokens = try_or_err_to_str!(lexer::tokenize(input));
    let ast = try_or_err_to_str!(parser::parse(&tokens));
    let result = try_or_err_to_str!(interpreter::interpret(&ast));
    Ok(format!("{}", result))
}

macro_rules! assert_execute(
    ($src:expr, $res:expr) => (
        assert_eq!(execute($src).unwrap().as_slice(), $res)
    )
)
macro_rules! assert_execute_fail(
    ($src:expr, $res:expr) => (
        assert_eq!(execute($src).err().unwrap().as_slice(), $res)
    )
)

#[test]
fn test_basic_identities() {
    assert_execute!("1", "1");
    assert_execute!("#f", "#f");
    assert_execute!("\"hi\"", "\"hi\"");
    assert_execute!("(lambda (x) x)", "#<procedure>");
}

#[test]
fn test_simple_function() {
    assert_execute!("(+ 2 3)", "5");
}

#[test]
fn test_multiple_expression_return() {
    assert_execute!("(+ 2 3)\n(+ 1 2)", "3");
}

#[test]
fn test_nested_expressions() {
    assert_execute!("(+ 2 (- (+ 9 1) 4))", "8");
}

#[test]
fn test_variable_definition() {
    assert_execute!("(define x 2) (+ x x x)", "6");
}

#[test]
fn test_duplicate_variable_definition() {
    assert_execute_fail!("(define x 2) (define x 3)", "RuntimeError: Duplicate define: x");
}

#[test]
fn test_variable_modification() {
    assert_execute!("(define x 2) (set! x 3) (+ x x x)", "9");
}

#[test]
fn test_unknown_variable_modification() {
    assert_execute_fail!("(set! x 3)", "RuntimeError: Can't set! an undefined variable: x");
}

#[test]
fn test_procedure_definition() {
    assert_execute!("(define double (lambda (x) (+ x x))) (double 8)", "16");
}

#[test]
fn test_bad_syntax() {
    assert_execute_fail!("(22+)", "SyntaxError: Unexpected character when looking for a delimiter: + (line: 1, column: 4)");
    assert_execute_fail!("(+ 2 3)\n(+ 1 2-)", "SyntaxError: Unexpected character when looking for a delimiter: - (line: 2, column: 7)");
}
