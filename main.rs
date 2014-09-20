#![feature(macro_rules)]
#![feature(globs)]

use std::os;
use std::io::File;
use std::path::posix::Path;

mod lexer;
mod parser;
mod interpreter;

#[cfg(not(test))]
mod repl;

macro_rules! try_or_err_to_string(
    ($inp:expr) => (
        match $inp {
            Ok(v) => v,
            Err(e) => return Err(e.to_string())
        }
    )
)

#[cfg(not(test))]
fn main() {
    let raw_args = os::args();
    let args = raw_args.slice_from(1);
    match args.len() {
        0 => start_repl(),
        1 => run_file(args.get(0).unwrap()),
        _ => fail!("You must provide 0 or 1 arguments to RustyScheme: {}", args)
    }
}

#[allow(unused_must_use)]
#[cfg(not(test))]
fn run_file(filename: &String) {
    let path = Path::new(filename.as_slice());
    let mut file = File::open(&path).unwrap();
    let contents = file.read_to_string().unwrap();
    let ctx = interpreter::new();
    execute(contents.as_slice(), ctx);
}

#[cfg(not(test))]
fn start_repl() {
    let ctx = interpreter::new();
    println!("\nWelcome to the RustyScheme REPL!");
    repl::start("> ", (|s| execute(s.as_slice(), ctx.clone())));
}

fn execute(input: &str, ctx: interpreter::Interpreter) -> Result<String, String> {
    let tokens = try_or_err_to_string!(lexer::tokenize(input));
    let ast = try_or_err_to_string!(parser::parse(&tokens));
    let result = try_or_err_to_string!(ctx.run(ast.as_slice()));
    Ok(format!("{}", result))
}

macro_rules! assert_execute(
    ($src:expr, $res:expr) => (
        assert_eq!(execute($src, interpreter::new()).unwrap().as_slice(), $res)
    )
)
macro_rules! assert_execute_fail(
    ($src:expr, $res:expr) => (
        assert_eq!(execute($src, interpreter::new()).err().unwrap().as_slice(), $res)
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
fn test_list_creation() {
    assert_execute!("(list)", "'()");
    assert_execute!("(list 1 2 3)", "'(1 2 3)");
    assert_execute!("(list 1 (list 2 3) (list 4) (list))", "'(1 (2 3) (4) ())");
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
    assert_execute!("(define twice (lambda (f v) (f (f v)))) (twice (lambda (x) (+ x x)) 8)", "32");
    assert_execute!("(define twice (λ (f v) (f (f v)))) (twice (λ (x) (+ x x)) 8)", "32");
    assert_execute!("((λ (x) (+ x x)) 8)", "16");
    assert_execute!("(define foo (λ (x) (λ (y) (+ x y)))) (define add2 (foo 2)) (add2 5)", "7");
    assert_execute!("(define foo (λ (x) (λ (y) (+ x y)))) (define add2 (foo 2)) ((λ (x) (add2 (+ x 1))) 1)", "4");
    assert_execute!("(define (twice f v) (f (f v))) (twice (lambda (x) (+ x x)) 8)", "32");
}

#[test]
fn test_conditional_execution() {
    assert_execute!("(if #t 1 2)", "1");
    assert_execute!("(if #f 1 2)", "2");
    assert_execute!("(if 0 1 2)", "1");
    assert_execute!("(if \"\" 1 2)", "1");
}

#[test]
fn test_conditional_execution_doesnt_run_other_case() {
    assert_execute!("(if #t 1 (error \"bad\"))", "1");
    assert_execute!("(if #f (error \"bad\") 2)", "2");
}

#[test]
fn test_boolean_operators() {
    assert_execute!("(and)", "#t");
    assert_execute!("(and #t)", "#t");
    assert_execute!("(and 1)", "1");
    assert_execute!("(and 1 2 3)", "3");
    assert_execute!("(and 1 #f 3)", "#f");
    assert_execute!("(and 1 #f (error \"bad\"))", "#f");
    assert_execute!("(or)", "#f");
    assert_execute!("(or #f)", "#f");
    assert_execute!("(or 1)", "1");
    assert_execute!("(or 1 2)", "1");
    assert_execute!("(or 1 #f)", "1");
    assert_execute!("(or #f 3)", "3");
    assert_execute!("(or #f #f)", "#f");
    assert_execute!("(or 1 (error \"bad\"))", "1");
}

#[test]
fn test_quoting() {
    assert_execute!("(quote #t)", "#t");
    assert_execute!("(quote 1)", "1");
    assert_execute!("(quote sym)", "'sym");
    assert_execute!("(quote \"hi\")", "\"hi\"");
    assert_execute!("(quote (1 2))", "'(1 2)");
    assert_execute!("(quote (a b))", "'(a b)");
    assert_execute!("(quote (a b (c (d) e ())))", "'(a b (c (d) e ()))");
    assert_execute!("(quote (a (quote b)))", "'(a (quote b))");
    assert_execute!("'(1 2)", "'(1 2)");
    assert_execute!("'(a b (c (d) e ()))", "'(a b (c (d) e ()))");
    assert_execute!("'(1 '2)", "'(1 (quote 2))");
}

#[test]
fn test_quasiquoting() {
    assert_execute!("(quasiquote (1 2))", "'(1 2)");
    assert_execute!("(quasiquote (2 (unquote (+ 1 2)) 4))", "'(2 3 4)");
    assert_execute!("`(2 ,(+ 1 2) 4)", "'(2 3 4)");
}

#[test]
fn test_apply() {
    assert_execute!("(apply + '(1 2 3))", "6");
    assert_execute!("(define foo (lambda (f) (lambda (x y) (f (f x y) y)))) (apply (apply foo '(+)) '(5 3))", "11");
}

#[test]
fn test_eval() {
    assert_execute!("(eval '(+ 1 2 3))", "6");
    assert_execute!("(define eval-formula (lambda (formula) (eval `((lambda (x y) ,formula) 2 3)))) (eval-formula '(+ (- y x) y))", "4");
    assert_execute_fail!("(define bad-eval-formula (lambda (formula) ((lambda (x y) (eval formula)) 2 3))) (bad-eval-formula '(+ x y))", "RuntimeError: Identifier not found: 'x");
}

#[test]
fn test_bad_syntax() {
    assert_execute_fail!("(22+)", "SyntaxError: Unexpected character when looking for a delimiter: + (line: 1, column: 4)");
    assert_execute_fail!("(+ 2 3)\n(+ 1 2-)", "SyntaxError: Unexpected character when looking for a delimiter: - (line: 2, column: 7)");
}

#[test]
fn test_generated_runtime_error() {
    assert_execute_fail!("(error \"fail, please\")", "RuntimeError: \"fail, please\"");
    assert_execute_fail!("(error (+ 2 3))", "RuntimeError: 5");
}

#[test]
fn test_unicode_identifiers() {
    assert_execute!("(define ★ 3) (define ♫ 4) (+ ★ ♫)", "7");
}
