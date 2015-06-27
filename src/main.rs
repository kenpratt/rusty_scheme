extern crate getopts;

#[cfg(not(test))]
use getopts::Options;

#[cfg(not(test))]
use std::env;

mod lexer;
mod parser;
mod interpreter;
mod ast_walk_interpreter;
mod cps_interpreter;
mod jvm_compiler;
mod classfile;
mod classfile_builder;

#[cfg(not(test))]
mod repl;

#[cfg(not(test))]
fn main() {
    // parse command-line arguments & options
    let args: Vec<String> = env::args().collect();
    let program = &args[0];
    let mut opts = Options::new();
    opts.optopt("t", "type", "set interpreter type", "ast_walk/cps");
    opts.optflag("h", "help", "print this help menu");
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => { m }
        Err(f) => { panic!(f.to_string()) }
    };

    if matches.opt_present("h") {
        print_usage(&program, opts);
        return;
    }

    let interpreter = match matches.opt_str("t") {
        Some(t) => interpreter::new(&t),
        None => interpreter::new("cps")
    };

    let rest = matches.free;
    match rest.len() {
        0 => interpreter.start_repl(),
        1 => interpreter.run_file(&rest[0]),
        _ => panic!("You must provide 0 or 1 arguments to RustyScheme: {:?}", rest)
    }
}

#[cfg(not(test))]
fn print_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} [options]", program);
    print!("{}", opts.usage(&brief));
}

macro_rules! test {
    ($name:ident, $src:expr, $res:expr) => (#[test] fn $name() { assert_execute_all!($name, $src, $res); });
    ($name:ident, $src:expr, $res:expr, cps) => (#[test] fn $name() { assert_execute_cps!($src, $res); });
}

macro_rules! test_fail {
    ($name:ident, $src:expr, $res:expr) => (#[test] fn $name() { assert_execute_fail_all!($name, $src, $res); });
    ($name:ident, $src:expr, $res:expr, cps) => (#[test] fn $name() { assert_execute_fail_cps!($src, $res); });
}

macro_rules! assert_execute_all {
    ($name:ident, $src:expr, $res:expr) => (
        assert_execute_ast_walk!($src, $res);
        assert_execute_cps!($src, $res);
        assert_execute_jvm!($name, $src, $res);
    )
}

macro_rules! assert_execute_fail_all {
    ($name:ident, $src:expr, $res:expr) => (
        assert_execute_fail_ast_walk!($src, $res);
        assert_execute_fail_cps!($src, $res);
        assert_execute_fail_jvm!($name, $src, $res);
    )
}

macro_rules! assert_execute_ast_walk {
    ($src:expr, $res:expr) => (assert_eq!(interpreter::new("ast_walk").execute($src).unwrap(), $res));
}

macro_rules! assert_execute_fail_ast_walk {
    ($src:expr, $res:expr) => (assert_eq!(interpreter::new("ast_walk").execute($src).unwrap_err(), $res));
}

macro_rules! assert_execute_cps {
    ($src:expr, $res:expr) => (assert_eq!(interpreter::new("cps").execute($src).unwrap(), $res));
}

macro_rules! assert_execute_fail_cps {
    ($src:expr, $res:expr) => (assert_eq!(interpreter::new("cps").execute($src).unwrap_err(), $res));
}

macro_rules! assert_execute_jvm {
    ($name:ident, $src:expr, $res:expr) => (assert_eq!(jvm_compiler::execute(stringify!(concat_idents!(test_, $name)), $src).unwrap(), $res));
}

macro_rules! assert_execute_fail_jvm {
    ($name:ident, $src:expr, $res:expr) => (assert_eq!(format!("{:?}", jvm_compiler::execute(stringify!(concat_idents!(test_, $name)), $src).unwrap_err()), $res));
}

test!(identity1, "1", "1");
test!(identity2, "#f", "#f");
test!(identity3, "\"hi\"", "\"hi\"");
test!(identity4, "(lambda (x) x)", "#<procedure>");

test!(addition1, "(+ 2 3)", "5");
test!(addition2, "(+ 2 -3)", "-1");
test!(addition3, "(+ 2 3 4 5)", "14");
test!(addition4, "(+ (+ 2 -3) (+ 4 -5) (+ 6 -7) (+ 8 -9 10 -11 12 -13))", "-6");

test!(subtraction1, "(- 3 2)", "1");
test!(subtraction2, "(- 2 -3)", "5");

test!(multiplication1, "(* 2 3)", "6");
test!(multiplication2, "(* 2 -3)", "-6");
test!(multiplication3, "(* 2 3 4 5)", "120");

test!(division1, "(/ 4 2)", "2");
test!(division2, "(/ 4 3)", "1");
test!(division3, "(/ 4 -2)", "-2");

test!(lessthan1, "(< 1 2)", "#t");
test!(lessthan2, "(< 2 2)", "#f");
test!(lessthan3, "(< 3 2)", "#f");
test!(lessthan4, "(< -1 2)", "#t");
test!(lessthan5, "(< 2 -1)", "#f");

test!(greaterthan1, "(> 3 2)", "#t");
test!(greaterthan2, "(> 2 2)", "#f");
test!(greaterthan3, "(> 1 2)", "#f");
test!(greaterthan4, "(> 2 -1)", "#t");
test!(greaterthan5, "(> -1 2)", "#f");

test!(equal1, "(= 2 2)", "#t");
test!(equal2, "(= 1 2)", "#f");
test!(equal3, "(= -1 -1)", "#t");
test!(equal4, "(= -1 2)", "#f");

test!(multiple_expression_return1, "(+ 2 3)\n(+ 1 2)", "3");

test!(nested_expressions1, "(+ 2 (- (+ 9 1) 4))", "8");

test!(list_creation1, "(list)", "()");
test!(list_creation2, "(list 1 2 3)", "(1 2 3)");
test!(list_creation3, "(list 1 (list 2 3) (list 4) (list))", "(1 (2 3) (4) ())");

test!(null1, "(null? '())", "#t");
test!(null2, "(null? '(1))", "#f");
test!(null3, "(null? '(()))", "#f");
test!(null4, "(null? 1)", "#f");
test!(null5, "(null? #t)", "#f");
test!(null6, "(null? #f)", "#f");
test!(null7, "(null? 'a)", "#f");
test!(null8, "(null? \"a\")", "#f");

test!(cons1, "(cons 1 '())", "(1)");
test!(cons2, "(cons 1 '(2))", "(1 2)");
test!(cons3, "(cons '(1) '(2))", "((1) 2)");

test!(car1, "(car '(1))", "1");
test!(car2, "(car '(1 2 3))", "1");
test!(car3, "(car '((1) (2 3)))", "(1)");
test_fail!(car4, "(car '())", "RuntimeError: Can't run car on an empty list");

test!(cdr1, "(cdr '(1 2))", "(2)");
test!(cdr2, "(cdr '(1 2 3))", "(2 3)");
test!(cdr3, "(cdr '(1))", "()");
test!(cdr4, "(cdr '((1) (2 3)))", "((2 3))");
test_fail!(cdr5, "(cdr '())", "RuntimeError: Can't run cdr on an empty list");

test!(append1, "(append '(1) '(2))", "(1 2)");
test!(append2, "(append '(1) '())", "(1)");
test!(append3, "(append '() '(2))", "(2)");
test!(append4, "(append '() '())", "()");
test!(append5, "(append '(1) '((2)))", "(1 (2))");

test!(variable_definition1, "(define x 2) (+ x x x)", "6");
test!(variable_definition2, "(define x 2) ((lambda (x) x) 3)", "3");
test!(variable_definition3, "(define x 2) (let ((x 3)) x)", "3");
test!(variable_definition4, "(define x 2) ((lambda (x) (define x 4) x) 3)", "4");
test!(variable_definition5, "(define x 2) (let ((x 3)) (define x 4) x)", "4");

test_fail!(duplicate_variable_definition1, "(define x 2) (define x 3)", "RuntimeError: Duplicate define: \"x\"");
test_fail!(duplicate_variable_definition2, "((lambda () (define x 2) (define x 3)))", "RuntimeError: Duplicate define: \"x\"");
test_fail!(duplicate_variable_definition3, "(let ((y 2)) (define x 2) (define x 3))", "RuntimeError: Duplicate define: \"x\"");

test!(variable_modification1, "(define x 2) (set! x 3) (+ x x x)", "9");
test!(variable_modification2, "(define x 2) ((lambda () (set! x 3))) x", "3");
test!(variable_modification3, "(define x 2) (let ((y 2)) (set! x 3)) x", "3");

test_fail!(unknown_variable_modification1, "(set! x 3)", "RuntimeError: Can't set! an undefined variable: \"x\"");

test!(procedure_definition1, "(define double (lambda (x) (+ x x))) (double 8)", "16");
test!(procedure_definition2, "(define twice (lambda (f v) (f (f v)))) (twice (lambda (x) (+ x x)) 8)", "32");
test!(procedure_definition3, "(define twice (λ (f v) (f (f v)))) (twice (λ (x) (+ x x)) 8)", "32");
test!(procedure_definition4, "((λ (x) (+ x x)) 8)", "16");
test!(procedure_definition5, "(define foo (λ (x) (λ (y) (+ x y)))) (define add2 (foo 2)) (add2 5)", "7");
test!(procedure_definition6, "(define foo (λ (x) (λ (y) (+ x y)))) (define add2 (foo 2)) ((λ (x) (add2 (+ x 1))) 1)", "4");
test!(procedure_definition7, "(define (twice f v) (f (f v))) (twice (lambda (x) (+ x x)) 8)", "32");

test!(begin_statement1, "(define x 1) (begin (set! x 5) (set! x (+ x 2)) x)", "7");

test!(let_statement1, "(let ((x 2)) (+ x x))", "4");
test!(let_statement2, "(let ((x 2) (y 3)) (+ x y))", "5");
test!(let_statement3, "(let ((x 2) (y 3)) (set! y (+ y 1)) (+ x y))", "6");

test!(conditional_execution1, "(if #t 1 2)", "1");
test!(conditional_execution2, "(if #f 1 2)", "2");
test!(conditional_execution3, "(if 0 1 2)", "1");
test!(conditional_execution4, "(if \"\" 1 2)", "1");

test!(conditional_execution_doesnt_run_other_case1, "(if #t 1 (error \"bad\"))", "1");
test!(conditional_execution_doesnt_run_other_case2, "(if #f (error \"bad\") 2)", "2");

test!(boolean_operators1, "(and)", "#t");
test!(boolean_operators2, "(and #t)", "#t");
test!(boolean_operators3, "(and 1)", "1");
test!(boolean_operators4, "(and 1 2 3)", "3");
test!(boolean_operators5, "(and 1 #f 3)", "#f");
test!(boolean_operators6, "(and 1 #f (error \"bad\"))", "#f");
test!(boolean_operators7, "(or)", "#f");
test!(boolean_operators8, "(or #f)", "#f");
test!(boolean_operators9, "(or 1)", "1");
test!(boolean_operators10, "(or 1 2)", "1");
test!(boolean_operators11, "(or 1 #f)", "1");
test!(boolean_operators12, "(or #f 3)", "3");
test!(boolean_operators13, "(or #f #f)", "#f");
test!(boolean_operators14, "(or 1 (error \"bad\"))", "1");

test!(quoting1, "(quote #t)", "#t");
test!(quoting2, "(quote 1)", "1");
test!(quoting3, "(quote sym)", "sym");
test!(quoting4, "(quote \"hi\")", "\"hi\"");
test!(quoting5, "(quote (1 2))", "(1 2)");
test!(quoting6, "(quote (a b))", "(a b)");
test!(quoting7, "(quote (a b (c (d) e ())))", "(a b (c (d) e ()))");
test!(quoting8, "(quote (a (quote b)))", "(a (quote b))");
test!(quoting9, "'(1 2)", "(1 2)");
test!(quoting10, "'(a b (c (d) e ()))", "(a b (c (d) e ()))");
test!(quoting11, "'(1 '2)", "(1 (quote 2))");

test!(quasiquoting1, "(quasiquote (1 2))", "(1 2)");
test!(quasiquoting2, "(quasiquote (2 (unquote (+ 1 2)) 4))", "(2 3 4)");
test!(quasiquoting3, "`(2 ,(+ 1 2) 4)", "(2 3 4)");
test!(quasiquoting4, "(define formula '(+ x y)) `((lambda (x y) ,formula) 2 3)", "((lambda (x y) (+ x y)) 2 3)");

test!(apply1, "(apply + '(1 2 3))", "6");
test!(apply2, "(define foo (lambda (f) (lambda (x y) (f (f x y) y)))) (apply (apply foo (list +)) '(5 3))", "11");

test!(eval1, "(eval '(+ 1 2 3))", "6");
test!(eval2, "(define eval-formula (lambda (formula) (eval `((lambda (x y) ,formula) 2 3)))) (eval-formula '(+ (- y x) y))", "4");
test_fail!(eval3, "(define bad-eval-formula (lambda (formula) ((lambda (x y) (eval formula)) 2 3))) (bad-eval-formula '(+ x y))", "RuntimeError: Identifier not found: x");

test_fail!(bad_syntax1, "(22+)", "SyntaxError: Unexpected character when looking for a delimiter: + (line: 1, column: 4)");
test_fail!(bad_syntax2, "(+ 2 3)\n(+ 1 2-)", "SyntaxError: Unexpected character when looking for a delimiter: - (line: 2, column: 7)");

test_fail!(generated_runtime_error1, "(error \"fail, please\")", "RuntimeError: \"fail, please\"");
test_fail!(generated_runtime_error2, "(error (+ 2 3))", "RuntimeError: 5");

test_fail!(errors_halt_execution1, "(error \"fail, please\") 5", "RuntimeError: \"fail, please\"");

test!(unicode_identifiers1, "(define ★ 3) (define ♫ 4) (+ ★ ♫)", "7");

test!(macros1, "(define-syntax-rule (incr x) (set! x (+ x 1))) (define a 1) (incr a) a", "2");
test!(macros2, "(define-syntax-rule (incr x) (set! x (+ x 1))) (define x 1) (incr x) x", "2");
test!(macros3, "(define-syntax-rule (incr x) (set! x (+ x 1))) (define-syntax-rule (foo x y z) (if x (incr y) (incr z))) (define a #t) (define b 10) (define c 20) (foo a b c) (set! a #f) (foo a b c) (list b c)", "(11 21)");
test!(macros4, "(define-syntax-rule (foo x) (if x (+ (foo #f) 3) 10)) (foo #t)", "13");
test!(macros5, "(define-syntax-rule (testy a b c) (if a b c)) (testy #t 1 (error \"test\")) (testy #f (error \"test\") 2)", "2");

test!(multiline1, "(define x 3)\n(define y 4)\n(+ x y)", "7");

test!(comment1, "(define x 3)\n(define y 4)\n;(set! y 5)\n(+ x y); (+ x y)", "7");

test!(tail_call_optimization1, "(define (f i) (if (= i 1000) '() (f (+ i 1)))) (f 1)", "()", cps);
