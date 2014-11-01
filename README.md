Rusty Scheme
============

A toy Scheme interpreter written in Rust, loosely based on the [R5RS Specification](http://www.schemers.org/Documents/Standards/R5RS/HTML/) with a bit of [Racket](http://docs.racket-lang.org/reference/index.html) thrown in as well.

It supports a small number of standard library functions, as well as:

* Function and variable definition
* Quote
* Quasiquote/unquote
* Apply & eval
* Macros (not hygenic yet)
* Let expressions
* Unicode
* REPL with history

Rusty Scheme currently uses the Rust stack and heap, implementing the standard library directly in Rust. I intend to refactor the interpret to manage its own stack and instruction pointer in order to support [call-with-current-continuation](http://en.wikipedia.org/wiki/Call-with-current-continuation), and may also develop a bytecode/VM version for comparison.

Requirements
------------

* Rust 0.12.0

Usage
-----

Download and install Rust 0.12.0 from http://www.rust-lang.org/install.html.

Build and start REPL:

    make && ./bin/rusty_scheme

Build & execute a Scheme file:

    make && ./bin/rusty_scheme myprogram.scm

Build & run unit tests:

    make test
