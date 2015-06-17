RustyScheme
===========

A toy Scheme interpreter written in Rust, loosely based on the [R5RS Specification](http://www.schemers.org/Documents/Standards/R5RS/HTML/) with a bit of [Racket](http://docs.racket-lang.org/reference/index.html) thrown in as well.

It supports a small number of standard library functions, as well as:

* Function and variable definition
* Quote, Quasiquote/unquote
* Apply & Eval
* Macros (not hygenic yet)
* Let expressions
* Tail-call optimization
* Continuations, [Call-with-current-continuation](http://en.wikipedia.org/wiki/Call-with-current-continuation)
* Unicode
* REPL, with history

There are two versions of the interpreter:

* A straight-forward AST-walking interpreter, which uses the Rust stack and heap, and uses vectors to represent Scheme lists.
* A [continuation-passing style](http://en.wikipedia.org/wiki/Continuation-passing_style) interpreter, which supports tail-call optimization and continuations, uses the Rust stack and heap, and uses a linked list to represent Scheme lists.

In the future, I may develop an interpreter that manages its own stack and/or heap, and possibly a bytecode VM & compiler as well for comparison.

Requirements
------------

* Rust 1.0-beta or later

Usage
-----

Download and install Rust 1.0 from http://www.rust-lang.org/install.html.

To start a REPL using the default CPS interpreter:

    cargo run

To execute a Scheme file using the default CPS interpreter:

    cargo run examples/printing.scm

To start a REPL using the AST-walking interpreter:

    cargo run -- -t ast_walk

To execute a Scheme file using the AST-walking interpreter:

    cargo run -- -t ast_walk examples/printing.scm

To run the test suite:

    cargo test

To watch for changes and auto-rebuild (on OS X):

    gem install kicker -s http://gemcutter.org
    ./watch
