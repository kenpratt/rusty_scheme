RustyScheme
===========

A toy Scheme interpreter written in Rust, loosely based on the [R5RS Specification](http://www.schemers.org/Documents/Standards/R5RS/HTML/) with a bit of [Racket](http://docs.racket-lang.org/reference/index.html) thrown in as well.

It supports a small number of standard library functions, as well as:

* Function and variable definition
* Quote, Quasiquote/unquote
* Apply & eval
* Macros (not hygenic yet)
* Let expressions
* Tail-call optimization
* Continuations, [Call-with-current-continuation](http://en.wikipedia.org/wiki/Call-with-current-continuation)
* Unicode
* REPL, with history

There are two versions of the interpreter:

* A straight-forward AST-walking interpreter, whicth uses the Rust stack and heap, and uses vectors to represent Scheme lists.
* A [continuation-passing style](http://en.wikipedia.org/wiki/Continuation-passing_style) interpreter, which supports tail-call optimization and continuations, uses the Rust stack and heap, and uses a linked list to represent Scheme lists.

In the future, I may develop an interpreter that manages its own stack and/or heap, and possible a bytecode VM version as well for comparison.

Requirements
------------

* Rust 1.0-beta or later

Usage
-----

Download and install Rust 0.12.0 from http://www.rust-lang.org/install.html.

Build and start REPL:

    cargo run

Build & execute a Scheme file:

    cargo run myprogram.scm

Build & run unit tests:

    cargo test

Watch for changes and auto-rebuild (on OS X):

    gem install kicker -s http://gemcutter.org
    ./watch
