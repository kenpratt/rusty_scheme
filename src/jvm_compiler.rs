use std::{convert, fs, io};
use std::io::Write;
use std::fs::File;
use std::path::Path;
use std::process::Command;

use classfile::*;
use classfile_builder::*;
use lexer;
use parser::*;

pub fn execute(name: &str, input: &str) -> Result<String, Error> {
    let tokens = try!(lexer::tokenize(input));
    let ast = try!(parse(&tokens));
    try!(compile(&name, &ast));
    let res = run_jvm(&name);
    try!(remove_file(&name));
    res
}

fn compile(name: &str, nodes: &[Node]) -> Result<(), Error> {
    let classfile = build_classfile(name, nodes);

    let mut bytes = vec![];
    classfile.serialize(&mut bytes);

    let filepath_str = format!("{}.class", name);
    let filepath = Path::new(&filepath_str);

    let mut f = try!(File::create(filepath));
    try!(f.write_all(&bytes));
    Ok(())
}

fn remove_file(name: &str) -> Result<(), Error> {
    let filepath_str = format!("{}.class", name);
    let filepath = Path::new(&filepath_str);
    try!(fs::remove_file(filepath));
    Ok(())
}

fn build_classfile(name: &str, nodes: &[Node]) -> Classfile {
    let mut classfile = ClassfileBuilder::new(ACC_PUBLIC, name, "java/lang/Object");

    {
        // create main method and push the PrintStrea object onto the stack so we can use it for print later
        let mut method = classfile.define_method(ACC_PUBLIC | ACC_STATIC, "main", "([Ljava/lang/String;)V");
        method.get_static("java/lang/System", "out", "Ljava/io/PrintStream;");

        for node in nodes {
            process_node(&mut method, node);
            // TODO clear stack depth, if any (but not on last element
        }

        // print the result left on the stack, and return
        method.invoke_virtual("java/io/PrintStream", "print", "(I)V");
        method.do_return();
        method.done();
    }

    classfile.done()
}

fn process_node(method: &mut MethodBuilder, node: &Node) {
    match *node {
        // Node::Identifier(ref val) => Value::Symbol(val.clone()),
        Node::Integer(val) => {
            if val >= -128 || val < 127 {
                method.bipush(val as i8);
            } else {
                // TODO create constant
                panic!("int size not supported yet: {}", val);
            }
        },
        // Node::Boolean(val) => Value::Boolean(val),
        // Node::String(ref val) => Value::String(val.clone()),
        Node::List(ref nodes) => {
            let fun = &nodes[0];
            let args = &nodes[1..];
            match *fun {
                Node::Identifier(ref s) if s == "+" => {
                    // push first value
                    process_node(method, &args[0]);
                    for v in &args[1..] {
                        // for subsequent values, push them and then call iadd
                        process_node(method, v);
                        method.iadd();
                    }
                },
                _ => panic!("Function not supported yet: {:?}", fun)
            }
        },
        _ => panic!("Node type not supported yet: {:?}", node)
    }
}

fn run_jvm(class: &str) -> Result<String, Error> {
    let output = try!(Command::new("java").arg(class).output());
    if output.status.success() && output.stderr.len() == 0 {
        Ok(String::from_utf8_lossy(&output.stdout).into_owned())
    } else {
        let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
        let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
        Err(Error::ExecutionFailed(output.status.code(), stdout, stderr))
    }
}

#[derive(Debug)]
pub enum Error {
    IoError(io::Error),
    SyntaxError(lexer::SyntaxError),
    ParseError(ParseError),
    ExecutionFailed(Option<i32>, String, String),
}

impl convert::From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::IoError(err)
    }
}

impl convert::From<lexer::SyntaxError> for Error {
    fn from(err: lexer::SyntaxError) -> Error {
        Error::SyntaxError(err)
    }
}

impl convert::From<ParseError> for Error {
    fn from(err: ParseError) -> Error {
        Error::ParseError(err)
    }
}
