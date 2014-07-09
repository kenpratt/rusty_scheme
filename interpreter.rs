use parser;
use parser::Node;

use std::fmt;

pub fn interpret(nodes: Vec<Node>) -> Result<Vec<Node>, RuntimeError> {
    evaluate_nodes(nodes)
}

pub struct RuntimeError {
    message: String,
}

impl fmt::Show for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "RuntimeError: {}", self.message)
    }
}

macro_rules! runtime_error(
    ($($arg:tt)*) => (
        return Err(RuntimeError { message: format!($($arg)*)})
    )
)

fn evaluate_nodes(nodes: Vec<Node>) -> Result<Vec<Node>, RuntimeError> {
    let mut results = Vec::new();
    for node in nodes.move_iter() {
        let res = try!(evaluate_node(node));
        results.push(res);
    };
    Ok(results)
}

fn evaluate_node(node: Node) -> Result<Node, RuntimeError> {
    match node {
        parser::List(vec) => {
            if vec.len() > 0 {
                let evaluated = try!(evaluate_nodes(vec));
                evaluate_expression(evaluated)
            } else {
                Ok(parser::List(vec))
            }
        },
        _ => Ok(node)
    }
}

fn evaluate_expression(nodes: Vec<Node>) -> Result<Node, RuntimeError> {
    if nodes.len() == 0 {
        runtime_error!("Can't evaluate an empty expression: {}", nodes);
    }
    let first = nodes.get(0);
    match *first {
        parser::Identifier(ref func) => {
            if *func == "+".to_str() {
                if nodes.len() < 3 {
                    runtime_error!("Must supply at least two arguments to +: {}", nodes);
                }
                let mut sum = 0;
                for n in nodes.tailn(1).iter() {
                    match *n {
                        parser::Integer(x) => sum += x,
                        _ => runtime_error!("Unexpected node during +: {}", n)
                    };
                };
                Ok(parser::Integer(sum))
            } else if *func == "-".to_str() {
                if nodes.len() != 3 {
                    runtime_error!("Must supply exactly two arguments to -: {}", nodes);
                }
                let mut result = match *nodes.get(1) {
                    parser::Integer(x) => x,
                    _ => runtime_error!("Unexpected node during -: {}", nodes)
                };
                result -= match *nodes.get(2) {
                    parser::Integer(x) => x,
                    _ => runtime_error!("Unexpected node during -: {}", nodes)
                };
                Ok(parser::Integer(result))
            } else {
                runtime_error!("Unknown function: {}", func);
            }
        },
        _ => {
            runtime_error!("First element in an expression must be an identifier: {}", first);
        }
    }
}
