use parser;
use parser::Node;

use std::fmt;

pub fn interpret(nodes: &Vec<Node>) -> Result<Vec<Value>, RuntimeError> {
    evaluate_nodes(nodes)
}

#[deriving(Show, PartialEq, Clone)]
pub enum Value {
    Integer(int),
    Boolean(bool),
    String(String),
    List(Vec<Value>),
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

fn evaluate_nodes(nodes: &Vec<Node>) -> Result<Vec<Value>, RuntimeError> {
    let mut results = Vec::new();
    for node in nodes.iter() {
        let res = try!(evaluate_node(node));
        results.push(res);
    };
    Ok(results)
}

fn evaluate_node(node: &Node) -> Result<Value, RuntimeError> {
    match node {
        &parser::Integer(v) => Ok(Integer(v)),
        &parser::Boolean(v) => Ok(Boolean(v)),
        &parser::String(ref v) => Ok(String(v.clone())),
        &parser::List(ref vec) => {
            if vec.len() > 0 {
                evaluate_expression(vec)
            } else {
                Ok(List(vec![]))
            }
        },
        _ => runtime_error!("Can't evaluate node: {}", node) // Ok(node.clone())
    }
}

fn evaluate_expression(nodes: &Vec<Node>) -> Result<Value, RuntimeError> {
    if nodes.len() == 0 {
        runtime_error!("Can't evaluate an empty expression: {}", nodes);
    }
    let first = nodes.get(0);
    match *first {
        parser::Identifier(ref func) => {
            match func.as_slice() {
                "+" => {
                    if nodes.len() < 3 {
                        runtime_error!("Must supply at least two arguments to +: {}", nodes);
                    }
                    let mut sum = 0;
                    for n in nodes.tailn(1).iter() {
                        let v = try!(evaluate_node(n));
                        match v {
                            Integer(x) => sum += x,
                            _ => runtime_error!("Unexpected node during +: {}", n)
                        };
                    };
                    Ok(Integer(sum))
                },
                "-" => {
                    if nodes.len() != 3 {
                        runtime_error!("Must supply exactly two arguments to -: {}", nodes);
                    }
                    let v1 = try!(evaluate_node(nodes.get(1)));
                    let v2 = try!(evaluate_node(nodes.get(2)));
                    let mut result = match v1 {
                        Integer(x) => x,
                        _ => runtime_error!("Unexpected node during -: {}", nodes)
                    };
                    result -= match v2 {
                        Integer(x) => x,
                        _ => runtime_error!("Unexpected node during -: {}", nodes)
                    };
                    Ok(Integer(result))
                },
                _ => {
                    runtime_error!("Unknown function: {}", func);
                }
            }
        },
        _ => {
            runtime_error!("First element in an expression must be an identifier: {}", first);
        }
    }
}
