use parser;
use parser::Node;

use std::fmt;
use std::collections::HashMap;

pub fn interpret(nodes: &Vec<Node>) -> Result<Vec<Value>, RuntimeError> {
    let mut env = Environment::root();
    evaluate_nodes(nodes, &mut env)
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

struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    fn root() -> Environment {
        Environment { values: HashMap::new() }
    }

    fn set(&mut self, key: String, value: Value) {
        self.values.insert(key, value);
    }

    fn get(&self, key: &String) -> Option<Value> {
        match self.values.find(key) {
            Some(val) => Some(val.clone()),
            None => None
        }
    }
}

fn evaluate_nodes(nodes: &Vec<Node>, env: &mut Environment) -> Result<Vec<Value>, RuntimeError> {
    let mut results = Vec::new();
    for node in nodes.iter() {
        let res = try!(evaluate_node(node, env));
        results.push(res);
    };
    Ok(results)
}

fn evaluate_node(node: &Node, env: &mut Environment) -> Result<Value, RuntimeError> {
    match node {
        &parser::Identifier(ref v) => {
            match env.get(v) {
                Some(val) => Ok(val),
                None => runtime_error!("Identifier not found: {}", node)
            }
        },
        &parser::Integer(v) => Ok(Integer(v)),
        &parser::Boolean(v) => Ok(Boolean(v)),
        &parser::String(ref v) => Ok(String(v.clone())),
        &parser::List(ref vec) => {
            if vec.len() > 0 {
                evaluate_expression(vec, env)
            } else {
                Ok(List(vec![]))
            }
        }
    }
}

fn evaluate_expression(nodes: &Vec<Node>, env: &mut Environment) -> Result<Value, RuntimeError> {
    if nodes.len() == 0 {
        runtime_error!("Can't evaluate an empty expression: {}", nodes);
    }
    let first = nodes.get(0);
    match *first {
        parser::Identifier(ref func) => {
            match func.as_slice() {
                "define" => {
                    if nodes.len() != 3 {
                        runtime_error!("Must supply exactly two arguments to define: {}", nodes);
                    }
                    let name = match *nodes.get(1) {
                        parser::Identifier(ref x) => x,
                        _ => runtime_error!("Unexpected node for name in define: {}", nodes)
                    };
                    let val = try!(evaluate_node(nodes.get(2), env));
                    env.set(name.clone(), val);
                    Ok(Integer(0)) // TODO change to more sensible return value
                },
                "+" => {
                    if nodes.len() < 3 {
                        runtime_error!("Must supply at least two arguments to +: {}", nodes);
                    }
                    let mut sum = 0;
                    for n in nodes.tailn(1).iter() {
                        let v = try!(evaluate_node(n, env));
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
                    let v1 = try!(evaluate_node(nodes.get(1), env));
                    let v2 = try!(evaluate_node(nodes.get(2), env));
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

#[test]
fn test_global_variables() {
    assert_eq!(interpret(&vec![parser::List(vec![parser::Identifier("define".to_str()), parser::Identifier("x".to_str()), parser::Integer(2)]), parser::List(vec![parser::Identifier("+".to_str()), parser::Identifier("x".to_str()), parser::Identifier("x".to_str()), parser::Identifier("x".to_str())])]).unwrap(),
               vec![Integer(0), Integer(6)]);
}
