use parser;
use parser::Node;

use std::fmt;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

pub fn interpret(nodes: &Vec<Node>) -> Result<Value, RuntimeError> {
    let env = Environment::new_root();
    evaluate_nodes(nodes, env)
}

#[deriving(Show, PartialEq, Clone)]
pub enum Value {
    Integer(int),
    Boolean(bool),
    String(String),
    List(Vec<Value>),
    Procedure(Vec<String>, Vec<Node>),
}

// null == empty list
macro_rules! null { () => (List(vec![])) }

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
    parent: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Value>,
}

impl Environment {
    fn new_root() -> Rc<RefCell<Environment>> {
        let env = Environment { parent: None, values: HashMap::new() };
        Rc::new(RefCell::new(env))
    }

    fn new_child(parent: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        let env = Environment { parent: Some(parent), values: HashMap::new() };
        Rc::new(RefCell::new(env))
    }

    fn set(&mut self, key: String, value: Value) {
        self.values.insert(key, value);
    }

    fn get(&self, key: &String) -> Option<Value> {
        match self.values.find(key) {
            Some(val) => Some(val.clone()),
            None => {
                // recurse up the environment tree until a value is found or the end is reached
                match self.parent {
                    Some(ref parent) => parent.borrow().get(key),
                    None => None
                }
            }
        }
    }
}

fn evaluate_nodes(nodes: &Vec<Node>, env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    let mut result = null!();
    for node in nodes.iter() {
        result = try!(evaluate_node(node, env.clone()));
    };
    Ok(result)
}

fn evaluate_node(node: &Node, env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    match node {
        &parser::Identifier(ref v) => {
            match env.borrow().get(v) {
                Some(val) => Ok(val),
                None => runtime_error!("Identifier not found: {}", node)
            }
        },
        &parser::Integer(v) => Ok(Integer(v)),
        &parser::Boolean(v) => Ok(Boolean(v)),
        &parser::String(ref v) => Ok(String(v.clone())),
        &parser::List(ref vec) => {
            if vec.len() > 0 {
                evaluate_expression(vec, env.clone())
            } else {
                Ok(null!())
            }
        }
    }
}

fn evaluate_expression(nodes: &Vec<Node>, env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
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
                    let val = try!(evaluate_node(nodes.get(2), env.clone()));
                    env.borrow_mut().set(name.clone(), val);
                    Ok(null!())
                },
                "lambda" => {
                    if nodes.len() < 3 {
                        runtime_error!("Must supply at least two arguments to lambda: {}", nodes);
                    }
                    let args = match *nodes.get(1) {
                        parser::List(ref list) => {
                            let mut names = vec![];
                            for item in list.iter() {
                                match *item {
                                    parser::Identifier(ref s) => names.push(s.clone()),
                                    _ => runtime_error!("Unexpected argument in lambda arguments: {}", item)
                                };
                            }
                            names
                        }
                        _ => runtime_error!("Unexpected node for arguments in lambda: {}", nodes)
                    };
                    let expressions = Vec::from_slice(nodes.tailn(2));
                    Ok(Procedure(args, expressions))
                }
                "+" => {
                    if nodes.len() < 3 {
                        runtime_error!("Must supply at least two arguments to +: {}", nodes);
                    }
                    let mut sum = 0;
                    for n in nodes.tailn(1).iter() {
                        let v = try!(evaluate_node(n, env.clone()));
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
                    let v1 = try!(evaluate_node(nodes.get(1), env.clone()));
                    let v2 = try!(evaluate_node(nodes.get(2), env.clone()));
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
                    match env.borrow().get(func) {
                        Some(Procedure(args, body)) => {
                            if nodes.len() != args.len() + 1 {
                                runtime_error!("Must supply exactly {} arguments to {}: {}", args.len(), func, nodes);
                            }

                            // create a new, child environment for the procedure and define the arguments as local variables
                            let procEnv = Environment::new_child(env.clone());
                            for (arg, node) in args.iter().zip(nodes.tailn(1).iter()) {
                                let val = try!(evaluate_node(node, env.clone()));
                                procEnv.borrow_mut().set(arg.clone(), val);
                            }

                            Ok(try!(evaluate_nodes(&body, procEnv)))
                        },
                        Some(other) => runtime_error!("Can't execute a non-procedure: {}", other),
                        None => runtime_error!("Unknown function: {}", func)
                    }
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
               Integer(6));
}

#[test]
fn test_global_function_definition() {
    assert_eq!(interpret(&vec![parser::List(vec![parser::Identifier("define".to_str()), parser::Identifier("double".to_str()), parser::List(vec![parser::Identifier("lambda".to_str()), parser::List(vec![parser::Identifier("x".to_str())]), parser::List(vec![parser::Identifier("+".to_str()), parser::Identifier("x".to_str()), parser::Identifier("x".to_str())])])]), parser::List(vec![parser::Identifier("double".to_str()), parser::Integer(8)])]).unwrap(),
               Integer(16));
}
