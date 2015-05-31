use parser::*;

use std::fmt;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

pub fn new() -> Interpreter {
    Interpreter::new()
}

#[derive(Clone)]
pub struct Interpreter {
    root: Rc<RefCell<Environment>>
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter { root: Environment::new_root() }
    }

    pub fn run(&self, nodes: &[Node]) -> Result<Value, RuntimeError> {
        let values = Value::from_nodes(nodes);
        evaluate_values(&values, self.root.clone())
    }
}

#[derive(PartialEq, Clone)]
pub enum Value {
    Symbol(String),
    Integer(i64),
    Boolean(bool),
    String(String),
    List(Vec<Value>),
    Procedure(Function),
    Macro(Vec<String>, Vec<Value>),
}

// null == empty list
macro_rules! null { () => (Value::List(vec![])) }

pub enum Function {
    Native(ValueOperation),
    Scheme(Vec<String>, Vec<Value>, Rc<RefCell<Environment>>),
}

// type signature for all native functions
type ValueOperation = fn(&[Value], Rc<RefCell<Environment>>) -> Result<Value, RuntimeError>;

impl Value {
    fn from_nodes(nodes: &[Node]) -> Vec<Value> {
        nodes.iter().map(Value::from_node).collect()
    }

    fn from_node(node: &Node) -> Value {
        match *node {
            Node::Identifier(ref val) => Value::Symbol(val.clone()),
            Node::Integer(val) => Value::Integer(val),
            Node::Boolean(val) => Value::Boolean(val),
            Node::String(ref val) => Value::String(val.clone()),
            Node::List(ref nodes) => Value::List(Value::from_nodes(&nodes))
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Symbol(ref val) => write!(f, "{}", val),
            Value::Integer(val)    => write!(f, "{}", val),
            Value::Boolean(val)    => write!(f, "#{}", if val { "t" } else { "f" }),
            Value::String(ref val) => write!(f, "{}", val),
            Value::List(ref list)  => {
                let strs: Vec<String> = list.iter().map(|v| format!("{}", v)).collect();
                write!(f, "({})", &strs.connect(" "))
            },
            Value::Procedure(_)   => write!(f, "#<procedure>"),
            Value::Macro(_,_)     => write!(f, "#<macro>"),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::String(ref val) => write!(f, "\"{}\"", val),
            Value::List(ref list)  => {
                let strs: Vec<String> = list.iter().map(|v| format!("{:?}", v)).collect();
                write!(f, "({})", &strs.connect(" "))
            },
            _                      => write!(f, "{}", self)
        }
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Function) -> bool {
        self == other
    }
}

impl Clone for Function {
    fn clone(&self) -> Function {
        match *self {
            Function::Native(ref func) => Function::Native(*func),
            Function::Scheme(ref a, ref b, ref env) => Function::Scheme(a.clone(), b.clone(), env.clone())
        }
    }
}

pub struct RuntimeError {
    message: String,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "RuntimeError: {}", self.message)
    }
}
impl fmt::Debug for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "RuntimeError: {}", self.message)
    }
}

macro_rules! runtime_error {
    ($($arg:tt)*) => (
        return Err(RuntimeError { message: format!($($arg)*)})
    )
}

struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Value>,
}

impl Environment {
    fn new_root() -> Rc<RefCell<Environment>> {
        let mut env = Environment { parent: None, values: HashMap::new() };
        let predefined_functions = &[
            ("define", Function::Native(native_define)),
            ("define-syntax-rule", Function::Native(native_define_syntax_rule)),
            ("begin", Function::Native(native_begin)),
            ("let", Function::Native(native_let)),
            ("set!", Function::Native(native_set)),
            ("lambda", Function::Native(native_lambda)),
            ("λ", Function::Native(native_lambda)),
            ("if", Function::Native(native_if)),
            ("+", Function::Native(native_plus)),
            ("-", Function::Native(native_minus)),
            ("*", Function::Native(native_multiply)),
            ("/", Function::Native(native_divide)),
            ("<", Function::Native(native_lessthan)),
            (">", Function::Native(native_greaterthan)),
            ("=", Function::Native(native_equal)),
            ("and", Function::Native(native_and)),
            ("or", Function::Native(native_or)),
            ("null?", Function::Native(native_null)),
            ("list", Function::Native(native_list)),
            ("car", Function::Native(native_car)),
            ("cdr", Function::Native(native_cdr)),
            ("cons", Function::Native(native_cons)),
            ("append", Function::Native(native_append)),
            ("quote", Function::Native(native_quote)),
            ("quasiquote", Function::Native(native_quasiquote)),
            ("error", Function::Native(native_error)),
            ("apply", Function::Native(native_apply)),
            ("eval", Function::Native(native_eval)),
            ("write", Function::Native(native_write)),
            ("display", Function::Native(native_display)),
            ("displayln", Function::Native(native_displayln)),
            ("print", Function::Native(native_print)),
            ("newline", Function::Native(native_newline)),
            ];
        for item in predefined_functions.iter() {
            let (name, ref func) = *item;
            env.define(name.to_string(), Value::Procedure(func.clone())).unwrap();
        }
        Rc::new(RefCell::new(env))
    }

    fn new_child(parent: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        let env = Environment { parent: Some(parent), values: HashMap::new() };
        Rc::new(RefCell::new(env))
    }

    // Define a variable at the current level
    // If key is not defined in the current env, set it
    // If key is already defined in the current env, return runtime error
    // (So if key is defined at a higher level, still define it at the current level)
    fn define(&mut self, key: String, value: Value) -> Result<(), RuntimeError> {
        if self.values.contains_key(&key) {
            runtime_error!("Duplicate define: {:?}", key)
        } else {
            self.values.insert(key, value);
            Ok(())
        }
    }

    // Set a variable to a value, at any level in the env, or throw a runtime error if it isn't defined at all
    fn set(&mut self, key: String, value: Value) -> Result<(), RuntimeError>  {
        if self.values.contains_key(&key) {
            self.values.insert(key, value);
            Ok(())
        } else {
            // recurse up the environment tree until a value is found or the end is reached
            match self.parent {
                Some(ref parent) => parent.borrow_mut().set(key, value),
                None => runtime_error!("Can't set! an undefined variable: {:?}", key)
            }
        }
    }

    fn get(&self, key: &String) -> Option<Value> {
        match self.values.get(key) {
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

    fn get_root(env_ref: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        let env = env_ref.borrow();
        match env.parent {
            Some(ref parent) => Environment::get_root(parent.clone()),
            None => env_ref.clone()
        }
    }
}

fn evaluate_values(values: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    let mut res = null!();
    for v in values.iter() {
        res = try!(evaluate_value(v, env.clone()));
    }
    Ok(res)
}

fn evaluate_value(value: &Value, env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    match value {
        &Value::Symbol(ref v) => {
            match env.borrow().get(v) {
                Some(val) => Ok(val),
                None => runtime_error!("Identifier not found: {:?}", value)
            }
        },
        &Value::Integer(v) => Ok(Value::Integer(v)),
        &Value::Boolean(v) => Ok(Value::Boolean(v)),
        &Value::String(ref v) => Ok(Value::String(v.clone())),
        &Value::List(ref vec) => {
            if vec.len() > 0 {
                evaluate_expression(vec, env.clone())
            } else {
                Ok(null!())
            }
        },
        &Value::Procedure(ref v) => Ok(Value::Procedure(v.clone())),
        &Value::Macro(ref a, ref b) => Ok(Value::Macro(a.clone(), b.clone())),
    }
}

fn quote_value(value: &Value, quasi: bool, env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    match value {
        &Value::Symbol(ref v) => Ok(Value::Symbol(v.clone())),
        &Value::Integer(v) => Ok(Value::Integer(v)),
        &Value::Boolean(v) => Ok(Value::Boolean(v)),
        &Value::String(ref v) => Ok(Value::String(v.clone())),
        &Value::List(ref vec) => {
            // check if we are unquoting inside a quasiquote
            if quasi && vec.len() > 0 && vec[0] == Value::Symbol("unquote".to_string()) {
                if vec.len() != 2 {
                    runtime_error!("Must supply exactly one argument to unquote: {:?}", vec);
                }
                evaluate_value(&vec[1], env.clone())
            } else {
                let res: Result<Vec<Value>, RuntimeError> = vec.iter().map(|v| quote_value(v, quasi, env.clone())).collect();
                let new_vec = try!(res);
                Ok(Value::List(new_vec))
            }
        },
        &Value::Procedure(ref v) => Ok(Value::Procedure(v.clone())),
        &Value::Macro(ref a, ref b) => Ok(Value::Macro(a.clone(), b.clone())),
    }
}

fn evaluate_expression(values: &Vec<Value>, env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if values.len() == 0 {
        runtime_error!("Can't evaluate an empty expression: {:?}", values);
    }
    let first = try!(evaluate_value(&values[0], env.clone()));
    match first {
        Value::Procedure(f) => apply_function(&f, &values[1..], env.clone()),
        Value::Macro(a, b) => expand_macro(a, b, &values[1..], env.clone()),
        _ => runtime_error!("First element in an expression must be a procedure: {:?}", first)
    }
}

fn apply_function(func: &Function, args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    match func {
        &Function::Native(native_fn) => {
            native_fn(args, env)
        },
        &Function::Scheme(ref arg_names, ref body, ref func_env) => {
            if arg_names.len() != args.len() {
                runtime_error!("Must supply exactly {} arguments to function: {:?}", arg_names.len(), args);
            }

            // create a new, child environment for the procedure and define the arguments as local variables
            let proc_env = Environment::new_child(func_env.clone());
            for (name, arg) in arg_names.iter().zip(args.iter()) {
                let val = try!(evaluate_value(arg, env.clone()));
                try!(proc_env.borrow_mut().define(name.clone(), val));
            }

            // evaluate procedure body with new environment with procedure environment as parent
            let inner_env = Environment::new_child(proc_env);
            evaluate_values(&body, inner_env)
        }
    }
}

fn expand_macro(arg_names: Vec<String>, body: Vec<Value>, args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    let mut substitutions = HashMap::new();
    for (name, arg) in arg_names.iter().zip(args.iter()) {
        substitutions.insert(name.clone(), arg.clone());
    }
    let expanded = try!(expand_macro_substitute_values(&body, substitutions));
    evaluate_values(&expanded, env)
}

fn expand_macro_substitute_values(values: &[Value], substitutions: HashMap<String,Value>) -> Result<Vec<Value>, RuntimeError> {
    values.iter().map(|n| expand_macro_substitute_value(n, substitutions.clone())).collect()
}

fn expand_macro_substitute_value(value: &Value, substitutions: HashMap<String,Value>) -> Result<Value, RuntimeError> {
    let res = match value {
        &Value::Symbol(ref s) => {
            if substitutions.contains_key(s) {
                substitutions.get(s).unwrap().clone()
            } else {
                Value::Symbol(s.clone())
            }
        },
        &Value::List(ref l) => {
            Value::List(try!(expand_macro_substitute_values(&l, substitutions)))
        },
        other => other.clone()
    };
    Ok(res)
}

fn native_define(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() < 2 {
        runtime_error!("Must supply at least two arguments to define: {:?}", args);
    }
    let (name, val) = match args[0] {
        Value::Symbol(ref name) => {
            let val = try!(evaluate_value(&args[1], env.clone()));
            (name, val)
        },
        Value::List(ref list) => {
            // if a list is the second argument, it's shortcut for defining a procedure
            // (define (<name> <args>) <body>) == (define <name> (lambda (<args>) <body>)
            if list.len() < 1 {
                runtime_error!("Must supply at least one argument in list part of define: {:?}", list);
            }
            match list[0] {
                Value::Symbol(ref name) => {
                    let res: Result<Vec<String>, RuntimeError> = (&list[1..]).iter().map(|i| match *i {
                        Value::Symbol(ref s) => Ok(s.clone()),
                        _ => runtime_error!("Unexpected argument in define arguments: {:?}", i)
                    }).collect();
                    let arg_names = try!(res);
                    let body = (&args[1..]).to_vec();
                    let val = Value::Procedure(Function::Scheme(arg_names, body, env.clone()));
                    (name, val)
                },
                _ => runtime_error!("Must supply a symbol in list part of define: {:?}", list)
            }
        },
        _ => runtime_error!("Unexpected value for name in define: {:?}", args)
    };

    try!(env.borrow_mut().define(name.clone(), val));
    Ok(null!())
}

fn native_define_syntax_rule(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("Must supply exactly two arguments to define-syntax-rule: {:?}", args);
    }
    let (name, val) = match args[0] {
        Value::List(ref list) => {
            // (define-syntax-rule (<name> <args>) <template>)
            if list.len() < 1 {
                runtime_error!("Must supply at least one argument in list part of define-syntax-rule: {:?}", list);
            }
            match list[0] {
                Value::Symbol(ref name) => {
                    let res: Result<Vec<String>, RuntimeError> = (&list[1..]).iter().map(|i| match *i {
                        Value::Symbol(ref s) => Ok(s.clone()),
                        _ => runtime_error!("Unexpected argument in define-syntax-rule arguments: {:?}", i)
                    }).collect();
                    let arg_names = try!(res);
                    let body = (&args[1..]).to_vec();
                    let val = Value::Macro(arg_names, body);
                    (name, val)
                },
                _ => runtime_error!("Must supply a symbol in list part of define: {:?}", list)
            }
        },
        _ => runtime_error!("Unexpected value for pattern in define-syntax-rule: {:?}", args)
    };

    try!(env.borrow_mut().define(name.clone(), val));
    Ok(null!())
}

fn native_begin(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() < 1 {
        runtime_error!("Must supply at least one argument to begin: {:?}", args);
    }
    evaluate_values(args, env)
}

fn native_let(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() < 2 {
        runtime_error!("Must supply at least two arguments to let: {:?}", args);
    }

    // create a new, child environment for the let expression and define the arguments as local variables
    let let_env = Environment::new_child(env.clone());
    match args[0] {
        Value::List(ref list) => {
            for i in list.iter() {
                match *i {
                    Value::List(ref entry) => {
                        if entry.len() != 2 {
                            runtime_error!("let expression values must have exactly 2 params: {:?}", entry);
                        }
                        let name = match entry[0] {
                            Value::Symbol(ref x) => x,
                            _ => runtime_error!("Unexpected value for name in set!: {:?}", args)
                        };
                        let val = try!(evaluate_value(&entry[1], env.clone()));
                        try!(let_env.borrow_mut().define(name.clone(), val));
                    },
                    _ => runtime_error!("Unexpected value inside expression in let: {:?}", i)
                }
            }
        },
        _ => runtime_error!("Unexpected value for expressions in let: {:?}", args)
    };

    // evaluate let statement body with new environment with let environment as parent
    let inner_env = Environment::new_child(let_env);
    let body = &args[1..];
    evaluate_values(body, inner_env)
}

fn native_set(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("Must supply exactly two arguments to set!: {:?}", args);
    }
    let name = match args[0] {
        Value::Symbol(ref x) => x,
        _ => runtime_error!("Unexpected value for name in set!: {:?}", args)
    };
    let val = try!(evaluate_value(&args[1], env.clone()));
    try!(env.borrow_mut().set(name.clone(), val));
    Ok(null!())
}

fn native_lambda(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() < 2 {
        runtime_error!("Must supply at least two arguments to lambda: {:?}", args);
    }
    let arg_names = match args[0] {
        Value::List(ref list) => {
            let res: Result<Vec<String>, RuntimeError> = list.iter().map(|i| match *i {
                Value::Symbol(ref s) => Ok(s.clone()),
                _ => runtime_error!("Unexpected argument in lambda arguments: {:?}", i)
            }).collect();
            try!(res)
        }
        _ => runtime_error!("Unexpected value for arguments in lambda: {:?}", args)
    };
    let body = (&args[1..]).to_vec();
    Ok(Value::Procedure(Function::Scheme(arg_names, body, env.clone())))
}

fn native_if(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 3 {
        runtime_error!("Must supply exactly three arguments to if: {:?}", args);
    }
    let condition = try!(evaluate_value(&args[0], env.clone()));
    match condition {
        Value::Boolean(false) => evaluate_value(&args[2], env.clone()),
        _ => evaluate_value(&args[1], env.clone())
    }
}

fn native_plus(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() < 2 {
        runtime_error!("Must supply at least two arguments to +: {:?}", args);
    }
    let mut sum = 0;
    for n in args.iter() {
        let v = try!(evaluate_value(n, env.clone()));
        match v {
            Value::Integer(x) => sum += x,
            _ => runtime_error!("Unexpected value during +: {:?}", n)
        };
    };
    Ok(Value::Integer(sum))
}

fn native_minus(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("Must supply exactly two arguments to -: {:?}", args);
    }
    let l = try!(evaluate_value(&args[0], env.clone()));
    let r = try!(evaluate_value(&args[1], env.clone()));
    let mut result = match l {
        Value::Integer(x) => x,
        _ => runtime_error!("Unexpected value during -: {:?}", args)
    };
    result -= match r {
        Value::Integer(x) => x,
        _ => runtime_error!("Unexpected value during -: {:?}", args)
    };
    Ok(Value::Integer(result))
}

fn native_multiply(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() < 2 {
        runtime_error!("Must supply at least two arguments to *: {:?}", args);
    }
    let mut product = 1;
    for n in args.iter() {
        let v = try!(evaluate_value(n, env.clone()));
        match v {
            Value::Integer(x) => product *= x,
            _ => runtime_error!("Unexpected value during *: {:?}", n)
        };
    };
    Ok(Value::Integer(product))
}

fn native_divide(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("Must supply exactly two arguments to /: {:?}", args);
    }
    let l = try!(evaluate_value(&args[0], env.clone()));
    let r = try!(evaluate_value(&args[1], env.clone()));
    let mut result = match l {
        Value::Integer(x) => x,
        _ => runtime_error!("Unexpected value during /: {:?}", args)
    };
    result /= match r {
        Value::Integer(x) => x,
        _ => runtime_error!("Unexpected value during /: {:?}", args)
    };
    Ok(Value::Integer(result))
}

fn native_lessthan(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("Must supply exactly two arguments to <: {:?}", args);
    }
    let l_raw = try!(evaluate_value(&args[0], env.clone()));
    let r_raw = try!(evaluate_value(&args[1], env.clone()));
    let l = match l_raw {
        Value::Integer(x) => x,
        _ => runtime_error!("Unexpected value during <: {:?}", args)
    };
    let r = match r_raw {
        Value::Integer(x) => x,
        _ => runtime_error!("Unexpected value during <: {:?}", args)
    };
    Ok(Value::Boolean(l < r))
}

fn native_greaterthan(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("Must supply exactly two arguments to >: {:?}", args);
    }
    let l_raw = try!(evaluate_value(&args[0], env.clone()));
    let r_raw = try!(evaluate_value(&args[1], env.clone()));
    let l = match l_raw {
        Value::Integer(x) => x,
        _ => runtime_error!("Unexpected value during >: {:?}", args)
    };
    let r = match r_raw {
        Value::Integer(x) => x,
        _ => runtime_error!("Unexpected value during >: {:?}", args)
    };
    Ok(Value::Boolean(l > r))
}

fn native_equal(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("Must supply exactly two arguments to =: {:?}", args);
    }
    let l_raw = try!(evaluate_value(&args[0], env.clone()));
    let r_raw = try!(evaluate_value(&args[1], env.clone()));
    let l = match l_raw {
        Value::Integer(x) => x,
        _ => runtime_error!("Unexpected value during =: {:?}", args)
    };
    let r = match r_raw {
        Value::Integer(x) => x,
        _ => runtime_error!("Unexpected value during =: {:?}", args)
    };
    Ok(Value::Boolean(l == r))
}

fn native_and(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    let mut res = Value::Boolean(true);
    for n in args.iter() {
        let v = try!(evaluate_value(n, env.clone()));
        match v {
            Value::Boolean(false) => return Ok(Value::Boolean(false)),
            _ => res = v
        }
    }
    Ok(res)
}

fn native_or(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    for n in args.iter() {
        let v = try!(evaluate_value(n, env.clone()));
        match v {
            Value::Boolean(false) => (),
            _ => return Ok(v)
        }
    }
    Ok(Value::Boolean(false))
}

fn native_null(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("Must supply exactly one argument to null?: {:?}", args);
    }
    let v = try!(evaluate_value(&args[0], env.clone()));
    match v {
        Value::List(l) => Ok(Value::Boolean(l.len() == 0)),
        _ => Ok(Value::Boolean(false))
    }
}

fn native_list(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    let res: Result<Vec<Value>, RuntimeError> = args.iter().map(|n| evaluate_value(n, env.clone())).collect();
    let elements = try!(res);
    Ok(Value::List(elements))
}

fn native_car(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("Must supply exactly one argument to car: {:?}", args);
    }
    let v = try!(evaluate_value(&args[0], env.clone()));
    match v {
        Value::List(mut l) => {
            if l.len() > 0 {
                Ok(l.remove(0))
            } else {
                runtime_error!("Can't run car on an empty list")
            }
        }
        _ => runtime_error!("Must supply a list to car")
    }
}

fn native_cdr(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("Must supply exactly one argument to cdr: {:?}", args);
    }
    let v = try!(evaluate_value(&args[0], env.clone()));
    match v {
        Value::List(mut l) => {
            if l.len() > 0 {
                l.remove(0);
                Ok(Value::List(l))
            } else {
                runtime_error!("Can't run cdr on an empty list")
            }
        }
        _ => runtime_error!("Must supply a list to cdr")
    }
}

fn native_cons(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("Must supply exactly two arguments to cons: {:?}", args);
    }

    let first = try!(evaluate_value(&args[0], env.clone()));
    let second = try!(evaluate_value(&args[1], env.clone()));
    match second {
        Value::List(elements) => {
            let mut new_elements = vec![first];
            for e in elements.into_iter() {
                new_elements.push(e);
            }
            return Ok(Value::List(new_elements))
        }
        _ => runtime_error!("Second argument to cons must be a list: {:?}", second)
    }
}

fn native_append(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("Must supply exactly two arguments to append: {:?}", args);
    }

    let first = try!(evaluate_value(&args[0], env.clone()));
    let second = try!(evaluate_value(&args[1], env.clone()));
    let mut first_vec = match first {
        Value::List(elements) => elements,
        _ => runtime_error!("First argument to append must be a list: {:?}", first)
    };
    let second_vec = match second {
        Value::List(elements) => elements,
        _ => runtime_error!("Second argument to append must be a list: {:?}", second)
    };
    for e in second_vec.into_iter() {
        first_vec.push(e);
    }
    return Ok(Value::List(first_vec))
}

fn native_quote(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("Must supply exactly one argument to quote: {:?}", args);
    }
    quote_value(&args[0], false, env.clone())
}

fn native_quasiquote(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("Must supply exactly one argument to quasiquote: {:?}", args);
    }
    quote_value(&args[0], true, env.clone())
}

fn native_error(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("Must supply exactly one arguments to error: {:?}", args);
    }
    let e = try!(evaluate_value(&args[0], env.clone()));
    runtime_error!("{:?}", e);
}

fn native_apply(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("Must supply exactly two arguments to apply: {:?}", args);
    }
    let func = match try!(evaluate_value(&args[0], env.clone())) {
        Value::Procedure(func) => func,
        _ => runtime_error!("First argument to apply must be a procedure: {:?}", args)
    };
    let func_args = match try!(evaluate_value(&args[1], env.clone())) {
        Value::List(func_args) => func_args,
        _ => runtime_error!("Second argument to apply must be a list of arguments: {:?}", args)
    };
    apply_function(&func, &func_args, env.clone())
}

fn native_eval(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("Must supply exactly one argument to eval: {:?}", args);
    }

    // eval is basically just a double-evaluation -- the first evaluate returns the data using the local envirnoment, and the second evaluate evaluates the data as code using the global environment
    let res = try!(evaluate_value(&args[0], env.clone()));
    evaluate_value(&res, Environment::get_root(env))
}

fn native_write(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("Must supply exactly one argument to write: {:?}", args);
    }

    let val = try!(evaluate_value(&args[0], env.clone()));
    print!("{:?}", val);
    Ok(null!())
}

fn native_display(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("Must supply exactly one argument to display: {:?}", args);
    }

    let val = try!(evaluate_value(&args[0], env.clone()));
    print!("{}", val);
    Ok(null!())
}

fn native_displayln(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("Must supply exactly one argument to displayln: {:?}", args);
    }

    let val = try!(evaluate_value(&args[0], env.clone()));
    println!("{}", val);
    Ok(null!())
}

fn native_print(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("Must supply exactly one argument to print: {:?}", args);
    }

    let val = try!(evaluate_value(&args[0], env.clone()));
    match val {
        Value::Symbol(_) | Value::List(_) => print!("'{:?}", val),
        _ => print!("{:?}", val)
    }
    Ok(null!())
}

#[allow(unused_variables)]
fn native_newline(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 0 {
        runtime_error!("Must supply exactly zero arguments to newline: {:?}", args);
    }
    println!("");
    Ok(null!())
}

#[test]
fn test_interpreter_global_variables() {
    assert_eq!(new().run(&[Node::List(vec![Node::Identifier("define".to_string()), Node::Identifier("x".to_string()), Node::Integer(2)]), Node::List(vec![Node::Identifier("+".to_string()), Node::Identifier("x".to_string()), Node::Identifier("x".to_string()), Node::Identifier("x".to_string())])]).unwrap(),
               Value::Integer(6));
}

#[test]
fn test_interpreter_global_function_definition() {
    assert_eq!(new().run(&[Node::List(vec![Node::Identifier("define".to_string()), Node::Identifier("double".to_string()), Node::List(vec![Node::Identifier("lambda".to_string()), Node::List(vec![Node::Identifier("x".to_string())]), Node::List(vec![Node::Identifier("+".to_string()), Node::Identifier("x".to_string()), Node::Identifier("x".to_string())])])]), Node::List(vec![Node::Identifier("double".to_string()), Node::Integer(8)])]).unwrap(),
               Value::Integer(16));
}
