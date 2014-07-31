use parser::*;

use std::fmt;
use std::result;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

pub fn new() -> Interpreter {
    Interpreter::new()
}

#[deriving(Clone)]
pub struct Interpreter {
    root: Rc<RefCell<Environment>>
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter { root: Environment::new_root() }
    }

    pub fn run(&self, nodes: &[Node]) -> Result<Value, RuntimeError> {
        let values = Value::from_nodes(nodes);
        evaluate_values(values.as_slice(), self.root.clone())
    }
}

#[deriving(PartialEq, Clone)]
pub enum Value {
    VSymbol(String),
    VInteger(int),
    VBoolean(bool),
    VString(String),
    VList(Vec<Value>),
    VProcedure(Function),
}

// null == empty list
macro_rules! null { () => (VList(vec![])) }

pub enum Function {
    NativeFunction(ValueOperation),
    SchemeFunction(Vec<String>, Vec<Value>, Rc<RefCell<Environment>>),
}

// type signature for all native functions
type ValueOperation = fn(&[Value], Rc<RefCell<Environment>>) -> Result<Value, RuntimeError>;

impl fmt::Show for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

impl Value {
    fn from_nodes(nodes: &[Node]) -> Vec<Value> {
        nodes.iter().map(Value::from_node).collect()
    }

    fn from_node(node: &Node) -> Value {
        match *node {
            NIdentifier(ref val) => VSymbol(val.clone()),
            NInteger(val) => VInteger(val),
            NBoolean(val) => VBoolean(val),
            NString(ref val) => VString(val.clone()),
            NList(ref nodes) => VList(Value::from_nodes(nodes.as_slice()))
        }
    }

    fn to_str(&self) -> String {
        match self {
            &VSymbol(_) => format!("'{}", self.to_raw_str()),
            &VList(_) => format!("'{}", self.to_raw_str()),
            _ => self.to_raw_str()
        }
    }

    fn to_raw_str(&self) -> String {
        match *self {
            VSymbol(ref val) => format!("{}", val),
            VInteger(val) => format!("{}", val),
            VBoolean(val) => format!("#{}", if val { "t" } else { "f" }),
            VString(ref val) => format!("\"{}\"", val),
            VList(ref val) => {
                let mut s = String::new();
                let mut first = true;
                for n in val.iter() {
                    if first {
                        first = false;
                    } else {
                        s = s.append(" ");
                    }
                    s = s.append(n.to_raw_str().as_slice());
                }
                format!("({})", s)
            }
            VProcedure(_) => format!("#<procedure>")
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
            NativeFunction(ref func) => NativeFunction(*func),
            SchemeFunction(ref a, ref b, ref env) => SchemeFunction(a.clone(), b.clone(), env.clone())
        }
    }
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
    parent: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Value>,
}

impl Environment {
    fn new_root() -> Rc<RefCell<Environment>> {
        let mut env = Environment { parent: None, values: HashMap::new() };
        for item in PREDEFINED_FUNCTIONS.iter() {
            let (name, ref func) = *item;
            env.set(name.to_str(), VProcedure(func.clone()));
        }
        Rc::new(RefCell::new(env))
    }

    fn new_child(parent: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        let env = Environment { parent: Some(parent), values: HashMap::new() };
        Rc::new(RefCell::new(env))
    }

    fn set(&mut self, key: String, value: Value) {
        self.values.insert(key, value);
    }

    fn has(&self, key: &String) -> bool {
        self.values.contains_key(key)
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

    fn get_root(env: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        match env.borrow().parent {
            Some(ref parent) => Environment::get_root(parent.clone()),
            None => env.clone()
        }
    }
}

fn evaluate_values(values: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    result::fold(values.iter().map(|v| evaluate_value(v, env.clone())), null!(), |_, r| r)
}

fn evaluate_value(value: &Value, env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    match value {
        &VSymbol(ref v) => {
            match env.borrow().get(v) {
                Some(val) => Ok(val),
                None => runtime_error!("Identifier not found: {}", value)
            }
        },
        &VInteger(v) => Ok(VInteger(v)),
        &VBoolean(v) => Ok(VBoolean(v)),
        &VString(ref v) => Ok(VString(v.clone())),
        &VList(ref vec) => {
            if vec.len() > 0 {
                evaluate_expression(vec, env.clone())
            } else {
                Ok(null!())
            }
        },
        &VProcedure(ref v) => Ok(VProcedure(v.clone()))
    }
}

fn quote_value(value: &Value, quasi: bool, env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    match value {
        &VSymbol(ref v) => Ok(VSymbol(v.clone())),
        &VInteger(v) => Ok(VInteger(v)),
        &VBoolean(v) => Ok(VBoolean(v)),
        &VString(ref v) => Ok(VString(v.clone())),
        &VList(ref vec) => {
            // check if we are unquoting inside a quasiquote
            if quasi && vec.len() > 0 && *vec.get(0) == VSymbol("unquote".to_str()) {
                if vec.len() != 2 {
                    runtime_error!("Must supply exactly one argument to unquote: {}", vec);
                }
                evaluate_value(vec.get(1), env.clone())
            } else {
                let res = try!(result::collect(vec.iter().map(|v| quote_value(v, quasi, env.clone()))));
                Ok(VList(res))
            }
        },
        &VProcedure(ref v) => Ok(VProcedure(v.clone()))
    }
}

fn evaluate_expression(values: &Vec<Value>, env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if values.len() == 0 {
        runtime_error!("Can't evaluate an empty expression: {}", values);
    }
    let first = try!(evaluate_value(values.get(0), env.clone()));
    match first {
        VProcedure(f) => apply_function(&f, values.tailn(1), env.clone()),
        _ => runtime_error!("First element in an expression must be a procedure: {}", first)
    }
}

fn apply_function(func: &Function, args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    match func {
        &NativeFunction(nativeFn) => {
            nativeFn(args, env)
        },
        &SchemeFunction(ref argNames, ref body, ref funcEnv) => {
            if argNames.len() != args.len() {
                runtime_error!("Must supply exactly {} arguments to function: {}", argNames.len(), args);
            }

            // create a new, child environment for the procedure and define the arguments as local variables
            let procEnv = Environment::new_child(funcEnv.clone());
            for (name, arg) in argNames.iter().zip(args.iter()) {
                let val = try!(evaluate_value(arg, env.clone()));
                procEnv.borrow_mut().set(name.clone(), val);
            }

            Ok(try!(evaluate_values(body.as_slice(), procEnv)))
        }
    }
}

static PREDEFINED_FUNCTIONS: &'static[(&'static str, Function)] = &[
    ("define", NativeFunction(native_define)),
    ("set!", NativeFunction(native_set)),
    ("lambda", NativeFunction(native_lambda)),
    ("λ", NativeFunction(native_lambda)),
    ("if", NativeFunction(native_if)),
    ("+", NativeFunction(native_plus)),
    ("-", NativeFunction(native_minus)),
    ("and", NativeFunction(native_and)),
    ("or", NativeFunction(native_or)),
    ("list", NativeFunction(native_list)),
    ("quote", NativeFunction(native_quote)),
    ("quasiquote", NativeFunction(native_quasiquote)),
    ("error", NativeFunction(native_error)),
    ("apply", NativeFunction(native_apply)),
    ("eval", NativeFunction(native_eval)),
];

fn native_define(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("Must supply exactly two arguments to define: {}", args);
    }
    let name = match *args.get(0).unwrap() {
        VSymbol(ref x) => x,
        _ => runtime_error!("Unexpected value for name in define: {}", args)
    };
    let alreadyDefined = env.borrow().has(name);
    if !alreadyDefined {
        let val = try!(evaluate_value(args.get(1).unwrap(), env.clone()));
        env.borrow_mut().set(name.clone(), val);
        Ok(null!())
    } else {
        runtime_error!("Duplicate define: {}", name)
    }
}

fn native_set(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("Must supply exactly two arguments to set!: {}", args);
    }
    let name = match *args.get(0).unwrap() {
        VSymbol(ref x) => x,
        _ => runtime_error!("Unexpected value for name in set!: {}", args)
    };
    let alreadyDefined = env.borrow().has(name);
    if alreadyDefined {
        let val = try!(evaluate_value(args.get(1).unwrap(), env.clone()));
        env.borrow_mut().set(name.clone(), val);
        Ok(null!())
    } else {
        runtime_error!("Can't set! an undefined variable: {}", name)
    }
}

fn native_lambda(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() < 2 {
        runtime_error!("Must supply at least two arguments to lambda: {}", args);
    }
    let argNames = match *args.get(0).unwrap() {
        VList(ref list) => {
            try!(result::collect(list.iter().map(|i| match *i {
                VSymbol(ref s) => Ok(s.clone()),
                _ => runtime_error!("Unexpected argument in lambda arguments: {}", i)
            })))
        }
        _ => runtime_error!("Unexpected value for arguments in lambda: {}", args)
    };
    let body = Vec::from_slice(args.tailn(1));
    Ok(VProcedure(SchemeFunction(argNames, body, env.clone())))
}

fn native_if(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 3 {
        runtime_error!("Must supply exactly three arguments to if: {}", args);
    }
    let condition = try!(evaluate_value(args.get(0).unwrap(), env.clone()));
    match condition {
        VBoolean(false) => evaluate_value(args.get(2).unwrap(), env.clone()),
        _ => evaluate_value(args.get(1).unwrap(), env.clone())
    }
}

fn native_plus(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() < 2 {
        runtime_error!("Must supply at least two arguments to +: {}", args);
    }
    let mut sum = 0;
    for n in args.iter() {
        let v = try!(evaluate_value(n, env.clone()));
        match v {
            VInteger(x) => sum += x,
            _ => runtime_error!("Unexpected value during +: {}", n)
        };
    };
    Ok(VInteger(sum))
}

fn native_minus(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("Must supply exactly two arguments to -: {}", args);
    }
    let l = try!(evaluate_value(args.get(0).unwrap(), env.clone()));
    let r = try!(evaluate_value(args.get(1).unwrap(), env.clone()));
    let mut result = match l {
        VInteger(x) => x,
        _ => runtime_error!("Unexpected value during -: {}", args)
    };
    result -= match r {
        VInteger(x) => x,
        _ => runtime_error!("Unexpected value during -: {}", args)
    };
    Ok(VInteger(result))
}

fn native_and(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    let mut res = VBoolean(true);
    for n in args.iter() {
        let v = try!(evaluate_value(n, env.clone()));
        match v {
            VBoolean(false) => return Ok(VBoolean(false)),
            _ => res = v
        }
    }
    Ok(res)
}

fn native_or(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    for n in args.iter() {
        let v = try!(evaluate_value(n, env.clone()));
        match v {
            VBoolean(false) => (),
            _ => return Ok(v)
        }
    }
    Ok(VBoolean(false))
}

fn native_list(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    let elements = try!(result::collect(args.iter().map(|n| evaluate_value(n, env.clone()))));
    Ok(VList(elements))
}

fn native_quote(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("Must supply exactly one argument to quote: {}", args);
    }
    quote_value(args.get(0).unwrap(), false, env.clone())
}

fn native_quasiquote(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("Must supply exactly one argument to quasiquote: {}", args);
    }
    quote_value(args.get(0).unwrap(), true, env.clone())
}

fn native_error(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("Must supply exactly one arguments to error: {}", args);
    }
    let e = try!(evaluate_value(args.get(0).unwrap(), env.clone()));
    runtime_error!("{}", e);
}

fn native_apply(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        runtime_error!("Must supply exactly two arguments to apply: {}", args);
    }
    let func = match try!(evaluate_value(args.get(0).unwrap(), env.clone())) {
        VProcedure(func) => func,
        _ => runtime_error!("First argument to apply must be a procedure: {}", args)
    };
    let funcArgs = match try!(evaluate_value(args.get(1).unwrap(), env.clone())) {
        VList(funcArgs) => funcArgs,
        _ => runtime_error!("Second argument to apply must be a list of arguments: {}", args)
    };
    apply_function(&func, funcArgs.as_slice(), env.clone())
}

fn native_eval(args: &[Value], env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        runtime_error!("Must supply exactly one argument to eval: {}", args);
    }

    // eval is basically just a double-evaluation -- the first evaluate returns the data using the local envirnoment, and the second evaluate evaluates the data as code using the global environment
    let res = try!(evaluate_value(args.get(0).unwrap(), env.clone()));
    evaluate_value(&res, Environment::get_root(env))
}

#[test]
fn test_global_variables() {
    assert_eq!(new().run([NList(vec![NIdentifier("define".to_str()), NIdentifier("x".to_str()), NInteger(2)]), NList(vec![NIdentifier("+".to_str()), NIdentifier("x".to_str()), NIdentifier("x".to_str()), NIdentifier("x".to_str())])]).unwrap(),
               VInteger(6));
}

#[test]
fn test_global_function_definition() {
    assert_eq!(new().run([NList(vec![NIdentifier("define".to_str()), NIdentifier("double".to_str()), NList(vec![NIdentifier("lambda".to_str()), NList(vec![NIdentifier("x".to_str())]), NList(vec![NIdentifier("+".to_str()), NIdentifier("x".to_str()), NIdentifier("x".to_str())])])]), NList(vec![NIdentifier("double".to_str()), NInteger(8)])]).unwrap(),
               VInteger(16));
}
