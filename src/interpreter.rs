use std::{cell::RefCell, collections::HashMap, fmt::Display, io::Write, rc::Rc};

use crate::{
    ast::{Expr, Program, Stmt},
    lexer::{Line, Token},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    name: String,
    methods: HashMap<String, Value>,
}

const INITIALIZER_NAME: &str = "init";

impl Class {
    fn new(name: String, methods: HashMap<String, Value>) -> Self {
        Self { name, methods }
    }

    fn method(&self, name: &str) -> Option<&Value> {
        self.methods.get(name)
    }

    fn init_arity(&self, line: Line) -> usize {
        match self.method(INITIALIZER_NAME) {
            Some(i) => i.arity(line).unwrap(),
            None => 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instance {
    class: Rc<Class>,
    fields: HashMap<String, Value>,
}

fn get_property(instance: Rc<RefCell<Instance>>, prop: &str) -> Option<Value> {
    let v = instance.borrow_mut().fields.get(prop).cloned();

    if v.is_some() {
        return v;
    }

    instance
        .borrow_mut()
        .class
        .method(prop)
        .map(|method| method.bind(instance.clone()))
}

impl Instance {
    fn new(class: Rc<Class>) -> Self {
        let fields = HashMap::new();
        Self { class, fields }
    }

    fn set(&mut self, name: String, value: Value) {
        self.fields.insert(name, value);
    }

    fn class_name(&self) -> String {
        self.class.name.clone()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    name: String,
    params: Vec<Token>,
    stmts: Vec<Stmt>,
    closure: ShareableEnv,
    is_initializer: bool,
}

impl Function {
    fn new(
        name: impl Into<String>,
        params: Vec<Token>,
        stmts: Vec<Stmt>,
        closure: ShareableEnv,
        is_initializer: bool,
    ) -> Self {
        Self {
            name: name.into(),
            params,
            stmts,
            closure,
            is_initializer,
        }
    }

    fn call(
        &self,
        i: &mut Interpreter<impl Write>,
        args: &mut Vec<Value>,
        line: Line,
    ) -> Result<Value> {
        let mut local_env = Env::new(Some(self.closure.clone()));

        for param in self.params.iter() {
            let v = args.remove(0);
            local_env.define(param.to_string(), v);
        }

        match i.interpret_stmts(&self.stmts, Rc::new(RefCell::new(local_env)))? {
            ControlFlow::Return(v) => Ok(v),
            _ => {
                if self.is_initializer {
                    self.closure
                        .borrow()
                        .get_at_depth("this".to_string(), 0)
                        .map_err(|msg| RuntimeError::new(msg, line))
                } else {
                    Ok(Value::Nil)
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Str(String),
    Bool(bool),
    Fun(Function),
    Class(Rc<Class>),
    Instance(Rc<RefCell<Instance>>),
    Nil,
}

impl Value {
    fn arity(&self, line: Line) -> Result<usize> {
        match self {
            Value::Fun(fun) => Ok(fun.params.len()),
            Value::Class(class) => Ok(class.init_arity(line)),
            t => error(format!("{} is not a function", t), line),
        }
    }

    fn call<Out: Write>(
        &self,
        i: &mut Interpreter<Out>,
        mut args: Vec<Value>,
        line: Line,
    ) -> Result<Value> {
        match self {
            Value::Fun(fun) => fun.call(i, &mut args, line),
            Value::Class(class) => {
                let instance = Rc::new(RefCell::new(Instance::new(class.clone())));
                if let Some(init) = class.method(INITIALIZER_NAME) {
                    init.bind(instance.clone()).call(i, args, line)?;
                }

                Ok(Value::Instance(instance))
            }
            t => error(format!("{} is not a function", t), line),
        }
    }

    fn equals(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Number(l), Value::Number(r)) => l == r,
            (Value::Str(l), Value::Str(r)) => l == r,
            (Value::Bool(l), Value::Bool(r)) => l == r,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }

    fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Nil => false,
            _ => true,
        }
    }

    fn is_falsey(&self) -> bool {
        !self.is_truthy()
    }

    fn bind(&self, this: Rc<RefCell<Instance>>) -> Self {
        if let Value::Fun(fun) = self {
            let mut env = Env::new(Some(fun.closure.clone()));
            env.define("this".to_string(), Value::Instance(this));

            let bound_fun = Function::new(
                fun.name.clone(),
                fun.params.to_vec(),
                fun.stmts.to_vec(),
                Rc::new(RefCell::new(env)),
                fun.is_initializer,
            );

            return Value::Fun(bound_fun);
        }

        panic!("attempted to bind this to {:?}", self);
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => n.fmt(f),
            Value::Str(s) => write!(f, "{:?}", s),
            Value::Bool(b) => b.fmt(f),
            Value::Nil => write!(f, "nil"),
            Value::Fun(fun) => write!(f, "<fn {}>", fun.name),
            Value::Class(class) => write!(f, "{}", class.name),
            Value::Instance(instance) => {
                write!(f, "{} instance", instance.borrow_mut().class_name())
            }
        }
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    pub report: String,
    pub line: Line,
}

impl RuntimeError {
    fn new(report: impl Into<String>, line: Line) -> Self {
        Self {
            report: report.into(),
            line,
        }
    }
}

type Result<T> = core::result::Result<T, RuntimeError>;

fn error<T>(report: impl Into<String>, line: Line) -> Result<T> {
    Err(RuntimeError::new(report, line))
}

type ShareableEnv = Rc<RefCell<Env>>;

#[derive(Debug, PartialEq)]
pub struct Env {
    pub enclosing: Option<ShareableEnv>,
    pub values: HashMap<String, Value>,
}

impl Env {
    pub fn new(enclosing: Option<ShareableEnv>) -> Self {
        Self {
            enclosing,
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get_at_depth(&self, name: String, depth: u8) -> core::result::Result<Value, String> {
        if depth == 0 {
            self.get(name)
        } else if let Some(e) = &self.enclosing {
            e.borrow_mut().get_at_depth(name, depth - 1)
        } else {
            Err(format!("variable lookup failed for {}", &name))
        }
    }

    pub fn get(&self, name: String) -> core::result::Result<Value, String> {
        if let Some(v) = self.values.get(&name) {
            Ok(v.clone())
        } else if let Some(e) = &self.enclosing {
            e.borrow_mut().get(name)
        } else {
            Err(format!("undefined variable \"{}\"", &name))
        }
    }

    pub fn assign(&mut self, name: String, value: Value) -> core::result::Result<Value, String> {
        if self.values.get(&name).is_some() {
            self.values.insert(name, value.clone());
            Ok(value)
        } else if let Some(e) = &self.enclosing {
            e.borrow_mut().assign(name, value)
        } else {
            Err(format!("undefined variable \"{}\"", &name))
        }
    }
}

pub enum ControlFlow {
    Continue,
    Break,
    Return(Value),
}

#[derive(Debug)]
pub struct Interpreter<Out: Write> {
    out: Out,
    pub env: ShareableEnv,
}

impl<Out: Write> Interpreter<Out> {
    pub fn new(out: Out) -> Self {
        let env = Rc::new(RefCell::new(Env::new(None)));
        Self { out, env }
    }

    pub fn interpret(&mut self, program: &Program) -> Result<ControlFlow> {
        self.interpret_stmts(&program.stmts, self.env.clone())
    }

    fn interpret_stmts(&mut self, stmts: &[Stmt], env: ShareableEnv) -> Result<ControlFlow> {
        for stmt in stmts {
            match self.interpret_stmt(stmt, env.clone())? {
                ControlFlow::Break => return Ok(ControlFlow::Break),
                ControlFlow::Return(v) => return Ok(ControlFlow::Return(v)),
                _ => (),
            }
        }

        Ok(ControlFlow::Continue)
    }

    fn interpret_stmt(&mut self, stmt: &Stmt, env: ShareableEnv) -> Result<ControlFlow> {
        match stmt {
            Stmt::Expr(expr) => {
                self.interpret_expr(expr, env)?;
                Ok(ControlFlow::Continue)
            }
            Stmt::Print(expr) => {
                self.interpret_print(expr, env)?;
                Ok(ControlFlow::Continue)
            }
            Stmt::VarDecl(name, expr) => {
                self.interpret_var_decl(name, expr, env)?;
                Ok(ControlFlow::Continue)
            }
            Stmt::Block(stmts) => self.interpret_block(stmts, env),
            Stmt::If(condition, then_branch, else_branch) => {
                self.interpret_if(condition, env, then_branch, else_branch)
            }
            Stmt::While(condition, stmt) => self.interpret_while(condition, stmt, env),
            Stmt::Break => Ok(ControlFlow::Break),
            Stmt::FunDecl(name, params, stmts) => self.interpret_fun_decl(name, params, stmts, env),
            Stmt::Return(_, expr) => self.interpret_return(expr, env),
            Stmt::Class(name, methods) => self.interpret_class_decl(name, methods, env),
        }
    }

    fn interpret_return(&mut self, expr: &Expr, env: ShareableEnv) -> Result<ControlFlow> {
        Ok(ControlFlow::Return(self.interpret_expr(expr, env)?))
    }

    fn interpret_while(
        &mut self,
        condition: &Expr,
        stmt: &Stmt,
        env: ShareableEnv,
    ) -> Result<ControlFlow> {
        while self.interpret_expr(condition, env.clone())?.is_truthy() {
            if let ControlFlow::Break = self.interpret_stmt(stmt, env.clone())? {
                break;
            }
        }

        Ok(ControlFlow::Continue)
    }

    fn interpret_if(
        &mut self,
        condition: &Expr,
        env: ShareableEnv,
        then_branch: &Stmt,
        else_branch: &Option<Box<Stmt>>,
    ) -> Result<ControlFlow> {
        let value = self.interpret_expr(condition, env.clone())?;

        if value.is_truthy() {
            self.interpret_stmt(then_branch, env)
        } else if let Some(b) = else_branch {
            self.interpret_stmt(b, env)
        } else {
            Ok(ControlFlow::Continue)
        }
    }

    fn interpret_block(&mut self, stmts: &[Stmt], env: ShareableEnv) -> Result<ControlFlow> {
        let local_env = Env::new(Some(env));
        self.interpret_stmts(stmts, Rc::new(RefCell::new(local_env)))
    }

    fn interpret_var_decl(&mut self, name: &Token, expr: &Expr, env: ShareableEnv) -> Result<()> {
        if let Token::Identifier(_, name) = name {
            let init_value = self.interpret_expr(expr, env.clone())?;
            env.borrow_mut().define(name.into(), init_value);
        } else {
            return error("invalid variable token", name.line());
        };

        Ok(())
    }

    fn interpret_print(&mut self, expr: &Expr, env: ShareableEnv) -> Result<()> {
        let value = self.interpret_expr(expr, env)?;
        self.out
            .write_all(format!("{}\n", value).as_bytes())
            .unwrap();
        self.out.flush().unwrap();
        Ok(())
    }

    pub fn interpret_expr(&mut self, expr: &Expr, env: ShareableEnv) -> Result<Value> {
        match expr {
            Expr::Number(n) => Ok(Value::Number(*n)),
            Expr::Str(s) => Ok(Value::Str(s.into())),
            Expr::Bool(b) => Ok(Value::Bool(*b)),
            Expr::Nil => Ok(Value::Nil),
            Expr::Grouping(expr) => self.interpret_expr(expr, env),
            Expr::Unary(op, expr) => self.interpret_unary_expr(op, expr, env),
            Expr::Binary(lhs, op, rhs) => self.interpret_binary_expr(lhs, op, rhs, env),
            Expr::Ternary(condition, conclusion, alternate) => {
                self.interpret_ternary_expr(condition, conclusion, alternate, env)
            }
            Expr::Var(name, depth) => self.interpret_var(name, *depth, env),
            Expr::Assign(lhs, rhs) => self.interpret_assign(lhs, rhs, env),
            Expr::Call(callee, t, args) => self.interpret_call(callee, t.line(), args, env),
            Expr::AnonFunDecl(params, body) => self.interpret_anon_fun_decl(params, body, env),
            Expr::Get(object, name) => self.interpret_get(object, name, env),
            Expr::Set(object, name, value) => self.interpret_set(object, name, value, env),
            Expr::This(token) => self.interpret_var(token, Some(0), env),
        }
    }

    fn interpret_get(&mut self, object: &Expr, name: &Token, env: ShareableEnv) -> Result<Value> {
        match self.interpret_expr(object, env)? {
            Value::Instance(instance) => match get_property(instance, &name.to_string()) {
                Some(v) => Ok(v),
                None => error(format!("undefined property {}", name), name.line()),
            },
            _ => error("only instances have properties", name.line()),
        }
    }

    fn interpret_set(
        &mut self,
        object: &Expr,
        name: &Token,
        value: &Expr,
        env: ShareableEnv,
    ) -> Result<Value> {
        match self.interpret_expr(object, env.clone())? {
            Value::Instance(instance) => {
                let v = self.interpret_expr(value, env)?;
                instance.borrow_mut().set(name.to_string(), v.clone());
                Ok(v)
            }
            _ => error("only instances have fields", name.line()),
        }
    }

    fn interpret_anon_fun_decl(
        &mut self,
        params: &[Token],
        body: &[Stmt],
        env: ShareableEnv,
    ) -> Result<Value> {
        let fun = Function::new("<anon fn>", params.to_vec(), body.to_vec(), env, false);
        Ok(Value::Fun(fun))
    }

    fn interpret_call(
        &mut self,
        callee: &Expr,
        line: Line,
        args: &[Expr],
        env: ShareableEnv,
    ) -> Result<Value> {
        let callable = self.interpret_expr(callee, env.clone())?;

        let mut arguments = Vec::new();
        for arg_expr in args {
            arguments.push(self.interpret_expr(arg_expr, env.clone())?);
        }

        if arguments.len() != callable.arity(line)? {
            return error(
                format!(
                    "{} expected {} arguments, provided {}",
                    callable,
                    callable.arity(line)?,
                    arguments.len()
                ),
                line,
            );
        }

        callable.call(self, arguments, line)
    }

    fn interpret_assign(&mut self, lhs: &Token, rhs: &Expr, env: ShareableEnv) -> Result<Value> {
        match lhs {
            Token::Identifier(line, name) => {
                let value = self.interpret_expr(rhs, env.clone())?;
                env.borrow_mut()
                    .assign(name.into(), value)
                    .map_err(|msg| RuntimeError::new(msg, *line))
            }
            t => error(format!("invalid LHS for assignment \"{}\"", t), t.line()),
        }
    }

    fn interpret_unary_expr(
        &mut self,
        op: &Token,
        expr: &Expr,
        env: ShareableEnv,
    ) -> Result<Value> {
        let value = self.interpret_expr(expr, env)?;

        match op {
            Token::Minus(_) => {
                if let Value::Number(n) = value {
                    Ok(Value::Number(-n))
                } else {
                    error("unary \"-\" can only be applied to numbers", op.line())
                }
            }
            Token::Bang(_) => {
                if value.is_truthy() {
                    Ok(Value::Bool(false))
                } else {
                    Ok(Value::Bool(true))
                }
            }
            _ => error(format!("invalid unary operator \"{}\"", op), op.line()),
        }
    }

    fn interpret_binary_expr(
        &mut self,
        lhs: &Expr,
        op: &Token,
        rhs: &Expr,
        env: ShareableEnv,
    ) -> Result<Value> {
        let left = self.interpret_expr(lhs, env.clone())?;

        // Handle short-circuiting logical operators
        match op {
            Token::And(_) => {
                if left.is_falsey() {
                    return Ok(left);
                } else {
                    return self.interpret_expr(rhs, env);
                }
            }
            Token::Or(_) => {
                if left.is_truthy() {
                    return Ok(left);
                } else {
                    return self.interpret_expr(rhs, env);
                }
            }
            _ => (),
        }

        let right = self.interpret_expr(rhs, env)?;

        match op {
            Token::Comma(_) => Ok(right),
            Token::Minus(_) => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
                _ => error("binary \"-\" can only be applied to numbers", op.line()),
            },
            Token::Plus(_) => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
                (Value::Str(l), Value::Str(r)) => Ok(Value::Str(format!("{}{}", l, r))),
                _ => error(
                    "binary \"+\" can only be applied to numbers and strings",
                    op.line(),
                ),
            },
            Token::Star(_) => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),
                _ => error("binary \"*\" can only be applied to numbers", op.line()),
            },
            Token::Slash(_) => match (left, right) {
                (Value::Number(l), Value::Number(r)) => {
                    if r == 0.0 {
                        error("divided by 0", op.line())
                    } else {
                        Ok(Value::Number(l / r))
                    }
                }
                _ => error("binary \"/\" can only be applied to numbers", op.line()),
            },
            Token::EqualEqual(_) => Ok(Value::Bool(left.equals(&right))),
            Token::BangEqual(_) => Ok(Value::Bool(!left.equals(&right))),
            Token::Greater(_) => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l > r)),
                _ => error("binary \">\" can only be applied to numbers", op.line()),
            },
            Token::GreaterEqual(_) => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l >= r)),
                _ => error("binary \">=\" can only be applied to numbers", op.line()),
            },
            Token::Less(_) => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l < r)),
                _ => error("binary \"<\" can only be applied to numbers", op.line()),
            },
            Token::LessEqual(_) => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l <= r)),
                _ => error("binary \"<=\" can only be applied to numbers", op.line()),
            },
            _ => error(format!("invalid binary operator \"{}\"", op), op.line()),
        }
    }

    fn interpret_ternary_expr(
        &mut self,
        condition_expr: &Expr,
        conclusion_expr: &Expr,
        alternate_expr: &Expr,
        env: ShareableEnv,
    ) -> Result<Value> {
        let condition = self.interpret_expr(condition_expr, env.clone())?;

        if condition.is_truthy() {
            self.interpret_expr(conclusion_expr, env)
        } else {
            self.interpret_expr(alternate_expr, env)
        }
    }

    fn interpret_var(&self, name: &Token, depth: Option<u8>, env: ShareableEnv) -> Result<Value> {
        let resolved_var = if let Some(d) = depth {
            env.borrow_mut().get_at_depth(name.to_string(), d)
        } else {
            self.env.borrow_mut().get(name.to_string())
        };

        resolved_var.map_err(|msg| RuntimeError::new(msg, name.line()))
    }

    fn interpret_fun_decl(
        &self,
        name: &Token,
        params: &[Token],
        stmts: &[Stmt],
        env: ShareableEnv,
    ) -> Result<ControlFlow> {
        let name = name.to_string();
        let fun = Function::new(
            name.clone(),
            params.to_vec(),
            stmts.to_vec(),
            env.clone(),
            false,
        );
        env.borrow_mut().define(name, Value::Fun(fun));
        Ok(ControlFlow::Continue)
    }

    fn interpret_class_decl(
        &self,
        name: &Token,
        methods: &[Stmt],
        env: ShareableEnv,
    ) -> Result<ControlFlow> {
        let class_name = name.to_string();
        env.borrow_mut().define(class_name.clone(), Value::Nil);

        let mut methods_map = HashMap::new();
        for method in methods {
            if let Stmt::FunDecl(name, params, body) = method {
                let fun = Function::new(
                    name.to_string(),
                    params.to_vec(),
                    body.to_vec(),
                    env.clone(),
                    name.to_string() == INITIALIZER_NAME,
                );

                methods_map.insert(fun.name.clone(), Value::Fun(fun));
            }
        }

        let class = Value::Class(Rc::new(Class::new(class_name.clone(), methods_map)));

        env.borrow_mut()
            .assign(class_name.clone(), class)
            .map_err(|_| {
                RuntimeError::new(
                    format!("class {} is already defined", class_name),
                    name.line(),
                )
            })?;

        Ok(ControlFlow::Continue)
    }
}

#[cfg(test)]
mod tests;
