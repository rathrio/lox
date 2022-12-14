use std::{
    borrow::Cow,
    cell::RefCell,
    fmt::Display,
    io::Write,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use fnv::FnvHashMap as HashMap;

use crate::{
    ast::{Expr, Program, Stmt},
    lexer::{Line, Token},
};

const INITIALIZER_NAME: &str = "init";

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    name: String,
    superclass: Option<Rc<Class>>,
    methods: HashMap<String, Value>,
    class_methods: HashMap<String, Value>,
}

impl Class {
    fn new(
        name: String,
        superclass: Option<Rc<Class>>,
        methods: HashMap<String, Value>,
        class_methods: HashMap<String, Value>,
    ) -> Self {
        Self {
            name,
            superclass,
            methods,
            class_methods,
        }
    }

    fn method(&self, name: &str) -> Option<&Value> {
        self.methods.get(name).or_else(|| {
            self.superclass
                .as_ref()
                .map(|class| class.method(name))
                .unwrap_or(None)
        })
    }

    fn init_arity(&self, line: Line) -> usize {
        match self.method(INITIALIZER_NAME) {
            Some(i) => i.arity(line).unwrap(),
            None => 0,
        }
    }

    fn class_method(&self, name: &str) -> Option<&Value> {
        self.class_methods.get(name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instance {
    class: Rc<Class>,
    fields: HashMap<String, Value>,
}

impl Instance {
    fn new(class: Rc<Class>) -> Self {
        let fields = HashMap::default();
        Self { class, fields }
    }

    fn get_property(instance: Rc<RefCell<Instance>>, prop: &str) -> Option<Value> {
        let field = instance.borrow().fields.get(prop).cloned();

        if field.is_some() {
            return field;
        }

        let class = &instance.borrow().class;

        let m = class
            .method(prop)
            .map(|method| method.bind(instance.clone()));

        if m.is_some() {
            return m;
        }

        None
    }

    fn set(&mut self, name: String, value: Value) {
        self.fields.insert(name, value);
    }

    fn class_name(&self) -> String {
        self.class.name.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeFunction {
    name: String,
    arity: usize,
}

impl NativeFunction {
    fn new(name: impl Into<String>, arity: usize) -> Self {
        Self {
            name: name.into(),
            arity,
        }
    }

    fn call<Out: Write>(
        &self,
        _i: &mut Interpreter<Out>,
        _args: &mut [Value],
        _line: usize,
    ) -> Result<Value> {
        match self.name.as_str() {
            "clock" => Ok(clock()),
            _ => todo!(),
        }
    }
}

/// The function data (the parameters and body) that is too expensive to clone,
/// so I'm heap allocating this once and have `Function` reference it through an
/// `Rc`. This makes quite a difference in the benchmarks.
#[derive(Debug)]
pub struct FunctionDeclaration {
    name: String,
    params: Vec<Token>,
    stmts: Vec<Stmt>,
    is_initializer: bool,
}

impl FunctionDeclaration {
    fn new(
        name: impl Into<String>,
        params: Vec<Token>,
        stmts: Vec<Stmt>,
        is_initializer: bool,
    ) -> Self {
        Self {
            name: name.into(),
            params,
            stmts,
            is_initializer,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    decl: Rc<FunctionDeclaration>,
    closure: ShareableEnv,
}

impl Function {
    fn new(decl: Rc<FunctionDeclaration>, closure: ShareableEnv) -> Self {
        Self { decl, closure }
    }

    fn get_this(&self, line: Line) -> Result<Value> {
        self.closure
            .borrow()
            .get_at_depth("this", 0)
            .map_err(|msg| RuntimeError::new(msg, line))
    }

    fn call(
        &self,
        i: &mut Interpreter<impl Write>,
        args: &mut Vec<Value>,
        line: Line,
    ) -> Result<Value> {
        let mut local_env = Env::new(Some(self.closure.clone()));

        for param in self.decl.params.iter() {
            let v = args.remove(0);
            local_env.define(param.id().to_string(), v);
        }

        match i.interpret_stmts(&self.decl.stmts, Rc::new(RefCell::new(local_env)))? {
            ControlFlow::Return(v) => {
                if self.decl.is_initializer {
                    self.get_this(line)
                } else {
                    Ok(v)
                }
            }
            _ => {
                if self.decl.is_initializer {
                    self.get_this(line)
                } else {
                    Ok(Value::Nil)
                }
            }
        }
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.decl.name == other.decl.name
            && self.decl.params == other.decl.params
            && self.decl.stmts == other.decl.stmts
            && self.decl.is_initializer == other.decl.is_initializer
            && Rc::ptr_eq(&self.closure, &other.closure)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Str(Cow<'static, str>),
    Bool(bool),
    Fun(Function),
    NativeFun(NativeFunction),
    Class(Rc<Class>),
    Instance(Rc<RefCell<Instance>>),
    Nil,
}

impl Value {
    fn arity(&self, line: Line) -> Result<usize> {
        match self {
            Value::Fun(fun) => Ok(fun.decl.params.len()),
            Value::NativeFun(fun) => Ok(fun.arity),
            Value::Class(class) => Ok(class.init_arity(line)),
            _ => error("Can only call functions and classes.".to_string(), line),
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
            Value::NativeFun(fun) => fun.call(i, &mut args, line),
            Value::Class(class) => {
                let instance = Rc::new(RefCell::new(Instance::new(class.clone())));
                if let Some(init) = class.method(INITIALIZER_NAME) {
                    init.bind(instance.clone()).call(i, args, line)?;
                }

                Ok(Value::Instance(instance))
            }
            _ => error("Can only call functions and classes.".to_string(), line),
        }
    }

    fn equals(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Number(l), Value::Number(r)) => l == r,
            (Value::Str(l), Value::Str(r)) => l == r,
            (Value::Bool(l), Value::Bool(r)) => l == r,
            (Value::Nil, Value::Nil) => true,
            (Value::Class(left), Value::Class(right)) => *left == *right,
            (Value::Fun(left), Value::Fun(right)) => *left == *right,
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

            let bound_fun = Function::new(fun.decl.clone(), Rc::new(RefCell::new(env)));

            return Value::Fun(bound_fun);
        }

        panic!("attempted to bind this to {:?}", self);
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => n.fmt(f),
            Value::Str(s) => write!(f, "{}", s),
            Value::Bool(b) => b.fmt(f),
            Value::Nil => write!(f, "nil"),
            Value::Fun(fun) => write!(f, "<fn {}>", fun.decl.name),
            Value::Class(class) => write!(f, "{}", class.name),
            Value::Instance(instance) => {
                write!(f, "{} instance", instance.borrow().class_name())
            }
            Value::NativeFun(_) => write!(f, "<native fn>"),
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
            values: HashMap::default(),
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get_at_depth(&self, name: &str, depth: u8) -> core::result::Result<Value, String> {
        if depth == 0 {
            self.get(name)
        } else if let Some(e) = &self.enclosing {
            e.borrow().get_at_depth(name, depth - 1)
        } else {
            Err(format!("variable lookup failed for {}", &name))
        }
    }

    pub fn get(&self, name: &str) -> core::result::Result<Value, String> {
        if let Some(v) = self.values.get(name) {
            Ok(v.clone())
        } else if let Some(e) = &self.enclosing {
            e.borrow().get(name)
        } else {
            Err(format!("Undefined variable '{}'.", &name))
        }
    }

    pub fn assign_at_depth(
        &mut self,
        name: String,
        value: Value,
        depth: u8,
    ) -> core::result::Result<Value, String> {
        if depth == 0 {
            self.assign(name, value)
        } else if let Some(e) = &self.enclosing {
            e.borrow_mut().assign_at_depth(name, value, depth - 1)
        } else {
            Err(format!("variable lookup failed for {}", &name))
        }
    }

    pub fn assign(&mut self, name: String, value: Value) -> core::result::Result<Value, String> {
        if self.values.get(&name).is_some() {
            self.values.insert(name, value.clone());
            Ok(value)
        } else if let Some(e) = &self.enclosing {
            e.borrow_mut().assign(name, value)
        } else {
            Err(format!("Undefined variable '{}'.", &name))
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

fn clock() -> Value {
    Value::Number(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_millis() as f64
            / 1000_f64,
    )
}

impl<Out: Write> Interpreter<Out> {
    pub fn new(out: Out) -> Self {
        let env = Rc::new(RefCell::new(Env::new(None)));

        env.borrow_mut().define(
            "clock".to_string(),
            Value::NativeFun(NativeFunction::new("clock", 0)),
        );

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
            Stmt::Class(name, superclass, methods, class_methods) => {
                self.interpret_class_decl(name, superclass, methods, class_methods, env)
            }
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
            match self.interpret_stmt(stmt, env.clone())? {
                ControlFlow::Break => break,
                ControlFlow::Return(v) => return Ok(ControlFlow::Return(v)),
                _ => (),
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
            Expr::Str(s) => Ok(Value::Str(s.clone())),
            Expr::Bool(b) => Ok(Value::Bool(*b)),
            Expr::Nil => Ok(Value::Nil),
            Expr::Grouping(expr) => self.interpret_expr(expr, env),
            Expr::Unary(op, expr) => self.interpret_unary_expr(op, expr, env),
            Expr::Binary(lhs, op, rhs) => self.interpret_binary_expr(lhs, op, rhs, env),
            Expr::Ternary(condition, conclusion, alternate) => {
                self.interpret_ternary_expr(condition, conclusion, alternate, env)
            }
            Expr::Var(name, depth) => self.interpret_var(name, *depth, env),
            Expr::Assign(lhs, rhs, depth) => self.interpret_assign(lhs, rhs, *depth, env),
            Expr::Call(callee, t, args) => self.interpret_call(callee, t.line(), args, env),
            Expr::AnonFunDecl(params, body) => self.interpret_anon_fun_decl(params, body, env),
            Expr::Get(object, name) => self.interpret_get(object, name, env),
            Expr::Set(object, name, value) => self.interpret_set(object, name, value, env),
            Expr::This(token) => self.interpret_var(token, Some(0), env),
            Expr::Super(_, method, depth) => self.interpret_super(method, *depth, env),
        }
    }

    fn interpret_super(
        &mut self,
        method: &Token,
        depth: Option<u8>,
        env: ShareableEnv,
    ) -> Result<Value> {
        let superclass = self.interpret_var(&Token::Super(method.line()), depth, env.clone())?;
        let object = self.interpret_var(&Token::This(method.line()), depth.map(|d| d - 1), env)?;

        match (superclass, object) {
            (Value::Class(superclass), Value::Instance(instance)) => superclass
                .method(method.id())
                .map(|m| m.bind(instance.clone()))
                .ok_or(RuntimeError {
                    report: format!("Undefined property '{}'.", method),
                    line: method.line(),
                }),

            _ => error("failed to lookup super method", method.line()),
        }
    }

    fn interpret_get(&mut self, object: &Expr, name: &Token, env: ShareableEnv) -> Result<Value> {
        match self.interpret_expr(object, env)? {
            Value::Instance(instance) => match Instance::get_property(instance, name.id()) {
                Some(v) => Ok(v),
                None => error(format!("Undefined property '{}'.", name), name.line()),
            },
            Value::Class(class) => match class.class_method(name.id()) {
                Some(v) => Ok(v.clone()),
                None => error(format!("Undefined class method '{}'", name), name.line()),
            },
            _ => error("Only instances have properties.", name.line()),
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
                instance.borrow_mut().set(name.id().to_string(), v.clone());
                Ok(v)
            }
            _ => error("Only instances have fields.", name.line()),
        }
    }

    fn interpret_anon_fun_decl(
        &mut self,
        params: &[Token],
        body: &[Stmt],
        env: ShareableEnv,
    ) -> Result<Value> {
        let decl = FunctionDeclaration::new("<anon fn>", params.to_vec(), body.to_vec(), false);
        let fun = Function::new(Rc::new(decl), env);
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
                    "Expected {} arguments but got {}.",
                    callable.arity(line)?,
                    arguments.len()
                ),
                line,
            );
        }

        callable.call(self, arguments, line)
    }

    fn interpret_assign(
        &mut self,
        lhs: &Token,
        rhs: &Expr,
        depth: Option<u8>,
        env: ShareableEnv,
    ) -> Result<Value> {
        match lhs {
            Token::Identifier(line, name) => {
                let value = self.interpret_expr(rhs, env.clone())?;

                if let Some(depth) = depth {
                    env.borrow_mut()
                        .assign_at_depth(name.into(), value, depth)
                        .map_err(|msg| RuntimeError::new(msg, *line))
                } else {
                    self.env
                        .borrow_mut()
                        .assign(name.into(), value)
                        .map_err(|msg| RuntimeError::new(msg, *line))
                }
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
                    error("Operand must be a number.", op.line())
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
                _ => error("Operands must be numbers.", op.line()),
            },
            Token::Plus(_) => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
                (Value::Str(l), Value::Str(r)) => Ok(Value::Str(Cow::Owned(format!("{}{}", l, r)))),
                _ => error("Operands must be two numbers or two strings.", op.line()),
            },
            Token::Star(_) => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),
                _ => error("Operands must be numbers.", op.line()),
            },
            Token::Slash(_) => match (left, right) {
                (Value::Number(l), Value::Number(r)) => {
                    if r == 0.0 {
                        error("divided by 0", op.line())
                    } else {
                        Ok(Value::Number(l / r))
                    }
                }
                _ => error("Operands must be numbers.", op.line()),
            },
            Token::EqualEqual(_) => Ok(Value::Bool(left.equals(&right))),
            Token::BangEqual(_) => Ok(Value::Bool(!left.equals(&right))),
            Token::Greater(_) => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l > r)),
                _ => error("Operands must be numbers.", op.line()),
            },
            Token::GreaterEqual(_) => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l >= r)),
                _ => error("Operands must be numbers.", op.line()),
            },
            Token::Less(_) => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l < r)),
                _ => error("Operands must be numbers.", op.line()),
            },
            Token::LessEqual(_) => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l <= r)),
                _ => error("Operands must be numbers.", op.line()),
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
            env.borrow().get_at_depth(name.id(), d)
        } else {
            self.env.borrow().get(name.id())
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
        let name = name.id();
        let decl =
            FunctionDeclaration::new(name.to_string(), params.to_vec(), stmts.to_vec(), false);
        let fun = Function::new(Rc::new(decl), env.clone());
        env.borrow_mut().define(name.to_string(), Value::Fun(fun));
        Ok(ControlFlow::Continue)
    }

    fn interpret_class_decl(
        &mut self,
        name: &Token,
        superclass_expr: &Option<Expr>,
        methods: &[Stmt],
        class_methods: &[Stmt],
        env: ShareableEnv,
    ) -> Result<ControlFlow> {
        let class_name = name.id();
        env.borrow_mut().define(class_name.to_string(), Value::Nil);

        let superclass = match superclass_expr {
            Some(expr) => {
                let superclass = self.interpret_expr(expr, env.clone())?;
                match superclass {
                    Value::Class(class) => Some(class),
                    _ => return error("Superclass must be a class.", name.line()),
                }
            }
            None => None,
        };

        let env = if let Some(ref superclass) = superclass {
            let e = Rc::new(RefCell::new(Env::new(Some(env))));
            e.borrow_mut()
                .define("super".to_string(), Value::Class(superclass.clone()));
            e
        } else {
            env
        };

        let mut methods_map = HashMap::default();
        for method in methods {
            if let Stmt::FunDecl(name, params, body) = method {
                let decl = FunctionDeclaration::new(
                    name.id(),
                    params.to_vec(),
                    body.to_vec(),
                    name.id() == INITIALIZER_NAME,
                );

                let fun = Function::new(Rc::new(decl), env.clone());

                methods_map.insert(fun.decl.name.clone(), Value::Fun(fun));
            }
        }

        let mut class_methods_map = HashMap::default();
        for method in class_methods {
            if let Stmt::FunDecl(name, params, body) = method {
                let decl =
                    FunctionDeclaration::new(name.id(), params.to_vec(), body.to_vec(), false);

                let fun = Function::new(Rc::new(decl), env.clone());
                class_methods_map.insert(fun.decl.name.clone(), Value::Fun(fun));
            }
        }

        let class = Value::Class(Rc::new(Class::new(
            class_name.to_string(),
            superclass,
            methods_map,
            class_methods_map,
        )));

        let env = if let Value::Class(class) = &class {
            if class.superclass.is_some() {
                env.borrow().enclosing.as_ref().unwrap().clone()
            } else {
                env
            }
        } else {
            env
        };

        env.borrow_mut()
            .assign(class_name.to_string(), class)
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
