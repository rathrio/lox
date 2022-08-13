use std::{cell::RefCell, collections::HashMap, fmt::Display, io::Write, ops::ControlFlow, rc::Rc};

use crate::{
    ast::{Expr, Program, Stmt},
    lexer::{Line, Token},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Str(String),
    Bool(bool),
    Fun(String, Vec<Token>, Vec<Stmt>),
    Nil,
}

impl TryInto<Function> for Value {
    type Error = RuntimeError;

    fn try_into(self) -> Result<Function, Self::Error> {
        match self {
            Value::Fun(name, params, body) => Ok(Function { name, params, body }),
            v => error(format!("{} is not a function", v), 9999),
        }
    }
}

pub struct Function {
    pub name: String,
    pub params: Vec<Token>,
    body: Vec<Stmt>,
}

impl Function {
    fn call<Out: Write>(
        &self,
        interpreter: &mut Interpreter<Out>,
        mut args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let mut local_env = Env::new(Some(interpreter.env.clone()));

        for param in self.params.iter() {
            let v = args.remove(0);
            local_env.define(param.to_string(), v);
        }

        interpreter.interpret_stmts(&self.body, Rc::new(RefCell::new(local_env)))?;
        Ok(Value::Nil)
    }

    fn arity(&self) -> usize {
        self.params.len()
    }
}

impl Value {
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
            Value::Number(_) | Value::Str(_) | Value::Fun(_, _, _) => true,
            Value::Nil => false,
        }
    }

    fn is_falsey(&self) -> bool {
        !self.is_truthy()
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => n.fmt(f),
            Value::Str(s) => write!(f, "{:?}", s),
            Value::Bool(b) => b.fmt(f),
            Value::Nil => write!(f, "nil"),
            Value::Fun(name, _, _) => write!(f, "<fn {}>", name),
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

fn error<T>(report: impl Into<String>, line: Line) -> Result<T, RuntimeError> {
    Err(RuntimeError::new(report, line))
}

type ShareableEnv = Rc<RefCell<Env>>;

#[derive(Debug)]
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

    pub fn get(&self, name: String) -> Result<Value, String> {
        if let Some(v) = self.values.get(&name) {
            Ok(v.clone())
        } else if let Some(e) = &self.enclosing {
            e.borrow_mut().get(name)
        } else {
            Err(format!("undefined variable \"{}\"", &name))
        }
    }

    pub fn assign(&mut self, name: String, value: Value) -> Result<Value, String> {
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

type Flow = ControlFlow<(), ()>;

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

    pub fn interpret(&mut self, program: &Program) -> Result<Flow, RuntimeError> {
        self.interpret_stmts(&program.stmts, self.env.clone())
    }

    fn interpret_stmts(&mut self, stmts: &[Stmt], env: ShareableEnv) -> Result<Flow, RuntimeError> {
        for stmt in stmts {
            if let Flow::Break(_) = self.interpret_stmt(stmt, env.clone())? {
                return Ok(Flow::Break(()));
            }
        }

        Ok(Flow::Continue(()))
    }

    fn interpret_stmt(&mut self, stmt: &Stmt, env: ShareableEnv) -> Result<Flow, RuntimeError> {
        match stmt {
            Stmt::Expr(expr) => {
                self.interpret_expr(expr, env)?;
                Ok(Flow::Continue(()))
            }
            Stmt::Print(expr) => {
                self.interpret_print(expr, env)?;
                Ok(Flow::Continue(()))
            }
            Stmt::VarDecl(name, expr) => {
                self.interpret_var_decl(name, expr, env)?;
                Ok(Flow::Continue(()))
            }
            Stmt::Block(stmts) => self.interpret_block(stmts, env),
            Stmt::If(condition, then_branch, else_branch) => {
                let value = self.interpret_expr(condition, env.clone())?;
                if value.is_truthy() {
                    self.interpret_stmt(then_branch, env)
                } else if let Some(b) = else_branch {
                    self.interpret_stmt(b, env)
                } else {
                    Ok(Flow::Continue(()))
                }
            }
            Stmt::While(condition, stmt) => {
                while self.interpret_expr(condition, env.clone())?.is_truthy() {
                    if let ControlFlow::Break(_) = self.interpret_stmt(stmt, env.clone())? {
                        break;
                    }
                }

                Ok(Flow::Continue(()))
            }
            Stmt::Break => Ok(Flow::Break(())),
            Stmt::FunDecl(name, params, stmts) => {
                let name = name.to_string();
                env.borrow_mut().define(
                    name.clone(),
                    Value::Fun(name, params.to_vec(), stmts.clone()),
                );
                Ok(Flow::Continue(()))
            }
            Stmt::Return(_, _expr) => todo!(),
        }
    }

    fn interpret_block(&mut self, stmts: &[Stmt], env: ShareableEnv) -> Result<Flow, RuntimeError> {
        let local_env = Env::new(Some(env));
        self.interpret_stmts(stmts, Rc::new(RefCell::new(local_env)))
    }

    fn interpret_var_decl(
        &mut self,
        name: &Token,
        expr: &Expr,
        env: ShareableEnv,
    ) -> Result<(), RuntimeError> {
        if let Token::Identifier(_, name) = name {
            let init_value = self.interpret_expr(expr, env.clone())?;
            env.borrow_mut().define(name.into(), init_value);
        } else {
            return error("invalid variable token", name.line());
        };

        Ok(())
    }

    fn interpret_print(&mut self, expr: &Expr, env: ShareableEnv) -> Result<(), RuntimeError> {
        let value = self.interpret_expr(expr, env)?;
        self.out
            .write_all(format!("{}\n", value).as_bytes())
            .unwrap();
        self.out.flush().unwrap();
        Ok(())
    }

    pub fn interpret_expr(
        &mut self,
        expr: &Expr,
        env: ShareableEnv,
    ) -> Result<Value, RuntimeError> {
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
            Expr::Var(name) => env
                .borrow()
                .get(format!("{}", name))
                .map_err(|msg| RuntimeError::new(msg, name.line())),
            Expr::Assign(lhs, rhs) => match lhs {
                Token::Identifier(line, name) => {
                    let value = self.interpret_expr(rhs, env.clone())?;
                    env.borrow_mut()
                        .assign(name.into(), value)
                        .map_err(|msg| RuntimeError::new(msg, *line))
                }
                t => error(format!("invalid LHS for assignment \"{}\"", t), t.line()),
            },
            Expr::Call(callee, t, args) => {
                let function: Function = self.interpret_expr(callee, env.clone())?.try_into()?;
                let mut arguments = Vec::new();
                for arg_expr in args {
                    arguments.push(self.interpret_expr(arg_expr, env.clone())?);
                }

                if arguments.len() != function.arity() {
                    return error(
                        format!(
                            "{} expected {} arguments, provided {}",
                            function.name,
                            function.arity(),
                            arguments.len()
                        ),
                        t.line(),
                    );
                }

                function.call(self, arguments)
            }
        }
    }

    fn interpret_unary_expr(
        &mut self,
        op: &Token,
        expr: &Expr,
        env: ShareableEnv,
    ) -> Result<Value, RuntimeError> {
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
    ) -> Result<Value, RuntimeError> {
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
    ) -> Result<Value, RuntimeError> {
        let condition = self.interpret_expr(condition_expr, env.clone())?;

        if condition.is_truthy() {
            self.interpret_expr(conclusion_expr, env)
        } else {
            self.interpret_expr(alternate_expr, env)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Parser;
    use std::io;

    use super::*;

    fn interpret_expr(input: &str) -> Result<Value, RuntimeError> {
        let expr = Parser::parse_expr_str(input).expect("syntax error");
        let mut interpreter = Interpreter::new(io::stdout());
        let env = Rc::new(RefCell::new(Env::new(None)));
        interpreter.interpret_expr(&expr, env)
    }

    fn interpret(input: &str, out: &mut impl Write) -> Result<Flow, RuntimeError> {
        let program = Parser::parse_str(input).expect("syntax error");
        let mut interpreter = Interpreter::new(out);
        interpreter.interpret(&program)
    }

    fn assert_outputted(out: Vec<u8>, expected_output: String) {
        assert_eq!(
            String::from_utf8(out).unwrap(),
            format!("{}\n", expected_output)
        );
    }

    #[test]
    fn test_unary() {
        assert_eq!(Value::Number(-42.0), interpret_expr("- 42").unwrap());
        assert_eq!(Value::Number(42.0), interpret_expr("--42").unwrap());

        assert_eq!(Value::Bool(false), interpret_expr("!true").unwrap());
        assert_eq!(Value::Bool(true), interpret_expr("!!true").unwrap());
        assert_eq!(Value::Bool(true), interpret_expr("!false").unwrap());
        assert_eq!(Value::Bool(true), interpret_expr("!nil").unwrap());
        assert_eq!(Value::Bool(false), interpret_expr("!42").unwrap());
        assert_eq!(
            Value::Bool(false),
            interpret_expr("!(\"hi\" + \"there\")").unwrap()
        );

        assert!(interpret_expr(r#"- false"#).is_err());
    }

    #[test]
    fn test_add() {
        assert_eq!(Value::Number(3.0), interpret_expr("1 + 2").unwrap());
        assert_eq!(Value::Number(6.0), interpret_expr("1 + 2 + 3").unwrap());
        assert_eq!(
            Value::Str("Well, hello there friends!".into()),
            interpret_expr(r#""Well," + " " + "hello there friends" + "!""#).unwrap()
        );

        assert!(interpret_expr("true + 23").is_err());
    }

    #[test]
    fn test_subtract() {
        assert_eq!(Value::Number(-1.0), interpret_expr("1 - 2").unwrap());
        assert!(interpret_expr("true - 23").is_err());
    }

    #[test]
    fn test_mult() {
        assert_eq!(Value::Number(6.0), interpret_expr("2 * 3").unwrap());
        assert!(interpret_expr("\"chabis\" * 23").is_err());
    }

    #[test]
    fn test_precedence() {
        assert_eq!(Value::Number(7.0), interpret_expr("1 + 2 * 3").unwrap());
        assert_eq!(Value::Number(9.0), interpret_expr("(1 + 2) * 3").unwrap());
    }

    #[test]
    fn test_div() {
        assert_eq!(
            Value::Number(0.6666666666666666),
            interpret_expr("2 / 3").unwrap()
        );
        assert!(interpret_expr("\"chabis\" / 23").is_err());
    }

    #[test]
    fn test_div_by_zero() {
        assert!(interpret_expr("42 / 0").is_err());
    }

    #[test]
    fn test_equal() {
        assert_eq!(Value::Bool(true), interpret_expr("1 == 1").unwrap());
        assert_eq!(Value::Bool(false), interpret_expr("1 != 1").unwrap());

        assert_eq!(Value::Bool(false), interpret_expr("1 == 42").unwrap());
        assert_eq!(Value::Bool(true), interpret_expr("1 != 42").unwrap());

        assert_eq!(Value::Bool(false), interpret_expr("1 == \"1\"").unwrap());
        assert_eq!(Value::Bool(true), interpret_expr("1 != \"1\"").unwrap());

        assert_eq!(Value::Bool(false), interpret_expr("nil == false").unwrap());
        assert_eq!(Value::Bool(true), interpret_expr("nil != false").unwrap());

        assert_eq!(
            Value::Bool(true),
            interpret_expr("\"hi\" == \"hi\"").unwrap()
        );
        assert_eq!(
            Value::Bool(false),
            interpret_expr("\"hi\" != \"hi\"").unwrap()
        );

        assert_eq!(
            Value::Bool(false),
            interpret_expr("\"there\" == \"hi\"").unwrap()
        );
        assert_eq!(
            Value::Bool(true),
            interpret_expr("\"there\" != \"hi\"").unwrap()
        );

        assert_eq!(Value::Bool(true), interpret_expr("nil == nil").unwrap());
        assert_eq!(Value::Bool(false), interpret_expr("nil != nil").unwrap());
    }

    #[test]
    fn test_comparisons() {
        assert_eq!(Value::Bool(true), interpret_expr("2 > 1").unwrap());
        assert_eq!(Value::Bool(true), interpret_expr("1 <= 1").unwrap());
        assert_eq!(Value::Bool(false), interpret_expr("1 < 1").unwrap());

        assert!(interpret_expr("1 > false").is_err());
    }

    #[test]
    fn test_ternary() {
        assert_eq!(
            Value::Number(42.0),
            interpret_expr("4 > 3 ? 2 + 20 * 2 : nil").unwrap()
        );

        assert_eq!(
            Value::Str("alternate".into()),
            interpret_expr("nil ? 1 + 2 : \"alternate\"").unwrap()
        );
    }

    #[test]
    fn test_comma() {
        assert_eq!(
            Value::Number(92.0),
            interpret_expr("1 == 1, 4 * 23").unwrap()
        );
    }

    #[test]
    fn test_out() {
        let mut out = Vec::new();
        interpret("print \"Hello\" + \" World!\";", &mut out).unwrap();
        assert_outputted(out, "\"Hello World!\"".into());
    }

    #[test]
    fn test_variable_decls() {
        let mut out = Vec::new();
        interpret("var a = 1; var b = 2; print a + b;", &mut out).unwrap();
        assert_outputted(out, "3".into());
    }

    #[test]
    fn test_variable_decls_2() {
        let mut out = Vec::new();
        interpret("var a = 40; var a = a + 2; print a;", &mut out).unwrap();
        assert_outputted(out, "42".into());
    }

    #[test]
    fn test_variable_assign() {
        let mut out = Vec::new();
        interpret("var a = 1; a = 2 + a; print a;", &mut out).unwrap();
        assert_outputted(out, "3".into());
    }

    #[test]
    fn test_block_scope() {
        let mut out = Vec::new();
        let script = r#"
        var a = "outer";
        {
            var a = "inner";
            print a;
        }
        print a;
        "#;
        interpret(script, &mut out).unwrap();
        assert_outputted(out, "\"inner\"\n\"outer\"".into());
    }

    #[test]
    fn test_block_scope_2() {
        let mut out = Vec::new();
        let script = r#"
        var a = "outer";
        {
            var b = "inner";
        }
        print b;
        "#;
        assert!(interpret(script, &mut out).is_err());
    }

    #[test]
    fn test_block_scope_3() {
        let mut out = Vec::new();
        let script = r#"
        var a = "outer";
        {
            {
                a = "inner";
            }
        }
        print a;
        "#;
        interpret(script, &mut out).unwrap();
        assert_outputted(out, "\"inner\"".into());
    }

    #[test]
    fn test_block_scope_4() {
        let mut out = Vec::new();
        let script = r#"
        var a = 1;
        {
             var a = a + 2;
             print a;
        }
        "#;
        interpret(script, &mut out).unwrap();
        assert_outputted(out, "3".into());
    }

    #[test]
    fn test_if_else() {
        let mut out = Vec::new();
        let script = r#"
        if (42)
            print "hey";
        else
            print "ho";
        "#;
        interpret(script, &mut out).unwrap();
        assert_outputted(out, "\"hey\"".into());

        let mut out = Vec::new();
        let script = r#"
        if (!42)
            print "hey";
        else {
            print "chabis";
        }
        "#;
        interpret(script, &mut out).unwrap();
        assert_outputted(out, "\"chabis\"".into());
    }

    #[test]
    fn test_logical() {
        assert_eq!(Value::Number(42.0), interpret_expr("nil or 42").unwrap());
        assert_eq!(Value::Number(12.0), interpret_expr("true and 12").unwrap());
        assert_eq!(Value::Bool(false), interpret_expr("false and 12").unwrap());
    }

    #[test]
    fn test_logical_2() {
        let mut out = Vec::new();
        let script = r#"
        var a = nil;
        var b = a or 42;
        print b;
        "#;
        interpret(script, &mut out).unwrap();
        assert_outputted(out, "42".into());
    }

    #[test]
    fn test_while() {
        let mut out = Vec::new();
        let script = r#"
        var a = 3;
        while (a >= 0) {
            print a;
            a = a - 1;
        }
        "#;
        interpret(script, &mut out).unwrap();
        assert_outputted(out, "3\n2\n1\n0".into());
    }

    #[test]
    fn test_break() {
        let mut out = Vec::new();
        let script = r#"
        var a = 3;
        while (true) {
            if (a < 0) break;
            print a;
            a = a - 1;
        }
        "#;
        interpret(script, &mut out).unwrap();
        assert_outputted(out, "3\n2\n1\n0".into());
    }

    #[test]
    fn test_functions() {
        let mut out = Vec::new();
        let script = r#"
        fun count(n) {
            if (n > 1) count(n - 1);
            print n;
        }
        count(3);
        "#;
        interpret(script, &mut out).unwrap();
        assert_outputted(out, "1\n2\n3".into());
    }
}
