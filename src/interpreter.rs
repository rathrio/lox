use std::{collections::HashMap, fmt::Display, io::Write};

use crate::{
    ast::{Expr, Program, Stmt},
    lexer::{Line, Token},
};

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Number(f64),
    Str(String),
    Bool(bool),
    Nil,
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
            Value::Number(_) | Value::Str(_) => true,
            Value::Nil => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => n.fmt(f),
            Value::Str(s) => write!(f, "{:?}", s),
            Value::Bool(b) => b.fmt(f),
            Value::Nil => write!(f, "nil"),
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

#[derive(Debug, Default)]
pub struct Environment {
    pub values: HashMap<String, Value>,
}

impl Environment {
    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: String) -> Result<Value, String> {
        match self.values.get(&name) {
            Some(v) => Ok(v.clone()),
            None => Err(format!("undefined variable \"{}\"", &name)),
        }
    }

    pub fn assign(&mut self, name: String, value: Value) -> Result<Value, String> {
        match self.values.get(&name) {
            Some(_) => {
                self.values.insert(name, value.clone());
                Ok(value)
            }
            None => Err(format!("undefined variable \"{}\"", &name)),
        }
    }
}

#[derive(Debug)]
pub struct Interpreter<Out: Write> {
    environment: Environment,
    out: Out,
}

impl<Out: Write> Interpreter<Out> {
    pub fn new(out: Out) -> Self {
        Self {
            environment: Environment::default(),
            out,
        }
    }

    pub fn interpret(&mut self, program: &Program) -> Result<(), RuntimeError> {
        for stmt in &program.stmts {
            self.interpret_stmt(stmt)?;
        }

        Ok(())
    }

    fn interpret_stmt(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Expr(expr) => {
                self.interpret_expr(expr)?;
            }
            Stmt::Print(expr) => {
                self.interpret_print(expr)?;
            }
            Stmt::VarDecl(name, expr) => {
                if let Token::Identifier(_, name) = name {
                    let init_value = self.interpret_expr(expr)?;
                    self.environment.define(name.into(), init_value);
                } else {
                    return error("invalid variable token", name.line());
                }
            }
        };

        Ok(())
    }

    fn interpret_print(&mut self, expr: &Expr) -> Result<(), RuntimeError> {
        let value = self.interpret_expr(expr)?;
        self.out
            .write_all(format!("{}\n", value).as_bytes())
            .unwrap();
        self.out.flush().unwrap();
        Ok(())
    }

    fn interpret_expr(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Number(n) => Ok(Value::Number(*n)),
            Expr::Str(s) => Ok(Value::Str(s.into())),
            Expr::Bool(b) => Ok(Value::Bool(*b)),
            Expr::Nil => Ok(Value::Nil),
            Expr::Grouping(expr) => self.interpret_expr(expr),
            Expr::Unary(op, expr) => self.interpret_unary_expr(op, expr),
            Expr::Binary(lhs, op, rhs) => self.interpret_binary_expr(lhs, op, rhs),
            Expr::Ternary(condition, conclusion, alternate) => {
                self.interpret_ternary_expr(condition, conclusion, alternate)
            }
            Expr::Var(name) => self
                .environment
                .get(format!("{}", name))
                .map_err(|msg| RuntimeError::new(msg, name.line())),
            Expr::Assign(lhs, rhs) => match lhs {
                Token::Identifier(line, name) => {
                    let value = self.interpret_expr(rhs)?;
                    self.environment
                        .assign(name.into(), value)
                        .map_err(|msg| RuntimeError::new(msg, *line))
                }
                t => error(format!("invalid LHS for assignment \"{}\"", t), t.line()),
            },
        }
    }

    fn interpret_unary_expr(&mut self, op: &Token, expr: &Expr) -> Result<Value, RuntimeError> {
        let value = self.interpret_expr(expr)?;

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
    ) -> Result<Value, RuntimeError> {
        let left = self.interpret_expr(lhs)?;
        let right = self.interpret_expr(rhs)?;

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
    ) -> Result<Value, RuntimeError> {
        let condition = self.interpret_expr(condition_expr)?;

        if condition.is_truthy() {
            self.interpret_expr(conclusion_expr)
        } else {
            self.interpret_expr(alternate_expr)
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
        interpreter.interpret_expr(&expr)
    }

    fn interpret(input: &str, out: &mut impl Write) -> Result<(), RuntimeError> {
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
}
