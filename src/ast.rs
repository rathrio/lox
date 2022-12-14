use std::borrow::Cow;

use crate::lexer::Token;

#[derive(Debug)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    VarDecl(Token, Expr),
    FunDecl(Token, Vec<Token>, Vec<Stmt>),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    Return(Token, Expr),
    Class(Token, Option<Expr>, Vec<Stmt>, Vec<Stmt>),
    Break,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(f64),
    Str(Cow<'static, str>),
    Bool(bool),
    Nil,
    Grouping(Box<Expr>),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
    Var(Token, Option<u8>),
    This(Token),
    Assign(Token, Box<Expr>, Option<u8>),
    Call(Box<Expr>, Token, Vec<Expr>),
    Get(Box<Expr>, Token),
    Set(Box<Expr>, Token, Box<Expr>),
    AnonFunDecl(Vec<Token>, Vec<Stmt>),
    Super(Token, Token, Option<u8>),
}

impl Expr {
    pub fn is_comma(&self) -> bool {
        matches!(self, Expr::Binary(_, Token::Comma(_), _))
    }
}

pub fn sexp_expr(e: &Expr) -> String {
    match e {
        Expr::Number(n) => format!("{}", n),
        Expr::Str(s) => format!("{:?}", s),
        Expr::Bool(b) => format!("{}", b),
        Expr::Nil => "nil".to_string(),
        Expr::Grouping(e) => format!("(group {})", sexp_expr(e)),
        Expr::Unary(op, e) => format!("({} {})", op, sexp_expr(e)),
        Expr::Binary(lhs, op, rhs) => format!("({} {} {})", op, sexp_expr(lhs), sexp_expr(rhs)),
        Expr::Ternary(condition, conclusion, alternate) => format!(
            "(? {} {} {})",
            sexp_expr(condition),
            sexp_expr(conclusion),
            sexp_expr(alternate)
        ),
        Expr::Var(name, _) => format!("{}", name),
        Expr::Assign(lhs, rhs, _) => format!("(= {} {})", lhs, sexp_expr(rhs)),
        Expr::Call(callee, _, args) => format!(
            "(call {} ({}))",
            sexp_expr(callee),
            args.iter()
                .map(sexp_expr)
                .collect::<Vec<String>>()
                .join(" ")
        ),
        Expr::AnonFunDecl(params, body) => {
            format!("(fun ({}) ({}))", sexp_fun_params(params), sexp_stmts(body))
        }
        Expr::Get(object, name) => format!("(. {} {})", sexp_expr(object), name),
        Expr::Set(object, name, value) => {
            format!("(.= {} {} {})", sexp_expr(object), name, sexp_expr(value))
        }
        Expr::This(token) => token.to_string(),
        Expr::Super(_, token, _) => format!("(super {})", token),
    }
}

pub fn sexp_stmt(s: &Stmt) -> String {
    match s {
        Stmt::Expr(e) => sexp_expr(e),
        Stmt::Print(e) => format!("(print {})", sexp_expr(e)),
        Stmt::VarDecl(id, e) => format!("(var {} {})", id, sexp_expr(e)),
        Stmt::Block(stmts) => format!(
            "(block {})",
            stmts
                .iter()
                .map(sexp_stmt)
                .collect::<Vec<String>>()
                .join(" ")
        ),
        Stmt::If(condition, then_branch, None) => {
            format!("(if {} {})", sexp_expr(condition), sexp_stmt(then_branch),)
        }
        Stmt::If(condition, then_branch, Some(else_branch)) => format!(
            "(if {} {} {})",
            sexp_expr(condition),
            sexp_stmt(then_branch),
            sexp_stmt(else_branch),
        ),
        Stmt::While(condition, stmt) => {
            format!("(while {} {})", sexp_expr(condition), sexp_stmt(stmt))
        }
        Stmt::Break => "break".to_string(),
        Stmt::FunDecl(name, params, body) => {
            format!(
                "(fun {} ({}) ({}))",
                name,
                sexp_fun_params(params),
                sexp_stmts(body)
            )
        }
        Stmt::Return(_, expr) => format!("(return {})", sexp_expr(expr)),
        Stmt::Class(name, superclass, methods, class_methods) => format!(
            "(class {} ({}) ({}) ({}))",
            name,
            superclass.as_ref().map(sexp_expr).unwrap_or_default(),
            sexp_stmts(class_methods),
            sexp_stmts(methods),
        ),
    }
}

fn sexp_stmts(stmts: &[Stmt]) -> String {
    stmts
        .iter()
        .map(sexp_stmt)
        .collect::<Vec<String>>()
        .join(" ")
}

fn sexp_fun_params(params: &[Token]) -> String {
    params
        .iter()
        .map(|t| format!("{}", t))
        .collect::<Vec<String>>()
        .join(" ")
}

pub fn sexp(p: &Program) -> String {
    p.stmts
        .iter()
        .map(sexp_stmt)
        .collect::<Vec<String>>()
        .join(" ")
}

#[allow(unused)]
fn rpn(e: &Expr) -> String {
    match e {
        Expr::Number(n) => format!("{}", n),
        Expr::Str(s) => s.to_string(),
        Expr::Bool(b) => format!("{}", b),
        Expr::Nil => "nil".to_string(),
        Expr::Grouping(e) => rpn(e),
        Expr::Unary(op, e) => format!("{} {}", rpn(e), op),
        Expr::Binary(lhs, op, rhs) => format!("{} {} {}", rpn(lhs), rpn(rhs), op),
        Expr::Var(name, _) => format!("{}", name),
        Expr::Ternary(condition, conclusion, alternate) => format!(
            "{} ? {} : {}",
            rpn(condition),
            rpn(conclusion),
            rpn(alternate)
        ),
        Expr::Assign(lhs, rhs, _) => format!("{} {} =", rpn(rhs), lhs),
        Expr::Call(callee, _, args) => format!(
            "{} {} call",
            args.iter().map(rpn).collect::<Vec<String>>().join(" "),
            rpn(callee)
        ),
        Expr::AnonFunDecl(params, body) => "unsupported".to_string(),
        Expr::Get(object, name) => format!("{} {} .", rpn(object), name),
        Expr::Set(object, name, value) => format!("{} {} {} .=", rpn(object), name, rpn(value)),
        Expr::This(token) => token.to_string(),
        Expr::Super(_, token, _) => format!("super {}", token),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sexp() {
        let lhs = Expr::Number(42.0);

        let add_lhs = Expr::Number(1.0);
        let add_rhs = Expr::Number(2.0);
        let add = Expr::Binary(Box::new(add_lhs), Token::Plus(1), Box::new(add_rhs));

        let rhs = Expr::Grouping(Box::new(add));
        let ast = Expr::Binary(Box::new(lhs), Token::Star(1), Box::new(rhs));

        assert_eq!("(* 42 (group (+ 1 2)))", sexp_expr(&ast));
    }

    #[test]
    fn test_sexp_stmt() {
        let expr = Expr::Number(42.0);
        let stmt = Stmt::Print(expr);
        assert_eq!("(print 42)", sexp_stmt(&stmt));
    }

    #[test]
    fn test_sexp_program() {
        let first = Stmt::Expr(Expr::Number(42.0));
        let second = Stmt::Print(Expr::Str("Hello World!".into()));
        let program = Program {
            stmts: vec![first, second],
        };

        assert_eq!("42 (print \"Hello World!\")", sexp(&program));
    }
}
