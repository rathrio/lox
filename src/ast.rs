use crate::lexer::Token;

/// ```ebnf
/// program        → declaration* EOF ;
///
/// declaration    → varDecl
///                | statement ;
///
/// varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
///
/// statement      → exprStmt
///                | printStmt ;
///
/// exprStmt       → expression ";" ;
/// printStmt      → "print" expression ";" ;
/// ```
pub struct Program {
    pub stmts: Vec<Stmt>,
}

pub enum Stmt {
    Expr(Expr),
    Print(Expr),
}

/// ```ebnf
/// expression     → primary
///                | unary
///                | binary
///                | grouping ;
///
/// primary        → NUMBER | STRING | "true" | "false" | "nil" | IDENTIFIER ;
/// grouping       → "(" expression ")" ;
/// unary          → ( "-" | "!" ) expression ;
/// binary         → expression operator expression ;
/// operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
///                | "+"  | "-"  | "*" | "/" ;
/// ```
#[derive(Debug)]
pub enum Expr {
    Number(f64),
    Str(String),
    Bool(bool),
    Nil,
    Grouping(Box<Expr>),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
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
    }
}

pub fn sexp_stmt(s: &Stmt) -> String {
    match s {
        Stmt::Expr(e) => sexp_expr(e),
        Stmt::Print(e) => format!("(print {})", sexp_expr(e)),
    }
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
        Expr::Ternary(condition, conclusion, alternate) => format!(
            "{} ? {} : {}",
            rpn(condition),
            rpn(conclusion),
            rpn(alternate)
        ),
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
