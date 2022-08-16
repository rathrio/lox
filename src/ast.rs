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
    Class(Token, Vec<Stmt>),
    Break,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(f64),
    Str(String),
    Bool(bool),
    Nil,
    Grouping(Box<Expr>),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
    Var(Token, Option<u8>),
    Assign(Token, Box<Expr>),
    Call(Box<Expr>, Token, Vec<Expr>),
    AnonFunDecl(Vec<Token>, Vec<Stmt>),
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
        Expr::Assign(lhs, rhs) => format!("(= {} {})", lhs, sexp_expr(rhs)),
        Expr::Call(callee, _, args) => format!(
            "(call {} ({}))",
            sexp_expr(callee),
            args.iter()
                .map(sexp_expr)
                .collect::<Vec<String>>()
                .join(" ")
        ),
        Expr::AnonFunDecl(params, body) => format!(
            "(fun ({}) ({}))",
            sexp_fun_params(params),
            sexp_fun_body(body)
        ),
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
                sexp_fun_body(body)
            )
        }
        Stmt::Return(_, expr) => format!("(return {})", sexp_expr(expr)),
        Stmt::Class(name, methods) => format!(
            "(class {} ({}))",
            name,
            methods.iter().map(sexp_stmt).collect::<Vec<_>>().join(" ")
        ),
    }
}

fn sexp_fun_body(body: &[Stmt]) -> String {
    body.iter()
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
        Expr::Assign(lhs, rhs) => format!("{} {} =", rpn(rhs), lhs),
        Expr::Call(callee, _, args) => format!(
            "{} {} call",
            args.iter().map(rpn).collect::<Vec<String>>().join(" "),
            rpn(callee)
        ),
        Expr::AnonFunDecl(params, body) => "unsupported".to_string(),
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
