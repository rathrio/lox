use crate::lexer::Token;

/// ```ebnf
/// expression     → literal
///                | unary
///                | binary
///                | grouping ;
///
/// literal        → NUMBER | STRING | "true" | "false" | "nil" ;
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

pub fn sexp(e: &Expr) -> String {
    match e {
        Expr::Number(n) => format!("{}", n),
        Expr::Str(s) => format!("{:?}", s),
        Expr::Bool(b) => format!("{}", b),
        Expr::Nil => "nil".to_string(),
        Expr::Grouping(e) => format!("(group {})", sexp(e)),
        Expr::Unary(op, e) => format!("({} {})", op, sexp(e)),
        Expr::Binary(lhs, op, rhs) => format!("({} {} {})", op, sexp(lhs), sexp(rhs)),
        Expr::Ternary(condition, conclusion, alternate) => format!(
            "(? {} {} {})",
            sexp(condition),
            sexp(conclusion),
            sexp(alternate)
        ),
    }
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

        assert_eq!("(* 42 (group (+ 1 2)))", sexp(&ast));
    }
}
