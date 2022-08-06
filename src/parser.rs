use crate::{
    ast::Expr,
    lexer::{Lexer, Token},
};

pub struct Parser {
    tokens: Vec<Token>,
}

impl Parser {
    pub fn parse_expr_from_str(input: &str) -> Expr {
        let mut s = Lexer::new(input.to_string());
        let tokens = s.lex_tokens();
        let mut parser = Self { tokens };
        parser.parse_expr(0)
    }

    fn parse_expr(&mut self, min_bp: u8) -> Expr {
        let mut lhs = match self.next() {
            Token::Number(_, number) => Expr::Number(number as i64),
            _ => panic!("invalid start of expression"),
        };

        loop {
            let op = match self.peek() {
                t if t.is_binary_op() => t,
                Token::Eof(_) => break,
                t => panic!("bad operator token {:?}", t),
            };

            let (l_bp, r_bp) = infix_binding_power(&op);
            if l_bp < min_bp {
                break;
            }

            // consume the operator
            self.next();
            let rhs = self.parse_expr(r_bp);
            lhs = Expr::Binary(Box::new(lhs), op.clone(), Box::new(rhs))
        }

        lhs
    }

    fn peek(&self) -> Token {
        self.tokens.get(0).expect("overshot EOF").clone()
    }

    fn next(&mut self) -> Token {
        self.tokens.drain(0..1).next().expect("overshot EOF")
    }
}

fn infix_binding_power(op: &Token) -> (u8, u8) {
    match op {
        Token::Minus(_) => (1, 2),
        Token::Plus(_) => (1, 2),
        Token::Slash(_) => (3, 4),
        Token::Star(_) => (3, 4),
        t => panic!("invalid infix operator {:?}", t),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::sexp;

    fn parse(input: &str) -> Expr {
        Parser::parse_expr_from_str(input)
    }

    #[test]
    fn test_parse_number() {
        let expr = parse("42");
        assert_eq!("42", sexp(&expr));
    }

    #[test]
    fn test_parse_binary() {
        let expr = parse("1 + 2");
        assert_eq!("(+ 1 2)", sexp(&expr));
    }

    #[test]
    fn test_parse_binary_2() {
        let expr = parse("1 + 2 - 3");
        assert_eq!("(- (+ 1 2) 3)", sexp(&expr));
    }

    #[test]
    fn test_parse_binary_precedence() {
        let expr = parse("1 + 2 * 3");
        assert_eq!("(+ 1 (* 2 3))", sexp(&expr));

        let expr = parse("1 + 2 / 3");
        assert_eq!("(+ 1 (/ 2 3))", sexp(&expr));
    }

    #[test]
    fn test_parse_binary_precedence_2() {
        let expr = parse("1 + 2 * 3 + 4");
        assert_eq!("(+ (+ 1 (* 2 3)) 4)", sexp(&expr));
    }

    #[test]
    fn test_parse_binary_precedence_3() {
        let expr = parse("1 / 2 + 3");
        assert_eq!("(+ (/ 1 2) 3)", sexp(&expr));
    }
}
