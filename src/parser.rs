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
            Token::String(_, string) => Expr::Str(string),
            Token::True(_) => Expr::Bool(true),
            Token::False(_) => Expr::Bool(false),
            Token::Nil(_) => Expr::Nil,
            Token::LeftParen(_) => {
                let lhs = self.parse_expr(0);
                match self.next() {
                    Token::RightParen(_) => {}
                    t => panic!("expected ), got {:?}", t),
                }
                lhs
            }
            t => panic!("invalid start of expression: {:?}", t),
        };

        loop {
            let op = match self.peek() {
                t if t.is_binary_op() => t,
                Token::Eof(_) | Token::RightParen(_) => break,
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
        Token::Minus(_) => (5, 6),
        Token::Plus(_) => (5, 6),
        Token::Slash(_) => (7, 8),
        Token::Star(_) => (7, 8),
        Token::BangEqual(_) | Token::EqualEqual(_) => (1, 2),
        Token::Greater(_) | Token::GreaterEqual(_) | Token::Less(_) | Token::LessEqual(_) => (3, 4),
        t => panic!("invalid infix operator {:?}", t),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast;

    fn sexp(input: &str) -> String {
        ast::sexp(&Parser::parse_expr_from_str(input))
    }

    #[test]
    fn test_literals() {
        assert_eq!("42", sexp("42"));
        assert_eq!("\"spongebob\"", sexp("\"spongebob\""));
        assert_eq!("true", sexp("true"));
        assert_eq!("false", sexp("false"));
        assert_eq!("nil", sexp("nil"));
    }

    #[test]
    fn test_binary_expr() {
        assert_eq!("(+ 1 2)", sexp("1 + 2"));
        assert_eq!("(- (+ 1 2) 3)", sexp("1 + 2 - 3"));
        assert_eq!("(<= 1 42)", sexp("1 <= 42"));
        assert_eq!("(== 3 1)", sexp("3 == 1"));
    }

    #[test]
    fn test_grouping() {
        assert_eq!("(* (+ 1 2) 3)", sexp("(1 + 2) * 3"));
        assert_eq!("(+ 1 2)", sexp("(((1 + 2)))"));
        assert_eq!(sexp("1 + (2 * 3)"), sexp("1 + 2 * 3"));
    }

    #[test]
    fn test_binary_expr_precedence() {
        assert_eq!("(+ 1 (* 2 3))", sexp("1 + 2 * 3"));
        assert_eq!("(+ 1 (/ 2 3))", sexp("1 + 2 / 3"));
        assert_eq!("(+ (+ 1 (* 2 3)) 4)", sexp("1 + 2 * 3 + 4"));
        assert_eq!("(+ (/ 1 2) 3)", sexp("1 / 2 + 3"));
        assert_eq!("(!= (> 1 3) (<= 23 2))", sexp("1 > 3 != 23 <= 2"));
    }
}
