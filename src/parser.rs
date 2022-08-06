use crate::{
    ast::Expr,
    lexer::{Lexer, Token},
};

#[derive(Debug)]
pub struct ParserError {
    pub report: String,
}

impl ParserError {
    fn new(report: impl Into<String>) -> Self {
        Self {
            report: report.into(),
        }
    }
}

pub struct Parser {
    tokens: Vec<Token>,
}

impl Parser {
    pub fn parse_expr_from_str(input: &str) -> Result<Expr, ParserError> {
        let mut s = Lexer::new(input.to_string());
        let tokens = s.lex_tokens();
        let mut parser = Self { tokens };
        parser.parse_expr(0)
    }

    fn parse_expr(&mut self, min_bp: u8) -> Result<Expr, ParserError> {
        let mut lhs = match self.next() {
            op if op.is_prefix_op() => {
                let (_, bp) = prefix_binding_power(&op)?;
                let lhs = self.parse_expr(bp)?;
                Ok(Expr::Unary(op, Box::new(lhs)))
            }
            op if op.is_infix_op() => {
                // Attempt to consume RHS for ease of error recovery
                let (l_bp, r_bp) = infix_binding_power(&op)?;
                if l_bp >= min_bp {
                    self.parse_expr(r_bp)?;
                }

                Err(ParserError::new(format!(
                    "LHS of binary operation \"{}\" is missing on line {}",
                    op,
                    op.line()
                )))
            }
            Token::Number(_, number) => Ok(Expr::Number(number as i64)),
            Token::String(_, string) => Ok(Expr::Str(string)),
            Token::True(_) => Ok(Expr::Bool(true)),
            Token::False(_) => Ok(Expr::Bool(false)),
            Token::Nil(_) => Ok(Expr::Nil),
            Token::LeftParen(_) => {
                let lhs = self.parse_expr(0)?;

                match self.next() {
                    Token::RightParen(_) => Ok(Expr::Grouping(Box::new(lhs))),
                    t => Err(ParserError::new(format!(
                        "expected a \")\", but got \"{}\" on line {}",
                        t,
                        t.line()
                    ))),
                }
            }
            t => Err(ParserError::new(format!(
                "invalid start of expression with \"{}\" on line {}",
                t,
                t.line()
            ))),
        }?;

        loop {
            let op = match self.peek() {
                t if t.is_infix_op() => t,
                // Not super fond if this approach here. I'm looking at what
                // could be potential expression ends and break out.
                Token::Eof(_) | Token::RightParen(_) | Token::Colon(_) => break,
                t => {
                    return Err(ParserError::new(format!(
                        "expected an operator, but got \"{}\" on line {}",
                        t,
                        t.line()
                    )))
                }
            };

            let (l_bp, r_bp) = infix_binding_power(&op)?;
            if l_bp < min_bp {
                break;
            }

            // consume the operator we peeked at the beginning of the loop
            self.next();

            if let Token::Query(_) = op {
                let conclusion = self.parse_expr(0)?;
                if let Token::Colon(_) = self.next() {
                    let alternate = self.parse_expr(r_bp)?;
                    lhs = Expr::Ternary(Box::new(lhs), Box::new(conclusion), Box::new(alternate));
                } else {
                    return Err(ParserError::new(format!(
                        "missing \":\" in ternary on line {}",
                        op.line()
                    )));
                }
            } else {
                let rhs = self.parse_expr(r_bp)?;
                lhs = Expr::Binary(Box::new(lhs), op.clone(), Box::new(rhs))
            }
        }

        Ok(lhs)
    }

    fn peek(&self) -> Token {
        self.tokens.get(0).expect("overshot EOF").clone()
    }

    fn next(&mut self) -> Token {
        self.tokens.drain(0..1).next().expect("overshot EOF")
    }
}

fn infix_binding_power(op: &Token) -> Result<(u8, u8), ParserError> {
    match op {
        Token::Comma(_) => Ok((1, 2)),
        Token::Query(_) => Ok((4, 3)),
        Token::BangEqual(_) | Token::EqualEqual(_) => Ok((4, 5)),
        Token::Greater(_) | Token::GreaterEqual(_) | Token::Less(_) | Token::LessEqual(_) => {
            Ok((6, 7))
        }
        Token::Minus(_) => Ok((8, 9)),
        Token::Plus(_) => Ok((8, 9)),
        Token::Slash(_) => Ok((10, 11)),
        Token::Star(_) => Ok((10, 11)),
        t => Err(ParserError::new(format!(
            "invalid infix operator {} on line {}",
            t,
            t.line()
        ))),
    }
}

fn prefix_binding_power(op: &Token) -> Result<((), u8), ParserError> {
    match op {
        Token::Minus(_) | Token::Bang(_) => Ok(((), 10)),
        t => Err(ParserError::new(format!(
            "invalid prefix operator {} on line {}",
            t,
            t.line()
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast;

    fn parse_expr(input: &str) -> Result<Expr, ParserError> {
        Parser::parse_expr_from_str(input)
    }

    fn sexp(input: &str) -> String {
        let expr = parse_expr(input).unwrap();
        ast::sexp(&expr)
    }

    #[test]
    fn test_literals() {
        assert_eq!("42", sexp("42"));
        assert_eq!("\"spongebob\"", sexp("\"spongebob\""));
        assert_eq!("true", sexp("true"));
        assert_eq!("false", sexp("false"));
        assert_eq!("nil", sexp("nil"));
    }

    #[ignore] // TODO as soon as identifiers are supported
    #[test]
    fn test_identifier() {
        assert_eq!("(+ a b)", sexp("a + b"));
    }

    #[test]
    fn test_unary() {
        assert_eq!("(! false)", sexp("!false"));
    }

    #[test]
    fn test_unary_precedence() {
        assert_eq!("(== (! false) (> 2 3))", sexp("!false == 2 > 3"));
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
        assert_eq!("(* (group (+ 1 2)) 3)", sexp("(1 + 2) * 3"));
        assert_eq!("(group (group (group (+ 1 2))))", sexp("(((1 + 2)))"));
    }

    #[test]
    fn test_binary_expr_precedence() {
        assert_eq!("(+ 1 (* 2 3))", sexp("1 + 2 * 3"));
        assert_eq!("(+ 1 (/ 2 3))", sexp("1 + 2 / 3"));
        assert_eq!("(+ (+ 1 (* 2 3)) 4)", sexp("1 + 2 * 3 + 4"));
        assert_eq!("(+ (/ 1 2) 3)", sexp("1 / 2 + 3"));
        assert_eq!("(!= (> 1 3) (<= 23 2))", sexp("1 > 3 != 23 <= 2"));
    }

    #[test]
    fn test_comma_expr() {
        assert_eq!("(, (+ 1 3) (+ 2 (/ 3 2)))", sexp("1 + 3, 2 + 3 / 2"));
    }

    #[test]
    fn test_ternary_expr() {
        assert_eq!(
            "(? (> (+ 3 2) 1) (/ 4 2) 42)",
            sexp("3 + 2 > 1 ? 4 / 2 : 42")
        );
    }

    #[test]
    fn test_ternary_expr_precedence() {
        assert_eq!("(? 1 2 (? 3 4 5))", sexp("1 ? 2 : 3 ? 4 : 5"));
    }

    #[test]
    fn test_errors() {
        assert!(parse_expr("1 +").is_err());
        assert!(parse_expr("(42").is_err());
    }

    #[test]
    fn test_binary_lhs_missin_error() {
        match parse_expr(" / 34") {
            Ok(_) => panic!("this parse should not succeed"),
            Err(e) => assert!(e.report.contains("LHS")),
        };
    }
}
