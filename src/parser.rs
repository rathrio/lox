use crate::{
    ast::{Expr, Program, Stmt},
    lexer::{Lexer, Line, Token},
};

#[derive(Debug)]
pub struct ParserError {
    pub report: String,
    pub line: Line,
}

impl ParserError {
    fn new(report: impl Into<String>, line: Line) -> Self {
        Self {
            report: report.into(),
            line,
        }
    }
}

fn error<T>(report: impl Into<String>, line: Line) -> Result<T, ParserError> {
    Err(ParserError::new(report, line))
}

fn try_into_assign(expr: Expr, line: Line) -> Result<Expr, ParserError> {
    if let Expr::Binary(lhs, Token::Equal(_), rhs) = expr {
        if let Expr::Var(name) = *lhs {
            return Ok(Expr::Assign(name, rhs));
        }
    }

    Err(ParserError::new("invalid LHS in assignment", line))
}

/// Flattens a comma expression tree into an expression list.
fn try_into_args(expr: Expr) -> Result<Vec<Expr>, ParserError> {
    match expr {
        Expr::Binary(lhs, Token::Comma(_), rhs) => {
            if lhs.is_comma() {
                let mut args = try_into_args(*lhs)?;
                args.push(*rhs);
                Ok(args)
            } else {
                Ok(vec![*lhs, *rhs])
            }
        }
        e => Ok(vec![e]),
    }
}

pub struct Parser {
    tokens: Vec<Token>,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        let mut s = Lexer::new(input.to_string());
        let tokens = s.lex_tokens();
        Self { tokens }
    }

    pub fn parse_str(input: &str) -> Result<Program, ParserError> {
        Self::new(input).parse_program()
    }

    pub fn parse_expr_str(input: &str) -> Result<Expr, ParserError> {
        Self::new(input).parse_expr(0)
    }

    fn parse_program(&mut self) -> Result<Program, ParserError> {
        let mut stmts = Vec::new();

        while !self.peek().is_eof() {
            stmts.push(self.parse_decl(false)?);
        }

        Ok(Program { stmts })
    }

    fn parse_decl(&mut self, is_break_allowed: bool) -> Result<Stmt, ParserError> {
        match self.peek() {
            Token::Var(_) => self.parse_var_decl(),
            _ => self.parse_stmt(is_break_allowed),
        }
    }

    fn parse_var_decl(&mut self) -> Result<Stmt, ParserError> {
        // consume var token
        self.next();

        let id = self.next();
        let eq = self.next();
        let decl = match (&id, &eq) {
            (Token::Identifier(_, _), Token::Equal(_)) => {
                Ok(Stmt::VarDecl(id, self.parse_expr(0)?))
            }
            (t, _) => error("invalid variable declaration", t.line()),
        };

        self.expect_semi("after variable declaration")?;
        decl
    }

    fn parse_stmt(&mut self, is_break_allowed: bool) -> Result<Stmt, ParserError> {
        match self.peek() {
            Token::If(_) => self.parse_if_stmt(is_break_allowed),
            Token::While(_) => self.parse_while_stmt(),
            Token::For(_) => self.parse_for_stmt(),
            Token::Break(line) => {
                if is_break_allowed {
                    self.parse_break_stmt()
                } else {
                    error("break not allowed in this context (there is no enclosing loop to break out of)", line)
                }
            }
            Token::Print(_) => self.parse_print_stmt(),
            Token::LeftBrace(_) => self.parse_block(is_break_allowed),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_if_stmt(&mut self, is_break_allowed: bool) -> Result<Stmt, ParserError> {
        // consume if token
        self.next();
        self.expect_left_paren("after if")?;
        let condition = self.parse_expr(0)?;
        self.expect_right_paren("after if condition")?;
        let then_branch = self.parse_stmt(is_break_allowed)?;

        let mut else_branch = None;
        if let Token::Else(_) = self.peek() {
            self.next();
            let eb = self.parse_stmt(is_break_allowed)?;
            else_branch = Some(Box::new(eb));
        }

        Ok(Stmt::If(condition, Box::new(then_branch), else_branch))
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt, ParserError> {
        // consume while token
        self.next();
        self.expect_left_paren("after while")?;
        let condition = self.parse_expr(0)?;
        self.expect_right_paren("after while condition")?;
        let stmt = self.parse_stmt(true)?;

        Ok(Stmt::While(condition, Box::new(stmt)))
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt, ParserError> {
        // consume for token
        self.next();
        self.expect_left_paren("after for")?;

        let mut block_stmts: Vec<Stmt> = Vec::new();

        match self.peek() {
            Token::Semicolon(_) => {
                self.next();
            }
            Token::Var(_) => block_stmts.push(self.parse_var_decl()?),
            _ => block_stmts.push(self.parse_expr_stmt()?),
        };

        let condition = match self.peek() {
            Token::Semicolon(_) => Expr::Bool(true),
            _ => self.parse_expr(0)?,
        };
        self.expect_semi("after loop condition")?;

        let increment = match self.peek() {
            Token::RightParen(_) => None,
            _ => Some(self.parse_expr(0)?),
        };
        self.expect_right_paren("after for condition")?;

        let for_stmt = self.parse_stmt(true)?;
        let while_stmt = if let Some(i) = increment {
            Stmt::Block(vec![for_stmt, Stmt::Expr(i)])
        } else {
            for_stmt
        };

        block_stmts.push(Stmt::While(condition, Box::new(while_stmt)));
        Ok(Stmt::Block(block_stmts))
    }

    fn parse_break_stmt(&mut self) -> Result<Stmt, ParserError> {
        self.next();
        self.expect_semi("after break")?;
        Ok(Stmt::Break)
    }

    fn parse_print_stmt(&mut self) -> Result<Stmt, ParserError> {
        // consume print token
        self.next();
        let expr = self.parse_expr(0)?;
        self.expect_semi("after print statement")?;
        Ok(Stmt::Print(expr))
    }

    fn parse_block(&mut self, is_break_allowed: bool) -> Result<Stmt, ParserError> {
        // consume {
        self.next();

        let mut stmts = Vec::new();
        loop {
            match self.peek() {
                Token::RightBrace(_) => break,
                Token::Eof(_) => break,
                _ => (),
            };

            stmts.push(self.parse_decl(is_break_allowed)?);
        }

        match self.next() {
            Token::RightBrace(_) => Ok(Stmt::Block(stmts)),
            t => error(format!("expected a \"}}\", but got \"{}\"", t), t.line()),
        }
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.parse_expr(0)?;
        self.expect_semi("after expression statement")?;
        Ok(Stmt::Expr(expr))
    }

    fn parse_expr(&mut self, min_bp: u8) -> Result<Expr, ParserError> {
        let mut expr = match self.next() {
            op if op.is_prefix_op() => {
                let (_, bp) = prefix_binding_power(&op)?;
                let lhs = self.parse_expr(bp)?;
                Ok(Expr::Unary(op, Box::new(lhs)))
            }
            op if !matches!(op, Token::LeftParen(_)) && op.is_infix_op() => {
                // Attempt to consume RHS for ease of error recovery
                let (l_bp, r_bp) = infix_binding_power(&op)?;
                if l_bp >= min_bp {
                    self.parse_expr(r_bp)?;
                }

                error(
                    format!("LHS of binary operation \"{}\" is missing", op),
                    op.line(),
                )
            }
            Token::Number(_, number) => Ok(Expr::Number(number)),
            Token::String(_, string) => Ok(Expr::Str(string)),
            Token::True(_) => Ok(Expr::Bool(true)),
            Token::False(_) => Ok(Expr::Bool(false)),
            Token::Nil(_) => Ok(Expr::Nil),
            Token::Identifier(line, name) => Ok(Expr::Var(Token::Identifier(line, name))),
            Token::LeftParen(_) => {
                let lhs = self.parse_expr(0)?;

                match self.next() {
                    Token::RightParen(_) => Ok(Expr::Grouping(Box::new(lhs))),
                    t => error(format!("expected a \")\", but got \"{}\"", t), t.line()),
                }
            }
            t => error(
                format!("invalid start of expression with \"{}\"", t),
                t.line(),
            ),
        }?;

        loop {
            let op = match self.peek() {
                t if t.is_infix_op() => t,
                // Not super fond if this approach here. I'm looking at what
                // could be potential expression ends and break out.
                Token::Eof(_)
                | Token::RightParen(_)
                | Token::RightBrace(_)
                | Token::Colon(_)
                | Token::Semicolon(_) => break,
                t => return error(format!("expected an operator, but got \"{}\"", t), t.line()),
            };

            let (l_bp, r_bp) = infix_binding_power(&op)?;
            if l_bp < min_bp {
                break;
            }

            if let Token::LeftParen(_) = op {
                // consume (
                self.next();
                let args = if let Token::RightParen(_) = self.peek() {
                    Vec::new()
                } else {
                    try_into_args(self.parse_expr(0)?)?
                };
                // consume )
                self.expect_right_paren("after call args")?;
                return Ok(Expr::Call(Box::new(expr), op, args));
            }

            // consume the operator we peeked at the beginning of the loop
            self.next();

            if let Token::Query(_) = op {
                let conclusion = self.parse_expr(0)?;
                if let Token::Colon(_) = self.next() {
                    let alternate = self.parse_expr(r_bp)?;
                    expr = Expr::Ternary(Box::new(expr), Box::new(conclusion), Box::new(alternate));
                } else {
                    return error("missing \":\" in ternary", op.line());
                }
            } else {
                let rhs = self.parse_expr(r_bp)?;
                expr = Expr::Binary(Box::new(expr), op.clone(), Box::new(rhs));

                if let Token::Equal(_) = op {
                    expr = try_into_assign(expr, op.line())?;
                }
            }
        }

        Ok(expr)
    }

    fn peek(&self) -> Token {
        self.tokens.get(0).expect("overshot EOF").clone()
    }

    fn next(&mut self) -> Token {
        self.tokens.drain(0..1).next().expect("overshot EOF")
    }

    fn expect_left_paren(&mut self, pos_msg: &str) -> Result<(), ParserError> {
        match self.next() {
            Token::LeftParen(_) => Ok(()),
            t => error(
                format!("expected \"(\" {}, but got \"{}\" instead", pos_msg, t),
                t.line(),
            )?,
        }
    }

    fn expect_right_paren(&mut self, pos_msg: &str) -> Result<(), ParserError> {
        match self.next() {
            Token::RightParen(_) => Ok(()),
            t => error(
                format!("expected \")\" {}, but got \"{}\" instead", pos_msg, t),
                t.line(),
            )?,
        }
    }

    fn expect_semi(&mut self, pos_msg: &str) -> Result<(), ParserError> {
        match self.next() {
            Token::Semicolon(_) => Ok(()),
            t => error(
                format!("expected \";\" {}, but got \"{}\" instead", pos_msg, t),
                t.line(),
            )?,
        }
    }
}

fn infix_binding_power(op: &Token) -> Result<(u8, u8), ParserError> {
    match op {
        Token::Equal(_) => Ok((1, 2)),
        Token::Comma(_) => Ok((3, 4)),
        Token::Or(_) | Token::And(_) => Ok((5, 6)),
        Token::Query(_) => Ok((8, 7)),
        Token::BangEqual(_) | Token::EqualEqual(_) => Ok((9, 10)),
        Token::Greater(_) | Token::GreaterEqual(_) | Token::Less(_) | Token::LessEqual(_) => {
            Ok((11, 12))
        }
        Token::Minus(_) | Token::Plus(_) => Ok((13, 14)),
        Token::Slash(_) | Token::Star(_) => Ok((15, 16)),
        Token::LeftParen(_) => Ok((17, 18)),
        t => error(format!("invalid infix operator {}", t), t.line()),
    }
}

fn prefix_binding_power(op: &Token) -> Result<((), u8), ParserError> {
    match op {
        Token::Minus(_) | Token::Bang(_) => Ok(((), 19)),
        t => error(format!("invalid prefix operator {}", t), t.line()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast;

    fn parse(input: &str) -> Result<Program, ParserError> {
        Parser::parse_str(input)
    }

    fn sexp(input: &str) -> String {
        let program = parse(input).unwrap();
        ast::sexp(&program)
    }

    fn parse_expr(input: &str) -> Result<Expr, ParserError> {
        Parser::parse_expr_str(input)
    }

    fn sexp_expr(input: &str) -> String {
        let expr = parse_expr(input).unwrap();
        ast::sexp_expr(&expr)
    }

    #[test]
    fn test_literals() {
        assert_eq!("42", sexp_expr("42"));
        assert_eq!("\"spongebob\"", sexp_expr("\"spongebob\""));
        assert_eq!("true", sexp_expr("true"));
        assert_eq!("false", sexp_expr("false"));
        assert_eq!("nil", sexp_expr("nil"));
    }

    #[test]
    fn test_identifier() {
        assert_eq!("(+ a b)", sexp_expr("a + b"));
    }

    #[test]
    fn test_unary() {
        assert_eq!("(! false)", sexp_expr("!false"));
    }

    #[test]
    fn test_unary_precedence() {
        assert_eq!("(== (! false) (> 2 3))", sexp_expr("!false == 2 > 3"));
    }

    #[test]
    fn test_binary_expr() {
        assert_eq!("(+ 1 2)", sexp_expr("1 + 2"));
        assert_eq!("(- (+ 1 2) 3)", sexp_expr("1 + 2 - 3"));
        assert_eq!("(<= 1 42)", sexp_expr("1 <= 42"));
        assert_eq!("(== 3 1)", sexp_expr("3 == 1"));
    }

    #[test]
    fn test_grouping() {
        assert_eq!("(* (group (+ 1 2)) 3)", sexp_expr("(1 + 2) * 3"));
        assert_eq!("(group (group (group (+ 1 2))))", sexp_expr("(((1 + 2)))"));
    }

    #[test]
    fn test_binary_expr_precedence() {
        assert_eq!("(+ 1 (* 2 3))", sexp_expr("1 + 2 * 3"));
        assert_eq!("(+ 1 (/ 2 3))", sexp_expr("1 + 2 / 3"));
        assert_eq!("(+ (+ 1 (* 2 3)) 4)", sexp_expr("1 + 2 * 3 + 4"));
        assert_eq!("(+ (/ 1 2) 3)", sexp_expr("1 / 2 + 3"));
        assert_eq!("(!= (> 1 3) (<= 23 2))", sexp_expr("1 > 3 != 23 <= 2"));
    }

    #[test]
    fn test_comma_expr() {
        assert_eq!("(, (+ 1 3) (+ 2 (/ 3 2)))", sexp_expr("1 + 3, 2 + 3 / 2"));
        assert_eq!("(, (, 1 2) 3)", sexp_expr("1, 2, 3"));
        assert_eq!("(, (, (, 1 2) 3) (! 42))", sexp_expr("1, 2, 3, !42"));
    }

    #[test]
    fn test_ternary_expr() {
        assert_eq!(
            "(? (> (+ 3 2) 1) (/ 4 2) 42)",
            sexp_expr("3 + 2 > 1 ? 4 / 2 : 42")
        );
    }

    #[test]
    fn test_ternary_expr_precedence() {
        assert_eq!("(? 1 2 (? 3 4 5))", sexp_expr("1 ? 2 : 3 ? 4 : 5"));
    }

    #[test]
    fn test_expr_errors() {
        assert!(parse_expr("1 +").is_err());
        assert!(parse_expr("(42").is_err());
    }

    #[test]
    fn test_binary_lhs_missing_error() {
        match parse_expr(" / 34") {
            Ok(_) => panic!("this parse should not succeed"),
            Err(e) => assert!(e.report.contains("LHS")),
        };
    }

    #[test]
    fn test_program() {
        assert_eq!("(print \"Hello World!\")", sexp("print \"Hello World!\";"));

        assert_eq!(
            "(+ 1 2) (print \"Hello World!\")",
            sexp("1 + 2; print \"Hello World!\";")
        );
    }

    #[test]
    fn test_declarations() {
        assert_eq!("(var a 42)", sexp("var a = 42;"));
        assert_eq!(
            "(var name (+ \"hi \" \"tadeus\"))",
            sexp("var name = \"hi \" + \"tadeus\";")
        );
    }

    #[test]
    fn test_assignment() {
        assert_eq!("(= a 42)", sexp("a = 42;"));
        assert!(parse_expr("1 + 2 = 42").is_err());
    }

    #[test]
    fn test_block() {
        assert_eq!(
            "(block (var a 42) (print a))",
            sexp("{ var a = 42; print a; }")
        );
    }

    #[test]
    fn test_if_else() {
        let script = r#"
        if (1 + 1)
            print "hey";
        "#;
        assert_eq!("(if (+ 1 1) (print \"hey\"))", sexp(script));

        let script = r#"
        if (42)
            print "hey";
        else
            print "ho";
        "#;
        assert_eq!("(if 42 (print \"hey\") (print \"ho\"))", sexp(script));
    }

    #[test]
    fn test_logical_expr() {
        assert_eq!("(or 42 (+ 4 2))", sexp_expr("42 or 4 + 2"));
    }

    #[test]
    fn test_while() {
        let script = r#"
        while (true)
            print "y";
        "#;
        assert_eq!("(while true (print \"y\"))", sexp(script));
    }

    #[test]
    fn test_for() {
        let script = r#"
        for (var i = 0; i < 10; i = i + 1)
            print i;
        "#;
        assert_eq!(
            "(block (var i 0) (while (< i 10) (block (print i) (= i (+ i 1)))))",
            sexp(script)
        );

        let script = r#"
        for (;;)
            print "y";
        "#;
        assert_eq!("(block (while true (print \"y\")))", sexp(script));
    }

    #[test]
    fn test_break() {
        let script = r#"
        while (true) break;
        "#;
        assert_eq!("(while true break)", sexp(script));

        assert!(parse("break;").is_err());
        assert!(parse("if (1) break;").is_err())
    }

    #[test]
    fn test_call() {
        assert_eq!("(call foo )", sexp_expr("foo()"));
        assert_eq!("(call add 1 2)", sexp_expr("add(1, 2)"));
        assert_eq!("(call add 1 2 (! 42))", sexp_expr("add(1, 2, !42)"));
    }
}
