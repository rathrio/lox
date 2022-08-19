use std::collections::HashMap;

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

fn error<T>(report: impl Into<String>, line: Line) -> Result<T> {
    Err(ParserError::new(report, line))
}

fn try_into_assign(expr: Expr, line: Line) -> Result<Expr> {
    if let Expr::Binary(lhs, Token::Equal(_), rhs) = expr {
        if let Expr::Var(name, _) = *lhs {
            return Ok(Expr::Assign(name, rhs));
        }
    }

    Err(ParserError::new("invalid LHS in assignment", line))
}

/// Flattens a comma expression tree into an expression list.
fn try_into_args(expr: Expr) -> Result<Vec<Expr>> {
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

#[derive(Debug)]
enum ScopeKind {
    Function,
    Block,
}

#[derive(Debug)]
struct Scope {
    kind: ScopeKind,
    defs: HashMap<String, bool>,
}

impl Scope {
    fn new(kind: ScopeKind) -> Self {
        let defs = HashMap::new();
        Self { kind, defs }
    }

    fn is_declared(&self, id: &str) -> bool {
        self.defs.contains_key(id)
    }

    fn declare(&mut self, id: String) {
        self.defs.insert(id, false);
    }

    fn define(&mut self, id: String) {
        self.defs.insert(id, true);
    }
}

type Result<T> = core::result::Result<T, ParserError>;

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    scopes: Vec<Scope>,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        let mut s = Lexer::new(input.to_string());
        let tokens = s.lex_tokens();
        let scopes = vec![];
        Self { tokens, scopes }
    }

    pub fn parse_str(input: &str) -> Result<Program> {
        Self::new(input).parse_program()
    }

    pub fn parse_expr_str(input: &str) -> Result<Expr> {
        Self::new(input).parse_expr(0)
    }

    fn parse_program(&mut self) -> Result<Program> {
        let mut stmts = Vec::new();

        while !self.peek().is_eof() {
            stmts.push(self.parse_decl(false)?);
        }

        Ok(Program { stmts })
    }

    fn parse_decl(&mut self, is_break_allowed: bool) -> Result<Stmt> {
        match self.peek() {
            Token::Class(_) => self.parse_class_decl(),
            Token::Var(_) => self.parse_var_decl(),
            Token::Fun(_) => self.parse_fun_decl(),
            _ => self.parse_stmt(is_break_allowed),
        }
    }

    fn parse_class_decl(&mut self) -> Result<Stmt> {
        // consume class token
        self.next();

        let class_name = self.parse_identifier()?;
        self.declare_and_define(&class_name)?;

        self.expect_left_brace("before class body")?;

        let mut methods = Vec::new();
        loop {
            match self.peek() {
                Token::RightBrace(_) => break,
                Token::Identifier(_, _) => {
                    let fun = self.parse_fun()?;
                    methods.push(fun);
                }
                t => {
                    return error(
                        format!(
                            "expected a method name starting a method declaration, but got \"{}\"",
                            t
                        ),
                        t.line(),
                    )
                }
            }
        }

        self.expect_right_brace("after class body")?;

        Ok(Stmt::Class(class_name, methods))
    }

    fn parse_var_decl(&mut self) -> Result<Stmt> {
        // consume var token
        self.next();

        let id = self.next();
        self.declare(&id)?;

        if let Token::Semicolon(_) = self.peek() {
            self.next();
            return Ok(Stmt::VarDecl(id, Expr::Nil));
        }

        let eq = self.next();
        let decl = match (&id, &eq) {
            (Token::Identifier(_, _), Token::Equal(_)) => {
                self.define(&id)?;
                Ok(Stmt::VarDecl(id, self.parse_expr(0)?))
            }
            (t, _) => error("invalid variable declaration", t.line()),
        };

        self.expect_semi("after variable declaration")?;
        decl
    }

    fn parse_fun_decl(&mut self) -> Result<Stmt> {
        // consume fun token
        self.next();
        self.parse_fun()
    }

    fn parse_fun(&mut self) -> Result<Stmt> {
        if let Token::LeftParen(line) = self.peek() {
            return error(
                "anonymous functions cannot be declared in this context",
                line,
            );
        }

        let name = self.parse_identifier()?;
        self.declare_and_define(&name)?;

        let params = self.parse_fun_params(name.line())?;
        let stmts = self.parse_fun_body(&params)?;

        Ok(Stmt::FunDecl(name, params, stmts))
    }

    fn parse_fun_body(&mut self, defs: &[Token]) -> Result<Vec<Stmt>> {
        let body = match self.peek() {
            Token::LeftBrace(_) => self.parse_block(false, defs, ScopeKind::Function)?,
            t => return error("expected { before function body", t.line()),
        };
        let stmts = match body {
            Stmt::Block(list) => list,
            _ => unreachable!(),
        };
        Ok(stmts)
    }

    fn parse_fun_params(&mut self, line: Line) -> Result<Vec<Token>> {
        self.expect_left_paren("after function name")?;
        let mut params = Vec::new();
        loop {
            if let Token::RightParen(_) = self.peek() {
                break;
            }

            params.push(self.parse_identifier()?);
            if params.len() >= 255 {
                return error("can't have more than 255 parameters", line);
            }

            // Let's allow trailing commas
            if let Token::Comma(_) = self.peek() {
                self.next();
            }
        }
        self.next();
        Ok(params)
    }

    fn parse_identifier(&mut self) -> Result<Token> {
        match self.next() {
            t if matches!(t, Token::Identifier(_, _)) => Ok(t),
            t => error(format!("expected an identifier, got {:?}", t), t.line()),
        }
    }

    fn parse_stmt(&mut self, is_break_allowed: bool) -> Result<Stmt> {
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
            Token::LeftBrace(_) => self.parse_block(is_break_allowed, &[], ScopeKind::Block),
            Token::Return(_) => self.parse_return(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_return(&mut self) -> Result<Stmt> {
        let ret = self.next();

        if self.current_scope().is_none()
            || !matches!(self.current_scope().unwrap().kind, ScopeKind::Function)
        {
            return error("cannot return from top level code", ret.line());
        }

        let expr = match self.peek() {
            Token::Semicolon(_) => Expr::Nil,
            _ => self.parse_expr(0)?,
        };
        self.expect_semi("after return statement")?;
        Ok(Stmt::Return(ret, expr))
    }

    fn parse_if_stmt(&mut self, is_break_allowed: bool) -> Result<Stmt> {
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

    fn parse_while_stmt(&mut self) -> Result<Stmt> {
        // consume while token
        self.next();
        self.expect_left_paren("after while")?;
        let condition = self.parse_expr(0)?;
        self.expect_right_paren("after while condition")?;
        let stmt = self.parse_stmt(true)?;

        Ok(Stmt::While(condition, Box::new(stmt)))
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt> {
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

    fn parse_break_stmt(&mut self) -> Result<Stmt> {
        self.next();
        self.expect_semi("after break")?;
        Ok(Stmt::Break)
    }

    fn parse_print_stmt(&mut self) -> Result<Stmt> {
        // consume print token
        self.next();
        let expr = self.parse_expr(0)?;
        self.expect_semi("after print statement")?;
        Ok(Stmt::Print(expr))
    }

    fn parse_block(
        &mut self,
        is_break_allowed: bool,
        defs: &[Token],
        kind: ScopeKind,
    ) -> Result<Stmt> {
        // consume {
        self.next();
        self.enter_scope(kind);

        for def in defs {
            self.declare_and_define(def)?;
        }

        let mut stmts = Vec::new();
        loop {
            match self.peek() {
                Token::RightBrace(_) => break,
                Token::Eof(_) => break,
                _ => (),
            };

            stmts.push(self.parse_decl(is_break_allowed)?);
        }

        self.exit_scope();

        match self.next() {
            Token::RightBrace(_) => Ok(Stmt::Block(stmts)),
            t => error(format!("expected a \"}}\", but got \"{}\"", t), t.line()),
        }
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt> {
        let expr = self.parse_expr(0)?;
        self.expect_semi("after expression statement")?;
        Ok(Stmt::Expr(expr))
    }

    fn parse_expr(&mut self, min_bp: u8) -> Result<Expr> {
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
            Token::Identifier(line, name) => {
                let id = Token::Identifier(line, name);
                Ok(Expr::Var(id.clone(), self.resolve_depth(&id)))
            }
            Token::Fun(line) => self.parse_anon_fn(line),
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

            // consume the operator we peeked at the beginning of the loop
            self.next();

            match op {
                Token::Query(_) => {
                    let conclusion = self.parse_expr(0)?;
                    if let Token::Colon(_) = self.next() {
                        let alternate = self.parse_expr(r_bp)?;
                        expr = Expr::Ternary(
                            Box::new(expr),
                            Box::new(conclusion),
                            Box::new(alternate),
                        );
                    } else {
                        return error("missing \":\" in ternary", op.line());
                    }
                }
                Token::LeftParen(line) => {
                    expr = self.parse_call(line, expr, op.clone())?;
                }
                Token::Dot(_) => {
                    expr = self.parse_get(expr)?;
                }
                _ => {
                    let rhs = self.parse_expr(r_bp)?;
                    expr = Expr::Binary(Box::new(expr), op.clone(), Box::new(rhs));

                    if let Token::Equal(_) = op {
                        expr = try_into_assign(expr, op.line())?;
                    }
                }
            }
        }

        Ok(expr)
    }

    fn parse_call(&mut self, line: usize, expr: Expr, op: Token) -> Result<Expr> {
        let args = if let Token::RightParen(_) = self.peek() {
            Vec::new()
        } else {
            try_into_args(self.parse_expr(0)?)?
        };

        if args.len() >= 255 {
            return error("can't have more than 255 arguments", line);
        }

        self.expect_right_paren("after call args")?;

        Ok(Expr::Call(Box::new(expr), op, args))
    }

    fn parse_get(&mut self, expr: Expr) -> Result<Expr> {
        let name = self.parse_identifier()?;
        Ok(Expr::Get(Box::new(expr), name))
    }

    fn parse_anon_fn(&mut self, line: Line) -> Result<Expr> {
        let params = self.parse_fun_params(line)?;
        let body = self.parse_fun_body(&params)?;
        Ok(Expr::AnonFunDecl(params, body))
    }

    fn peek(&self) -> Token {
        self.tokens.get(0).expect("overshot EOF").clone()
    }

    fn next(&mut self) -> Token {
        self.tokens.drain(0..1).next().expect("overshot EOF")
    }

    fn expect_left_paren(&mut self, pos_msg: &str) -> Result<()> {
        match self.next() {
            Token::LeftParen(_) => Ok(()),
            t => error(
                format!("expected \"(\" {}, but got \"{}\" instead", pos_msg, t),
                t.line(),
            )?,
        }
    }

    fn expect_right_paren(&mut self, pos_msg: &str) -> Result<()> {
        match self.next() {
            Token::RightParen(_) => Ok(()),
            t => error(
                format!("expected \")\" {}, but got \"{}\" instead", pos_msg, t),
                t.line(),
            )?,
        }
    }

    fn expect_left_brace(&mut self, pos_msg: &str) -> Result<()> {
        match self.next() {
            Token::LeftBrace(_) => Ok(()),
            t => error(
                format!("expected \"{{\" {}, but got \"{}\" instead", pos_msg, t),
                t.line(),
            )?,
        }
    }

    fn expect_right_brace(&mut self, pos_msg: &str) -> Result<()> {
        match self.next() {
            Token::RightBrace(_) => Ok(()),
            t => error(
                format!("expected \"}}\" {}, but got \"{}\" instead", pos_msg, t),
                t.line(),
            )?,
        }
    }

    fn expect_semi(&mut self, pos_msg: &str) -> Result<()> {
        match self.next() {
            Token::Semicolon(_) => Ok(()),
            t => error(
                format!("expected \";\" {}, but got \"{}\" instead", pos_msg, t),
                t.line(),
            )?,
        }
    }

    fn enter_scope(&mut self, kind: ScopeKind) {
        self.scopes.push(Scope::new(kind));
    }

    fn exit_scope(&mut self) {
        self.scopes.pop().expect("attempted to pop global scope");
    }

    fn declare(&mut self, id: &Token) -> Result<()> {
        if self.scopes.is_empty() {
            return Ok(());
        }

        if self
            .current_scope()
            .unwrap()
            .is_declared(id.to_string().as_str())
        {
            error(format!("variable \"{}\" already in scope", id), id.line())
        } else {
            self.current_scope().unwrap().declare(id.to_string());
            Ok(())
        }
    }

    fn define(&mut self, id: &Token) -> Result<()> {
        if self.scopes.is_empty() {
            return Ok(());
        }

        if self
            .current_scope()
            .unwrap()
            .is_declared(id.to_string().as_str())
        {
            self.current_scope().unwrap().define(id.to_string());
            Ok(())
        } else {
            error(
                format!("attempting to assign to undeclared variable {}", id),
                id.line(),
            )
        }
    }

    fn declare_and_define(&mut self, id: &Token) -> Result<()> {
        self.declare(id)?;
        self.define(id)
    }

    fn current_scope(&mut self) -> Option<&mut Scope> {
        if self.scopes.is_empty() {
            return None;
        }

        let size = self.scopes.len();
        Some(self.scopes.get_mut(size - 1).unwrap())
    }

    fn resolve_depth(&self, id: &Token) -> Option<u8> {
        for (depth, scope) in self.scopes.iter().rev().enumerate() {
            if scope.defs.contains_key(id.to_string().as_str()) {
                return Some(depth as u8);
            }
        }

        None
    }
}

fn infix_binding_power(op: &Token) -> Result<(u8, u8)> {
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
        Token::LeftParen(_) | Token::Dot(_) => Ok((17, 18)),
        t => error(format!("invalid infix operator {}", t), t.line()),
    }
}

fn prefix_binding_power(op: &Token) -> Result<((), u8)> {
    match op {
        Token::Minus(_) | Token::Bang(_) => Ok(((), 19)),
        t => error(format!("invalid prefix operator {}", t), t.line()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast;

    fn parse(input: &str) -> Result<Program> {
        Parser::parse_str(input)
    }

    fn sexp(input: &str) -> String {
        let program = parse(input).unwrap();
        ast::sexp(&program)
    }

    fn parse_expr(input: &str) -> Result<Expr> {
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
    fn test_var_decl() {
        assert_eq!("(var a nil)", sexp("var a;"));
        assert_eq!("(var a 42)", sexp("var a = 42;"));
        assert_eq!(
            "(var name (+ \"hi \" \"tadeus\"))",
            sexp("var name = \"hi \" + \"tadeus\";")
        );
    }

    #[test]
    fn test_assignment() {
        assert_eq!("(var a nil) (= a 42)", sexp("var a; a = 42;"));
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
        assert_eq!(
            "(fun add (a b c) ()) (call add (1 2 (! 42)))",
            sexp("fun add(a, b, c) {} add(1, 2, !42);")
        );
    }

    #[test]
    fn test_fun_decl() {
        let script = r#"
        fun add(a, b) {
            b = b + 1;
            print a + b;
        }
        "#;
        assert_eq!(
            "(fun add (a b) ((= b (+ b 1)) (print (+ a b))))",
            sexp(script)
        );
    }

    #[test]
    fn test_return() {
        let script = r#"
        fun add(a, b) {
            return a + b;
        }
        "#;
        assert_eq!("(fun add (a b) ((return (+ a b))))", sexp(script));
    }

    #[test]
    fn test_anon_fun() {
        assert_eq!(
            "(fun (x y) ((return (+ x y))))",
            sexp_expr("fun (x, y) { return x + y; }")
        );
    }

    #[test]
    fn test_anon_functions_as_expr_stmt() {
        let script = "fun () {};";
        assert!(parse(script).is_err());
    }

    #[test]
    fn test_naming_collision() {
        let script = r#"
        fun bad() {
            var a = "first";
            var a = "second";
        }
        "#;

        assert!(parse(script).is_err());
    }

    #[test]
    fn test_return_outside_function() {
        let script = "return 42;";
        assert!(parse(script).is_err());

        let script = "{ return 42; }";
        assert!(parse(script).is_err());
    }

    #[test]
    fn test_classes() {
        assert_eq!("(class Dog ())", sexp("class Dog {}"));

        let script = r#"
        class Breakfast {
            cook() {
              print "Eggs a-fryin'!";
            }

            serve(who) {
              print "Enjoy your breakfast, " + who + ".";
            }
        }
        "#;

        assert_eq!(
            r#"(class Breakfast ((fun cook () ((print "Eggs a-fryin'!"))) (fun serve (who) ((print (+ (+ "Enjoy your breakfast, " who) "."))))))"#,
            sexp(script)
        );
    }

    #[test]
    fn test_prop_access() {
        assert_eq!(
            "(. someObject someProperty)",
            sexp_expr("someObject.someProperty")
        );

        assert_eq!(
            "(call (. (call (. egg scramble) (3)) with) (cheddar))",
            sexp_expr("egg.scramble(3).with(cheddar)")
        );
    }
}
