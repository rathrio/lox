use fnv::FnvHashMap as HashMap;

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

type Result<T> = core::result::Result<T, Vec<ParserError>>;

fn error<T>(report: impl Into<String>, line: Line) -> Result<T> {
    Err(vec![ParserError::new(report, line)])
}

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    scopes: Vec<Scope>,
    current_fun: FunType,
    current_class: ClassType,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        let mut s = Lexer::new(input);
        let tokens = s.lex_tokens();
        let scopes = vec![];
        let current_class = ClassType::None;
        let current_fun = FunType::None;

        Self {
            tokens,
            scopes,
            current_fun,
            current_class,
        }
    }

    pub fn parse_str(input: &str) -> Result<Program> {
        Self::new(input).parse_program()
    }

    pub fn parse_expr_str(input: &str) -> Result<Expr> {
        Self::new(input).parse_expr(0)
    }

    fn parse_program(&mut self) -> Result<Program> {
        let mut stmts = Vec::new();

        let mut errors = Vec::new();

        while !self.peek().is_eof() {
            match self.parse_decl(false) {
                Ok(decl) => stmts.push(decl),
                Err(mut e) => {
                    // if let ParserError::Syntactic(_, _) = e.first().unwrap() {
                    self.try_error_recovery();
                    // }

                    errors.append(&mut e);
                }
            }
        }

        if errors.is_empty() {
            Ok(Program { stmts })
        } else {
            Err(errors)
        }
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

        let enclosing_class = self.current_class.clone();
        self.current_class = ClassType::Class;

        let class_name = self.parse_identifier(None)?;
        self.declare_and_define(&class_name)?;

        let superclass = match self.peek() {
            Token::Less(_) => {
                self.next();

                let superclass_name = self.parse_identifier(Some("Expect superclass name."))?;
                self.enter_scope();
                self.declare_and_define(&Token::Super(superclass_name.line()))?;

                if superclass_name.to_string() == class_name.to_string() {
                    return error(
                        format!(
                            "Error at '{}': A class can't inherit from itself.",
                            class_name
                        ),
                        superclass_name.line(),
                    );
                }

                self.current_class = ClassType::Subclass;
                Some(Expr::Var(superclass_name, Some(0)))
            }
            _ => None,
        };

        self.expect_left_brace("before class body")?;
        self.enter_scope();
        self.declare_and_define(&Token::This(class_name.line()))?;

        let mut methods = Vec::new();
        let mut class_methods = Vec::new();
        loop {
            match self.peek() {
                Token::RightBrace(_) => break,
                Token::Identifier(_, _) => methods.push(self.parse_fun(FunType::Method)?),
                Token::Class(_) => {
                    self.next();
                    class_methods.push(self.parse_fun(FunType::Function)?)
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
        self.exit_scope();

        if superclass.is_some() {
            self.exit_scope();
        }

        self.current_class = enclosing_class;

        Ok(Stmt::Class(class_name, superclass, methods, class_methods))
    }

    fn parse_var_decl(&mut self) -> Result<Stmt> {
        // consume var token
        self.next();

        let id = self.parse_identifier(Some("Expect variable name."))?;
        self.declare(&id)?;

        if let Token::Semicolon(_) = self.peek() {
            self.next();
            return Ok(Stmt::VarDecl(id, Expr::Nil));
        }

        let eq = self.next();
        let decl = match (&id, &eq) {
            (Token::Identifier(_, _), Token::Equal(_)) => {
                let rhs = self.parse_expr(0)?;
                self.define(&id)?;
                Ok(Stmt::VarDecl(id, rhs))
            }
            (t, _) => error("invalid variable declaration", t.line()),
        };

        self.expect_semi("after variable declaration")?;
        decl
    }

    fn parse_fun_decl(&mut self) -> Result<Stmt> {
        // consume fun token
        self.next();
        self.parse_fun(FunType::Function)
    }

    fn parse_fun(&mut self, fun_type: FunType) -> Result<Stmt> {
        if let Token::LeftParen(line) = self.peek() {
            return error(
                "anonymous functions cannot be declared in this context",
                line,
            );
        }

        let name = self.parse_identifier(None)?;
        self.declare_and_define(&name)?;

        self.current_fun = if matches!(fun_type, FunType::Method) && name.to_string() == "init" {
            FunType::Initializer
        } else {
            fun_type
        };

        let params = self.parse_fun_params()?;
        let stmts = self.parse_fun_body(&params)?;

        Ok(Stmt::FunDecl(name, params, stmts))
    }

    fn parse_fun_body(&mut self, defs: &[Token]) -> Result<Vec<Stmt>> {
        let body = match self.peek() {
            Token::LeftBrace(_) => self.parse_block(false, defs)?,
            t => {
                return error(
                    format!("Error at '{}': Expect '{{' before function body.", t),
                    t.line(),
                )
            }
        };
        let stmts = match body {
            Stmt::Block(list) => list,
            _ => unreachable!(),
        };

        Ok(stmts)
    }

    fn parse_fun_params(&mut self) -> Result<Vec<Token>> {
        self.expect_left_paren("before function params")?;
        let mut params = Vec::new();
        loop {
            if let Token::RightParen(_) = self.peek() {
                break;
            }

            params.push(self.parse_identifier(None)?);
            if params.len() > 255 {
                let offending_param = params.last().unwrap();
                return error(
                    format!(
                        "Error at '{}': Can't have more than 255 parameters.",
                        offending_param
                    ),
                    offending_param.line(),
                );
            }

            match self.peek() {
                // Let's allow trailing commas
                Token::Comma(_) => self.next(),
                Token::RightParen(_) => continue,
                t => {
                    return error(
                        format!("Error at '{}': Expect ')' after parameters.", t),
                        t.line(),
                    );
                }
            };

            // Let's allow trailing commas
            if let Token::Comma(_) = self.peek() {
                self.next();
            }
        }
        self.next();
        Ok(params)
    }

    fn parse_identifier(&mut self, custom_msg: Option<&str>) -> Result<Token> {
        match self.next() {
            t if matches!(t, Token::Identifier(_, _)) => Ok(t),
            t => error(
                format!(
                    "Error at '{}': {}",
                    t,
                    custom_msg.unwrap_or("Expect identifier.")
                ),
                t.line(),
            ),
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
            Token::LeftBrace(_) => self.parse_block(is_break_allowed, &[]),
            Token::Return(_) => self.parse_return(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_return(&mut self) -> Result<Stmt> {
        let ret = self.next();

        if matches!(self.current_fun, FunType::None) {
            return error(
                "Error at 'return': Can't return from top-level code.",
                ret.line(),
            );
        }

        let expr = match self.peek() {
            Token::Semicolon(_) => Expr::Nil,
            _ => self.parse_expr(0)?,
        };

        if matches!(self.current_fun, FunType::Initializer) && !matches!(expr, Expr::Nil) {
            return error(
                "Error at 'return': Can't return a value from an initializer.",
                ret.line(),
            );
        }

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

        self.enter_scope();
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

        let for_body = self.parse_stmt(true)?;

        let while_body = if let Some(i) = increment {
            let increment_stmt = Stmt::Expr(i);
            match for_body {
                Stmt::Block(stmts) => Stmt::Block(vec![Stmt::Block(stmts), increment_stmt]),
                _ => Stmt::Block(vec![Stmt::Block(vec![for_body]), increment_stmt]),
            }
        } else {
            for_body
        };

        block_stmts.push(Stmt::While(condition, Box::new(while_body)));
        self.exit_scope();

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

    fn parse_block(&mut self, is_break_allowed: bool, defs: &[Token]) -> Result<Stmt> {
        // consume {
        self.next();
        self.enter_scope();

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
            Token::This(line) => {
                if matches!(self.current_class, ClassType::None) {
                    return error(
                        "Error at 'this': Can't use 'this' outside of a class.",
                        line,
                    );
                } else {
                    Ok(Expr::This(Token::This(line)))
                }
            }
            Token::Number(_, number) => Ok(Expr::Number(number)),
            Token::String(_, string) => Ok(Expr::Str(string)),
            Token::True(_) => Ok(Expr::Bool(true)),
            Token::False(_) => Ok(Expr::Bool(false)),
            Token::Nil(_) => Ok(Expr::Nil),
            Token::Identifier(line, name) => {
                let id = Token::Identifier(line, name);

                if self
                    .current_scope()
                    .map(|scope| {
                        scope
                            .defs
                            .get(&id.to_string())
                            .map(|def| !def)
                            .unwrap_or(false)
                    })
                    .unwrap_or(false)
                {
                    error(
                        format!(
                            "Error at '{}': Can't read local variable in its own initializer.",
                            id
                        ),
                        line,
                    )
                } else {
                    Ok(Expr::Var(id.clone(), self.resolve_depth(&id)))
                }
            }
            Token::Fun(_) => self.parse_anon_fn(),
            Token::LeftParen(_) => {
                let lhs = self.parse_expr(0)?;

                match self.next() {
                    Token::RightParen(_) => Ok(Expr::Grouping(Box::new(lhs))),
                    t => error(format!("expected a \")\", but got \"{}\"", t), t.line()),
                }
            }
            Token::Super(line) => self.parse_super(line),
            Token::Error(line, msg) => error(format!("Error: {}", msg), line),
            t => error(format!("Error at '{}': Expect expression.", t), t.line()),
        }?;

        loop {
            let op = match self.peek() {
                t if t.is_infix_op() => t,
                // Not super fond of this approach here. I'm looking at what
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
                    expr = self.parse_ternary(expr, r_bp, op.line())?;
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

    fn parse_super(&mut self, line: usize) -> Result<Expr> {
        if matches!(self.current_class, ClassType::None) {
            return error(
                "Error at 'super': Can't use 'super' outside of a class.",
                line,
            );
        }

        if !matches!(self.current_class, ClassType::Subclass) {
            return error(
                "Error at 'super': Can't use 'super' in a class with no superclass.",
                line,
            );
        }

        match self.peek() {
            Token::Dot(_) => {
                self.next();
                let method = self.parse_identifier(Some("Expect superclass method name."))?;
                let id = Token::Super(line);

                Ok(Expr::Super(id.clone(), method, self.resolve_depth(&id)))
            }
            t => error(format!("Error at '{}': Expect '.' after 'super'.", t), line),
        }
    }

    fn parse_ternary(&mut self, condition: Expr, r_bp: u8, line: Line) -> Result<Expr> {
        let conclusion = self.parse_expr(0)?;

        if let Token::Colon(_) = self.next() {
            let alternate = self.parse_expr(r_bp)?;
            Ok(Expr::Ternary(
                Box::new(condition),
                Box::new(conclusion),
                Box::new(alternate),
            ))
        } else {
            error("missing \":\" in ternary", line)
        }
    }

    fn parse_call(&mut self, line: usize, expr: Expr, op: Token) -> Result<Expr> {
        let args = if let Token::RightParen(_) = self.peek() {
            Vec::new()
        } else {
            try_into_args(self.parse_expr(0)?)?
        };

        if args.len() > 255 {
            return error("Can't have more than 255 arguments.".to_string(), line);
        }

        self.expect_right_paren("after call args")?;

        Ok(Expr::Call(Box::new(expr), op, args))
    }

    fn parse_get(&mut self, expr: Expr) -> Result<Expr> {
        let name = self.parse_identifier(Some("Expect property name after '.'."))?;
        Ok(Expr::Get(Box::new(expr), name))
    }

    fn parse_anon_fn(&mut self) -> Result<Expr> {
        let enclosing_fun = self.current_fun.clone();
        self.current_fun = FunType::Function;
        let params = self.parse_fun_params()?;
        let body = self.parse_fun_body(&params)?;
        self.current_fun = enclosing_fun;
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

    fn enter_scope(&mut self) {
        self.scopes.push(Scope::new());
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
            error(
                format!(
                    "Error at '{}': Already a variable with this name in this scope.",
                    id
                ),
                id.line(),
            )
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

    fn try_error_recovery(&mut self) {
        while !self.peek().is_eof() {
            match self.peek() {
                Token::Class(_)
                | Token::Fun(_)
                | Token::For(_)
                | Token::If(_)
                | Token::Nil(_)
                | Token::Print(_)
                | Token::Return(_)
                | Token::Var(_)
                | Token::While(_) => break,
                _ => {
                    self.next();
                }
            }
        }
    }
}

#[derive(Debug)]
struct Scope {
    defs: HashMap<String, bool>,
}

impl Scope {
    fn new() -> Self {
        let defs = HashMap::default();
        Self { defs }
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

#[derive(Debug, Clone)]
enum ClassType {
    None,
    Class,
    Subclass,
}

#[derive(Debug, Clone)]
enum FunType {
    None,
    Function,
    Method,
    Initializer,
}

fn try_into_assign(expr: Expr, line: Line) -> Result<Expr> {
    if let Expr::Binary(lhs, Token::Equal(_), rhs) = expr {
        match *lhs {
            Expr::Var(name, depth) => return Ok(Expr::Assign(name, rhs, depth)),
            Expr::Get(e, name) => return Ok(Expr::Set(e, name, rhs)),
            _ => (),
        }
    }

    Err(vec![ParserError::new(
        "Error at '=': Invalid assignment target.".to_string(),
        line,
    )])
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

fn infix_binding_power(op: &Token) -> Result<(u8, u8)> {
    match op {
        Token::Equal(_) => Ok((2, 1)),
        Token::Comma(_) => Ok((3, 4)),
        Token::Or(_) | Token::And(_) => Ok((5, 6)),
        Token::Query(_) => Ok((8, 7)),
        Token::BangEqual(_) | Token::EqualEqual(_) => Ok((9, 10)),
        Token::Greater(_) | Token::GreaterEqual(_) | Token::Less(_) | Token::LessEqual(_) => {
            Ok((11, 12))
        }
        Token::Minus(_) | Token::Plus(_) => Ok((13, 14)),
        Token::Slash(_) | Token::Star(_) => Ok((15, 16)),
        Token::LeftParen(_) | Token::Dot(_) => Ok((20, 21)),
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
mod tests;
