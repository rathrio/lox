use std::fmt::Display;

pub type Line = usize;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Single-character tokens
    LeftParen(Line),
    RightParen(Line),
    LeftBrace(Line),
    RightBrace(Line),
    Comma(Line),
    Dot(Line),
    Minus(Line),
    Plus(Line),
    Semicolon(Line),
    Colon(Line),
    Query(Line),
    Slash(Line),
    Star(Line),

    // One or two character tokens
    Bang(Line),
    BangEqual(Line),
    Equal(Line),
    EqualEqual(Line),
    Greater(Line),
    GreaterEqual(Line),
    Less(Line),
    LessEqual(Line),

    // Literals
    Identifier(Line, String),
    String(Line, String),
    Number(Line, f64),

    // Keywords
    And(Line),
    Class(Line),
    Else(Line),
    False(Line),
    Fun(Line),
    For(Line),
    If(Line),
    Nil(Line),
    Or(Line),
    Print(Line),
    Return(Line),
    Super(Line),
    This(Line),
    True(Line),
    Var(Line),
    While(Line),
    Break(Line),
    Eof(Line),

    // Error
    Error(Line, String),
}

impl Token {
    pub fn id(&self) -> &str {
        match self {
            Token::Identifier(_, id) => id,
            Token::This(_) => "this",
            Token::Super(_) => "super",
            t => panic!("{:?} is not an identifier", t),
        }
    }

    pub fn is_infix_op(&self) -> bool {
        matches!(
            self,
            Token::Minus(_)
                | Token::Plus(_)
                | Token::Slash(_)
                | Token::Star(_)
                | Token::BangEqual(_)
                | Token::EqualEqual(_)
                | Token::Greater(_)
                | Token::GreaterEqual(_)
                | Token::Less(_)
                | Token::LessEqual(_)
                | Token::And(_)
                | Token::Or(_)
                | Token::Comma(_)
                | Token::Query(_)
                | Token::Equal(_)
                | Token::LeftParen(_)
                | Token::Dot(_)
        )
    }

    pub fn is_prefix_op(&self) -> bool {
        matches!(self, Token::Bang(_) | Token::Minus(_))
    }

    pub fn is_eof(&self) -> bool {
        matches!(self, Token::Eof(_))
    }

    pub fn is_identifier(&self) -> bool {
        matches!(self, Token::Identifier(_, _))
    }

    // TODO: there must be a more concise way to do this...
    pub fn line(&self) -> Line {
        match self {
            Token::LeftParen(line)
            | Token::RightParen(line)
            | Token::LeftBrace(line)
            | Token::RightBrace(line)
            | Token::Comma(line)
            | Token::Dot(line)
            | Token::Minus(line)
            | Token::Plus(line)
            | Token::Colon(line)
            | Token::Semicolon(line)
            | Token::Slash(line)
            | Token::Star(line)
            | Token::Bang(line)
            | Token::BangEqual(line)
            | Token::Equal(line)
            | Token::EqualEqual(line)
            | Token::Greater(line)
            | Token::GreaterEqual(line)
            | Token::Less(line)
            | Token::LessEqual(line)
            | Token::Identifier(line, _)
            | Token::String(line, _)
            | Token::Number(line, _)
            | Token::And(line)
            | Token::Class(line)
            | Token::Else(line)
            | Token::False(line)
            | Token::Fun(line)
            | Token::For(line)
            | Token::If(line)
            | Token::Nil(line)
            | Token::Or(line)
            | Token::Print(line)
            | Token::Return(line)
            | Token::Super(line)
            | Token::This(line)
            | Token::True(line)
            | Token::Var(line)
            | Token::While(line)
            | Token::Eof(line)
            | Token::Query(line)
            | Token::Break(line)
            | Token::Error(line, _) => *line,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::LeftParen(_) => write!(f, "("),
            Token::RightParen(_) => write!(f, ")"),
            Token::LeftBrace(_) => write!(f, "{{"),
            Token::RightBrace(_) => write!(f, "}}"),
            Token::Comma(_) => write!(f, ","),
            Token::Dot(_) => write!(f, "."),
            Token::Minus(_) => write!(f, "-"),
            Token::Plus(_) => write!(f, "+"),
            Token::Semicolon(_) => write!(f, ";"),
            Token::Slash(_) => write!(f, "/"),
            Token::Star(_) => write!(f, "*"),
            Token::Bang(_) => write!(f, "!"),
            Token::BangEqual(_) => write!(f, "!="),
            Token::Equal(_) => write!(f, "="),
            Token::EqualEqual(_) => write!(f, "=="),
            Token::Greater(_) => write!(f, ">"),
            Token::GreaterEqual(_) => write!(f, ">="),
            Token::Less(_) => write!(f, "<"),
            Token::LessEqual(_) => write!(f, "<="),
            Token::Identifier(_, id) => write!(f, "{}", id),
            Token::String(_, s) => write!(f, "\"{}\"", s),
            Token::Number(_, n) => write!(f, "{}", n),
            Token::And(_) => write!(f, "and"),
            Token::Class(_) => write!(f, "class"),
            Token::Else(_) => write!(f, "else"),
            Token::False(_) => write!(f, "false"),
            Token::Fun(_) => write!(f, "fun"),
            Token::For(_) => write!(f, "for"),
            Token::If(_) => write!(f, "if"),
            Token::Nil(_) => write!(f, "nil"),
            Token::Or(_) => write!(f, "or"),
            Token::Print(_) => write!(f, "print"),
            Token::Return(_) => write!(f, "return"),
            Token::Super(_) => write!(f, "super"),
            Token::This(_) => write!(f, "this"),
            Token::True(_) => write!(f, "true"),
            Token::Var(_) => write!(f, "var"),
            Token::While(_) => write!(f, "while"),
            Token::Break(_) => write!(f, "break"),
            Token::Eof(_) => write!(f, "EOF"),
            Token::Colon(_) => write!(f, ":"),
            Token::Query(_) => write!(f, "?"),
            Token::Error(_, _) => Ok(()),
        }
    }
}

#[derive(Debug)]
pub struct Lexer {
    source: Vec<char>,
    tokens: Vec<Token>,
    current: usize,
    start: usize,
    line: usize,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().into_iter().collect(),
            tokens: Vec::new(),
            current: 0,
            start: 0,
            line: 1,
        }
    }

    pub fn lex_tokens(&mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.lex_token();
        }

        self.tokens.push(Token::Eof(self.line));
        self.tokens.clone()
    }

    fn lex_token(&mut self) {
        let c = self.advance();

        match c {
            c if c.is_ascii_digit() => self.digit(),
            c if c.is_alphabetic() => self.identifier(),
            '(' => self.tokens.push(Token::LeftParen(self.line)),
            ')' => self.tokens.push(Token::RightParen(self.line)),
            '{' => self.tokens.push(Token::LeftBrace(self.line)),
            '}' => self.tokens.push(Token::RightBrace(self.line)),
            ',' => self.tokens.push(Token::Comma(self.line)),
            '.' => self.tokens.push(Token::Dot(self.line)),
            '-' => self.tokens.push(Token::Minus(self.line)),
            '+' => self.tokens.push(Token::Plus(self.line)),
            ';' => self.tokens.push(Token::Semicolon(self.line)),
            ':' => self.tokens.push(Token::Colon(self.line)),
            '?' => self.tokens.push(Token::Query(self.line)),
            '*' => self.tokens.push(Token::Star(self.line)),
            '!' => {
                if self.is_next('=') {
                    self.tokens.push(Token::BangEqual(self.line));
                } else {
                    self.tokens.push(Token::Bang(self.line));
                }
            }
            '=' => {
                if self.is_next('=') {
                    self.tokens.push(Token::EqualEqual(self.line));
                } else {
                    self.tokens.push(Token::Equal(self.line));
                }
            }
            '<' => {
                if self.is_next('=') {
                    self.tokens.push(Token::LessEqual(self.line));
                } else {
                    self.tokens.push(Token::Less(self.line));
                }
            }
            '>' => {
                if self.is_next('=') {
                    self.tokens.push(Token::GreaterEqual(self.line));
                } else {
                    self.tokens.push(Token::Greater(self.line));
                }
            }
            '/' => {
                if self.is_next('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.tokens.push(Token::Slash(self.line));
                }
            }
            ' ' => {}
            '\r' => {}
            '\t' => {}
            '\n' => self.line += 1,
            '"' => self.string(),
            _ => self.tokens.push(Token::Error(
                self.line,
                format!("Unknown character '{}'", c),
            )),
        };
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            self.tokens
                .push(Token::Error(self.line, "Unterminated string.".to_string()));
            return;
        }

        self.advance(); // The closing "

        // Trim the surrounding quotes
        let value: String = self.source[self.start + 1..self.current - 1]
            .iter()
            .collect::<String>();

        self.tokens.push(Token::String(self.line, value));
    }

    fn digit(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance();

            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let value: f64 = self.source[self.start..self.current]
            .iter()
            .collect::<String>()
            .parse::<f64>()
            .unwrap();

        self.tokens.push(Token::Number(self.line, value));
    }

    fn identifier(&mut self) {
        while is_valid_identifier_char(self.peek()) {
            self.advance();
        }

        let value: String = self.source[self.start..self.current]
            .iter()
            .collect::<String>();

        let token = self.keyword_or_identifier(&value);
        self.tokens.push(token);
    }

    fn keyword_or_identifier(&self, word: &str) -> Token {
        match word {
            "and" => Token::And(self.line),
            "class" => Token::Class(self.line),
            "else" => Token::Else(self.line),
            "false" => Token::False(self.line),
            "for" => Token::For(self.line),
            "fun" => Token::Fun(self.line),
            "if" => Token::If(self.line),
            "nil" => Token::Nil(self.line),
            "or" => Token::Or(self.line),
            "print" => Token::Print(self.line),
            "return" => Token::Return(self.line),
            "super" => Token::Super(self.line),
            "this" => Token::This(self.line),
            "true" => Token::True(self.line),
            "var" => Token::Var(self.line),
            "while" => Token::While(self.line),
            "break" => Token::Break(self.line),
            _ => Token::Identifier(self.line, word.to_string()),
        }
    }

    fn is_next(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.source[self.current] != expected {
            return false;
        }

        self.current += 1;

        true
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> char {
        let c = self.source[self.current];
        self.current += 1;
        c
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        self.source[self.current]
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }

        self.source[self.current + 1]
    }
}

fn is_valid_identifier_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(input: &str) -> Vec<Token> {
        let mut lexer = Lexer::new(input);
        lexer.lex_tokens()
    }

    #[test]
    fn test_identifier() {
        assert_eq!(
            &Token::Identifier(1, "foo_bar".to_string()),
            lex("foo_bar").get(0).unwrap()
        );
    }
}
