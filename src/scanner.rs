use crate::token::Token;

#[derive(Debug)]
pub struct Scanner {
    source: Vec<char>,
    tokens: Vec<Token>,
    current: usize,
    start: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Self {
            source: source.chars().into_iter().collect(),
            tokens: Vec::new(),
            current: 0,
            start: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token::Eof(self.line));
        self.tokens.clone()
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            c if c.is_ascii_digit() => self.digit(),
            c if c.is_alphanumeric() => self.identifier(),
            '(' => self.tokens.push(Token::LeftParen(self.line)),
            ')' => self.tokens.push(Token::RightParen(self.line)),
            '{' => self.tokens.push(Token::LeftBrace(self.line)),
            '}' => self.tokens.push(Token::RightBrace(self.line)),
            ',' => self.tokens.push(Token::Comma(self.line)),
            '.' => self.tokens.push(Token::Dot(self.line)),
            '-' => self.tokens.push(Token::Minus(self.line)),
            '+' => self.tokens.push(Token::Plus(self.line)),
            ';' => self.tokens.push(Token::Semicolon(self.line)),
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
                .push(Token::Error(self.line, "Unterminated string".to_string()));
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
        while self.peek().is_alphanumeric() {
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
