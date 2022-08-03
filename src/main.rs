use std::io::{stdout, Write};

use clap::Parser;

#[derive(Parser, Debug, Default, Clone)]
#[clap(author, version, about, long_about = None)]
pub struct Args {
    /// Will read from STDIN if omitted
    file: Option<String>,
}

type Line = usize;

#[derive(Debug, Clone)]
enum Token {
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
    Identifier(Line),
    String(Line, String),
    Number(Line, u64),

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
    Eof(Line),

    // Error
    Error(Line, String),
}

#[derive(Debug)]
struct ScannerError {}

#[derive(Debug)]
struct Scanner {
    source: Vec<char>,
    tokens: Vec<Token>,
    current: usize,
    start: usize,
    line: usize,
}

impl Scanner {
    fn new(source: String) -> Self {
        Self {
            source: source.chars().into_iter().collect(),
            tokens: Vec::new(),
            current: 0,
            start: 0,
            line: 1,
        }
    }

    fn scan_tokens(&mut self) -> Vec<Token> {
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

    fn is_next(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.source[self.current] != expected {
            return false;
        }

        self.current += 1;
        return true;
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
}

fn run(script: String) {
    let mut scanner = Scanner::new(script);
    let tokens: Vec<Token> = scanner.scan_tokens();

    for token in tokens {
        println!("{:?}", token);
    }
}

fn run_file(path: &str) {
    let script = std::fs::read_to_string(path).unwrap();
    run(script);
}

fn run_prompt() {
    loop {
        print!("> ");
        stdout().flush().unwrap();
        let mut buffer = String::new();
        std::io::stdin()
            .read_line(&mut buffer)
            .expect("failed to read string");
        run(buffer);
    }
}

fn main() {
    let args = Args::parse();

    match args.file {
        Some(path) => run_file(&path),
        None => run_prompt(),
    }
}