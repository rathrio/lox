use std::fmt::Display;

pub type Line = usize;

#[derive(Debug, Clone)]
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
    Slash(Line),
    Star(Line),

    // One or two character tokensjjjjjjjjjjjjjjjj
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
    Eof(Line),

    // Error
    Error(Line, String),
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
            Token::Eof(_) => write!(f, ""),
            Token::Error(_, _) => Ok(()),
        }
    }
}
