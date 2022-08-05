use std::io::{stdout, Write};

use clap::Parser;
use lox::scanner::{Scanner, Token};

#[derive(Parser, Debug, Default, Clone)]
#[clap(author, version, about, long_about = None)]
pub struct Args {
    /// Will read from STDIN if omitted
    file: Option<String>,
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
