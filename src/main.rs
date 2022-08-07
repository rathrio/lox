use clap::Parser as ClapParser;
use lox::interpreter::Interpreter;
use lox::parser::Parser;
use std::io::{stdout, Write};

#[derive(ClapParser, Debug, Default, Clone)]
#[clap(author, version, about, long_about = None)]
pub struct Args {
    file: Option<String>,
}

fn run(interpreter: &mut Interpreter, script: String) {
    match Parser::parse_str(&script) {
        Ok(program) => match interpreter.interpret(&program) {
            Ok(_) => (),
            Err(error) => println!("runtime error: {}", error.report),
        },
        Err(error) => println!("syntax error: {}", error.report),
    };
}

fn run_file(path: &str) {
    let script = std::fs::read_to_string(path).unwrap();
    let mut interpreter = Interpreter::new();
    run(&mut interpreter, script);
}

fn run_prompt() {
    let mut interpreter = Interpreter::new();

    loop {
        print!("> ");
        stdout().flush().unwrap();
        let mut buffer = String::new();
        std::io::stdin()
            .read_line(&mut buffer)
            .expect("failed to read string");
        run(&mut interpreter, buffer);
    }
}

fn main() {
    let args = Args::parse();

    match args.file {
        Some(path) => run_file(&path),
        None => run_prompt(),
    }
}
