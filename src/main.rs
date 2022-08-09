use clap::Parser as ClapParser;
use lox::interpreter::Interpreter;
use lox::parser::Parser;
use std::io::{self, stdout, Write};

#[derive(ClapParser, Debug, Default, Clone)]
#[clap(author, version, about, long_about = None)]
pub struct Args {
    file: Option<String>,
}

fn run(interpreter: &mut Interpreter<io::Stdout>, script: String) {
    let script = script.trim();
    if script.ends_with(';') || script.ends_with('}') {
        run_statment(script.into(), interpreter);
    } else {
        run_expr(script.into(), interpreter);
    }
}

fn run_statment(script: String, interpreter: &mut Interpreter<io::Stdout>) {
    match Parser::parse_str(&script) {
        Ok(program) => match interpreter.interpret(&program) {
            Ok(_) => (),
            Err(error) => println!("runtime error: {}", error.report),
        },
        Err(error) => println!("syntax error: {}", error.report),
    };
}

fn run_expr(script: String, interpreter: &mut Interpreter<io::Stdout>) {
    match Parser::parse_expr_str(&script) {
        Ok(expr) => match interpreter.interpret_expr(&expr, interpreter.env.clone()) {
            Ok(v) => println!("=> {}", v),
            Err(error) => println!("runtime error: {}", error.report),
        },

        Err(error) => println!("syntax error: {}", error.report),
    };
}

fn run_file(path: &str) {
    let script = std::fs::read_to_string(path).unwrap();
    let mut interpreter = Interpreter::new(io::stdout());
    run(&mut interpreter, script);
}

fn run_prompt() {
    let mut interpreter = Interpreter::new(io::stdout());

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
