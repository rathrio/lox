use clap::Parser as ClapParser;
use lox::ast;
use lox::parser::Parser;
use lox::{interpreter::Interpreter, lexer::Lexer};
use std::io::{self, stdout, Write};

#[derive(ClapParser, Debug, Default, Clone)]
#[clap(author, version, about, long_about = None)]
pub struct Args {
    file: Option<String>,

    #[clap(long)]
    tokens: bool,

    #[clap(long)]
    sexp: bool,

    #[clap(long)]
    ast: bool,
}

fn main() {
    let args = Args::parse();

    match &args.file {
        Some(path) => run_file(path, &args),
        None => run_prompt(),
    }
}

fn run(script: String, interpreter: &mut Interpreter<io::Stdout>) {
    let script = script.trim();
    if script.ends_with(';') || script.ends_with('}') {
        run_program(script.into(), interpreter);
    } else {
        run_expr(script.into(), interpreter);
    }
}

fn run_program(script: String, interpreter: &mut Interpreter<io::Stdout>) {
    match Parser::parse_str(&script) {
        Ok(program) => match interpreter.interpret(&program) {
            Ok(_) => (),
            Err(error) => {
                eprintln!("{}\n[line {}]", error.report, error.line);
                std::process::exit(70);
            }
        },
        Err(error) => {
            eprintln!("[line {}] {}", error.line, error.report);
            std::process::exit(65);
        }
    };
}

fn run_expr(script: String, interpreter: &mut Interpreter<io::Stdout>) {
    match Parser::parse_expr_str(&script) {
        Ok(expr) => match interpreter.interpret_expr(&expr, interpreter.env.clone()) {
            Ok(v) => println!("=> {}", v),
            Err(error) => println!("{}", error.report),
        },

        Err(error) => println!("syntax error: {}", error.report),
    };
}

fn run_file(path: &str, args: &Args) {
    let script = std::fs::read_to_string(path).unwrap();

    if args.tokens {
        let mut lexer = Lexer::new(&script);
        println!("{:#?}", lexer.lex_tokens());
        return;
    }

    if args.ast {
        let program = Parser::parse_str(&script).unwrap();
        println!("{:#?}", program);
        return;
    }

    if args.sexp {
        let program = Parser::parse_str(&script).unwrap();
        println!("{}", ast::sexp(&program));
        return;
    }

    let mut interpreter = Interpreter::new(io::stdout());
    run_program(script, &mut interpreter);
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
        run(buffer, &mut interpreter);
    }
}
