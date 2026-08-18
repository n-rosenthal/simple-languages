//! S/src/main.rs
//! Ponto de entrada do interpretador de `S`.
pub mod Scanner;

use Scanner::Evaluator::type_check_and_eval;
use Scanner::Lexer::Lexer;
use Scanner::Parser::Parser;
use Scanner::Types::SourceFile;

fn run_file(path: &str) {
    let source = match SourceFile::read(path) {
        Ok(src) => src,
        Err(err) => {
            eprintln!("erro ao ler arquivo: {err}");
            std::process::exit(1);
        }
    };

    let tokens = match Lexer::new(&source).tokenize() {
        Ok(tokens) => tokens,
        Err(err) => {
            eprintln!("erro léxico: {err}");
            std::process::exit(1);
        }
    };

    let term = match Parser::new(tokens).parse() {
        Ok(term) => term,
        Err(err) => {
            eprintln!("erro sintático: {err}");
            std::process::exit(1);
        }
    };

    match type_check_and_eval(&term) {
        Ok(value) => println!("{term} = {value}"),
        Err(err) => {
            eprintln!("erro de tipo: {err}");
            std::process::exit(1);
        }
    }
}

fn main() {
    match std::env::args().nth(1) {
        Some(path) => run_file(&path),
        None => Scanner::Repl::run(),
    }
}
