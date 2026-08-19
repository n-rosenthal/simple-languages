//! S/src/main.rs
//! Ponto de entrada do interpretador de `S`.
#![allow(non_snake_case)]

pub mod Scanner;

use Scanner::Evaluator::{eval_in, Store, ValueEnv};
use Scanner::Lexer::Lexer;
use Scanner::Parser::Parser;
use Scanner::TypeChecker::type_of;
use Scanner::Types::SourceFile;

fn run_file(path: &str) {
    let source = match SourceFile::read(path) {
        Ok(src) => src,
        Err(err) => {
            eprintln!("erro ao ler arquivo: {err}");
            std::process::exit(1);
        }
    };

    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer);

    let term = match parser.parse() {
        Ok(t) => t,
        Err(err) => {
            eprintln!("erro sintático: {err}");
            std::process::exit(1);
        }
    };

    if let Err(err) = type_of(&term) {
        eprintln!("erro de tipo: {err}");
        std::process::exit(1);
    }

    let mut env = ValueEnv::new();
    let mut store = Store::new();
    let value = eval_in(&term, &mut env, &mut store);
    println!("{term} = {value}");
}

fn main() {
    match std::env::args().nth(1) {
        Some(path) => run_file(&path),
        None => Scanner::Repl::run(),
    }
}
