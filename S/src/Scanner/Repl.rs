//! S/src/Scanner/Repl.rs
//!
//! Read-Eval-Print Loop para a linguagem `S` com estado persistente.
//!
//! author:  n-rosenthal
//! date:    2026-08-17
//! version: 0.3 (persistent type env)

use std::io::{self, Write};
use std::path::PathBuf;

use crate::Scanner::Evaluator::{eval_in, Store, Value, ValueEnv};
use crate::Scanner::Lexer::Lexer;
use crate::Scanner::Parser::Parser;
use crate::Scanner::TypeChecker::{type_of_with_env, TypeEnv};
use crate::Scanner::Types::{SourceFile, Term};

const PROMPT: &str = "S> ";

enum ReplCommand {
    Quit,
    Help,
    Term(String),
}

fn parse_command(line: &str) -> ReplCommand {
    match line.trim() {
        ":quit" | ":q" | "exit" => ReplCommand::Quit,
        ":help" | ":h" => ReplCommand::Help,
        other => ReplCommand::Term(other.to_string()),
    }
}

fn print_help() {
    println!("Comandos disponíveis:");
    println!("  :help, :h     mostra esta mensagem");
    println!("  :quit, :q     encerra o REPL");
    println!("  <expressão>   analisa e avalia uma expressão de S");
    println!();
    println!("Exemplos:");
    println!("  let x: Integer = 1");       // define x para a próxima linha
    println!("  x + 1");
    println!("  let r: Ref Integer = ref 1 in r := 99 ; !r");
}

/// Roda uma linha no REPL com estado persistente.
fn run_line(line: &str, env: &mut ValueEnv, type_env: &mut TypeEnv, store: &mut Store) {
    let source = SourceFile {
        path: PathBuf::from("<repl>"),
        content: line.to_string(),
        lines: vec![line.to_string()],
    };

    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer);

    let term = match parser.parse() {
        Ok(t) => t,
        Err(err) => {
            println!("erro sintático: {err}");
            return;
        }
    };

    // Verificação de tipo usando o ambiente persistente
    if let Err(err) = type_of_with_env(&term, type_env) {
        println!("erro de tipo: {err}");
        return;
    }

    // Avalia e atualiza os ambientes
    let value = eval_in(&term, env, store);

    // Se for um Let, adicionamos o tipo ao TypeEnv para persistência
    if let Term::Let { name, declared_type, .. } = &term {
        type_env.insert(name.clone(), declared_type.clone());
    }

    // Exibe resultado apropriado
    if !matches!(term, Term::Let { .. }) {
        println!("{term} = {value}");
    } else {
        match value {
            Value::Unit => println!("ok"),
            _ => println!("{term} = {value}"),
        }
    }
}

pub fn run() {
    println!("S — linguagem de termos (REPL). Digite :help para ajuda, :quit para sair.");
    println!("Nota: variáveis e memória persistem entre linhas!");

    let stdin = io::stdin();
    let mut input = String::new();
    let mut env = ValueEnv::new();
    let mut type_env = TypeEnv::new();
    let mut store = Store::new();

    loop {
        print!("{PROMPT}");
        io::stdout().flush().expect("falha ao escrever no stdout");

        input.clear();
        let bytes_read = stdin.read_line(&mut input).expect("falha ao ler stdin");

        if bytes_read == 0 {
            println!();
            break;
        }

        let trimmed = input.trim();
        if trimmed.is_empty() {
            continue;
        }

        match parse_command(trimmed) {
            ReplCommand::Quit => break,
            ReplCommand::Help => print_help(),
            ReplCommand::Term(text) => run_line(&text, &mut env, &mut type_env, &mut store),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reconhece_comando_de_saida() {
        assert!(matches!(parse_command(":quit"), ReplCommand::Quit));
        assert!(matches!(parse_command(":q"), ReplCommand::Quit));
        assert!(matches!(parse_command("exit"), ReplCommand::Quit));
    }

    #[test]
    fn reconhece_comando_de_ajuda() {
        assert!(matches!(parse_command(":help"), ReplCommand::Help));
        assert!(matches!(parse_command(":h"), ReplCommand::Help));
    }
}
