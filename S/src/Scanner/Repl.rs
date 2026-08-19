//! S/src/Scanner/Repl.rs
//!
//! Read-Eval-Print Loop para a linguagem `S`.
//!
//! author:  n-rosenthal
//! date:    2026-08-17
//! version: 0.1

use std::io::{self, Write};
use std::path::PathBuf;

use crate::Scanner::Evaluator::type_check_and_eval;
use crate::Scanner::Lexer::Lexer;
use crate::Scanner::Parser::Parser;
use crate::Scanner::Types::SourceFile;

const PROMPT: &str = "S> ";

/// Comandos especiais do REPL, reconhecidos antes de tentar
/// interpretar a linha como um termo de `S`.
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
    println!("  1 + 2");
    println!("  true == false");
    println!("  let x: Integer = 10 in x + x");
    println!("  let r: Ref Integer = ref 1 in let _: Unit = (r := 99) in !r");
}

/// Roda uma única linha de entrada através do pipeline completo e
/// imprime o resultado (ou erro) no formato apropriado a cada etapa.
///
/// Nota: cada linha recebe um ambiente e uma memória (`Store`)
/// próprios e vazios — nada persiste de uma linha para a outra. Como
/// `let ... in ...` já exige que o corpo esteja na mesma linha, isso
/// não limita o que dá para expressar; só significa que não existe
/// (ainda) um jeito de declarar uma variável numa linha e usá-la na
/// próxima.
fn run_line(line: &str) {
    let source = SourceFile {
        path: PathBuf::from("<repl>"),
        content: line.to_string(),
        lines: vec![line.to_string()],
    };

    let tokens = match Lexer::new(&source).tokenize() {
        Ok(tokens) => tokens,
        Err(err) => {
            println!("erro léxico: {err}");
            return;
        }
    };

    let term = match Parser::new(tokens).parse() {
        Ok(term) => term,
        Err(err) => {
            println!("erro sintático: {err}");
            return;
        }
    };

    match type_check_and_eval(&term) {
        Ok(value) => println!("{term} : {value}"),
        Err(err) => println!("erro de tipo: {err}"),
    }
}

/// Inicia o loop interativo, lendo de stdin até EOF (Ctrl+D) ou até
/// o usuário digitar um comando de saída.
pub fn run() {
    println!("S — linguagem de termos (REPL). Digite :help para ajuda, :quit para sair.");

    let stdin = io::stdin();
    let mut input = String::new();

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
            ReplCommand::Term(text) => run_line(&text),
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

    #[test]
    fn linha_comum_vira_termo() {
        match parse_command("1 + 2") {
            ReplCommand::Term(text) => assert_eq!(text, "1 + 2"),
            _ => panic!("deveria reconhecer como termo"),
        }
    }
}
