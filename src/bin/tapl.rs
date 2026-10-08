//! `tapl`: executa qualquer linguagem registrada.
//!
//!     tapl                          lista as linguagens e os comandos
//!     tapl stlc                     REPL (`:help` lista os comandos)
//!     tapl stlc <modo> [programa]   executa um programa (ou lê o stdin se omitido ou `-`)
//!
//! Um programa tem uma ou mais instruções separadas por `;;`, que podem ocupar
//! várias linhas; um erro de sintaxe mostra o trecho com um `^` sob o erro. No
//! REPL, uma linha incompleta (um parêntese aberto) pede continuação com `...`;
//! uma linha em branco executa o que foi digitado.

use std::io::{self, BufRead, Read, Write};
use std::process::ExitCode;

use simple_languages::common::driver::Command;
use simple_languages::common::interpreter::{Interpreter, ReplyKind};
use simple_languages::registry;

fn usage() -> String {
    let languages = registry::names();
    let commands: Vec<_> = Command::ALL.iter().map(|c| c.name()).collect();

    format!(
        "usage: tapl <language> [<command> [term]]\n\
         \x20 languages: {}\n\
         \x20 commands:  {}\n\
         \x20 without a command, starts a REPL (`:<command>` switches, `:q` quits)\n",
        languages.join(", "),
        commands.join(", "),
    )
}

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();

    match run(&args) {
        Ok(()) => ExitCode::SUCCESS,
        Err(message) => {
            if !message.is_empty() {
                eprintln!("{message}");
            }
            ExitCode::FAILURE
        }
    }
}

fn run(args: &[String]) -> Result<(), String> {
    let Some(language) = args.first() else {
        print!("{}", usage());
        return Ok(());
    };

    if matches!(language.as_str(), "-h" | "--help" | "help") {
        print!("{}", usage());
        return Ok(());
    }

    let mut session = registry::session(language)
        .ok_or_else(|| format!("unknown language `{language}`\n\n{}", usage()))?;

    match args.get(1) {
        None => repl(&mut *session),
        Some(command) => {
            let command: Command = command.parse()?;
            let source = if args.len() > 2 && args[2] != "-" {
                args[2..].join(" ")
            } else {
                read_stdin()?
            };

            session.select_mode(command)?;
            let reply = session.submit(&source);

            if reply.kind == ReplyKind::Error {
                // um programa mostra o que executou antes do erro
                if reply.blocks.is_empty() {
                    return Err(reply.text.trim_end().to_string());
                }
                print!("{}", with_newline(&reply.text));
                return Err(String::new());
            }

            print!("{}", with_newline(&reply.text));
            Ok(())
        }
    }
}

fn read_stdin() -> Result<String, String> {
    let mut source = String::new();
    io::stdin()
        .read_to_string(&mut source)
        .map_err(|e| e.to_string())?;
    Ok(source)
}

fn repl(session: &mut dyn Interpreter) -> Result<(), String> {
    let stdin = io::stdin();
    let mut buffer = String::new();

    println!("{} — :help for commands, :quit to leave", session.language());

    loop {
        if buffer.is_empty() {
            print!("{}({})> ", session.language(), session.mode().name());
        } else {
            print!("{}... ", " ".repeat(session.language().len() + session.mode().name().len() + 1));
        }
        io::stdout().flush().map_err(|e| e.to_string())?;

        let mut line = String::new();
        if stdin.lock().read_line(&mut line).map_err(|e| e.to_string())? == 0 {
            println!();
            if buffer.trim().is_empty() {
                return Ok(()); // EOF
            }
            line.clear(); // EOF no meio de uma entrada: executa o que há
        } else if buffer.is_empty() && line.trim().is_empty() {
            continue;
        } else {
            buffer.push_str(&line);
            // uma linha em branco executa, mesmo incompleta (para ver o erro)
            if !line.trim().is_empty() && session.needs_more(&buffer) {
                continue;
            }
        }

        let reply = session.submit(&buffer);
        buffer.clear();

        if !reply.text.is_empty() {
            match (reply.kind, reply.blocks.is_empty()) {
                (ReplyKind::Error, true) => println!("error: {}", reply.text.trim_end()),
                _ => print!("{}", with_newline(&reply.text)),
            }
        }
        if reply.quit {
            return Ok(());
        }
    }
}

fn with_newline(text: &str) -> String {
    if text.ends_with('\n') {
        text.to_string()
    } else {
        format!("{text}\n")
    }
}
