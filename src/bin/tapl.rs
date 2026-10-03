//! `tapl`: executa qualquer linguagem registrada.
//!
//!     tapl                        lista as linguagens e os comandos
//!     tapl lambda                 REPL (`:help` lista os comandos)
//!     tapl lambda <comando> [t]   executa um comando (t, ou stdin se omitido ou `-`)

use std::io::{self, BufRead, Read, Write};
use std::process::ExitCode;

use simple_languages::common::driver::Command;
use simple_languages::common::interpreter::ReplyKind;
use simple_languages::registry;

fn usage() -> String {
    let languages: Vec<_> = registry::all().iter().map(|r| r.name()).collect();
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
            eprintln!("{message}");
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

    let runner = registry::find(language)
        .ok_or_else(|| format!("unknown language `{language}`\n\n{}", usage()))?;

    match args.get(1) {
        None => repl(runner.name()),
        Some(command) => {
            let command: Command = command.parse()?;
            let source = if args.len() > 2 && args[2] != "-" {
                args[2..].join(" ")
            } else {
                read_stdin()?
            };

            print!("{}", runner.run(command, source.trim())?);
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

fn repl(language: &str) -> Result<(), String> {
    let mut session =
        registry::session(language).ok_or_else(|| format!("unknown language `{language}`"))?;
    let stdin = io::stdin();

    println!("{} — :help for commands, :quit to leave", session.language());

    loop {
        print!("{}({})> ", session.language(), session.mode().name());
        io::stdout().flush().map_err(|e| e.to_string())?;

        let mut line = String::new();
        if stdin.lock().read_line(&mut line).map_err(|e| e.to_string())? == 0 {
            println!();
            return Ok(()); // EOF
        }

        let reply = session.submit(&line);
        if !reply.text.is_empty() {
            match reply.kind {
                ReplyKind::Error => println!("error: {}", reply.text.trim_end()),
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
