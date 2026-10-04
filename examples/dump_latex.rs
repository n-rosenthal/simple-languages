//! Despeja, em JSON (uma linha por bloco), todo o LaTeX que a interface web
//! renderiza: os exemplos de cada linguagem em todos os modos, a sintaxe e as
//! regras. Serve para conferir o LaTeX contra o KaTeX:
//!
//!     cargo run -q --example dump_latex | node web/tools/check-latex.mjs

use simple_languages::common::document::Block;
use simple_languages::common::driver::Command;
use simple_languages::registry;

fn json(s: &str) -> String {
    let mut out = String::from("\"");
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

fn emit(language: &str, origin: &str, source: &str, blocks: &[Block]) {
    for block in blocks {
        if let Block::Math { web, tex, .. } = block {
            println!(
                "{{\"language\":{},\"origin\":{},\"source\":{},\"web\":{},\"tex\":{}}}",
                json(language),
                json(origin),
                json(source),
                json(web),
                json(tex)
            );
        }
    }
}

fn main() {
    for language in registry::names() {
        let mut session = registry::session(language).expect("registered");
        session.set_fuel(300);

        emit(language, "syntax", "", &session.syntax());
        emit(language, "rules", "", &session.rules());

        for command in Command::ALL {
            session.set_mode(command);
            for example in session.examples() {
                let reply = session.submit(example.source);
                emit(language, command.name(), example.source, &reply.blocks);
            }
        }
    }
}
