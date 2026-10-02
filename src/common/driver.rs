//! O driver: executa um comando sobre o texto-fonte de qualquer
//! [`Language`], devolvendo o relatório como texto.
//!
//! [`Runner`] é a versão com tipos apagados (objeto-seguro), que o
//! registro e o CLI usam para guardar linguagens diferentes na mesma
//! lista.
//!
//! Os erros são `String`s já formatadas, com o nome do estágio
//! (`syntax error: ...`, `type error: ...`). Quando existir um trait
//! `Diagnostic`, o driver passa a mostrar o trecho do fonte.

use std::marker::PhantomData;
use std::str::FromStr;

use crate::common::language::{law_violations, Language};
use crate::common::machine_language::{Compile, Vm};
use crate::common::semantics::{run, BigStep, Machine, Typing};

// =============================================================================
// Command
// =============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Command {
    /// O termo lido, reimpresso (confere o parser).
    Parse,
    /// Γ ⊢ t : T, com a derivação.
    Type,
    /// A execução passo a passo (semântica estrutural).
    Small,
    /// t ⇓ v, com a derivação.
    Big,
    /// As derivações e a execução em LaTeX (requer `mathpartir`).
    Latex,
    /// O código de máquina (desassemblado).
    Compile,
    /// A execução na máquina virtual.
    Machine,
    /// Roda as leis genéricas sobre o termo.
    Laws,
    /// Todos os estágios, seguindo mesmo se um deles falhar.
    Full,
}

impl Command {
    pub const ALL: [Command; 9] = [
        Command::Parse,
        Command::Type,
        Command::Small,
        Command::Big,
        Command::Latex,
        Command::Compile,
        Command::Machine,
        Command::Laws,
        Command::Full,
    ];

    pub fn name(self) -> &'static str {
        match self {
            Command::Parse => "parse",
            Command::Type => "type",
            Command::Small => "small",
            Command::Big => "big",
            Command::Latex => "latex",
            Command::Compile => "compile",
            Command::Machine => "machine",
            Command::Laws => "laws",
            Command::Full => "full",
        }
    }
}

impl FromStr for Command {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, String> {
        Command::ALL
            .into_iter()
            .find(|command| command.name() == s)
            .ok_or_else(|| {
                let names: Vec<_> = Command::ALL.iter().map(|c| c.name()).collect();
                format!("unknown command `{s}` (expected one of: {})", names.join(", "))
            })
    }
}

// =============================================================================
// execute
// =============================================================================

pub fn execute<L: Language>(command: Command, source: &str) -> Result<String, String> {
    let term = L::parse(source).map_err(|e| format!("syntax error: {e}"))?;

    match command {
        Command::Parse => Ok(format!("{term}\n")),
        Command::Type => typing::<L>(&term),
        Command::Small => Ok(small_step::<L>(&term)),
        Command::Big => big_step::<L>(&term),
        Command::Latex => Ok(latex::<L>(&term)),
        Command::Compile => compile::<L>(&term),
        Command::Machine => machine::<L>(&term, true),
        Command::Laws => Ok(laws::<L>(&term)),
        Command::Full => Ok(full::<L>(&term)),
    }
}

fn typing<L: Language>(term: &L::Term) -> Result<String, String> {
    let derivation =
        <L::Typing as Typing>::check(term).map_err(|e| format!("type error: {e}"))?;

    Ok(format!(
        "type: {}\n{}",
        derivation.conclusion.ty,
        derivation.to_text()
    ))
}

fn small_step<L: Language>(term: &L::Term) -> String {
    run::<L::Small>(term.clone()).to_text()
}

fn big_step<L: Language>(term: &L::Term) -> Result<String, String> {
    let derivation =
        <L::Big as BigStep>::evaluate(term).map_err(|e| format!("evaluation error: {e}"))?;

    Ok(format!(
        "value: {}\n{}",
        derivation.conclusion.value,
        derivation.to_text()
    ))
}

fn compile<L: Language>(term: &L::Term) -> Result<String, String> {
    let program =
        <L::Compiler as Compile>::compile(term).map_err(|e| format!("compile error: {e}"))?;

    Ok(program.to_string())
}

fn machine<L: Language>(term: &L::Term, verbose: bool) -> Result<String, String> {
    let program =
        <L::Compiler as Compile>::compile(term).map_err(|e| format!("compile error: {e}"))?;
    let execution = Vm::execute(&program);

    let mut out = String::new();
    if verbose {
        out.push_str(&execution.trace.to_text());
    }
    out.push_str(&format!(
        "steps: {}, outcome: {}\n",
        execution.trace.len(),
        execution.trace.outcome
    ));
    match &execution.value {
        Some(value) => out.push_str(&format!("value: {value}\n")),
        None => out.push_str("no value\n"),
    }

    Ok(out)
}

fn latex<L: Language>(term: &L::Term) -> String {
    let mut out = String::new();

    match <L::Typing as Typing>::check(term) {
        Ok(d) => out.push_str(&format!("% typing\n\\[\n{}\n\\]\n\n", d.to_latex_tree())),
        Err(e) => out.push_str(&format!("% type error: {e}\n\n")),
    }

    match <L::Big as BigStep>::evaluate(term) {
        Ok(d) => out.push_str(&format!("% big-step\n\\[\n{}\n\\]\n\n", d.to_latex_tree())),
        Err(e) => out.push_str(&format!("% evaluation error: {e}\n\n")),
    }

    let trace = run::<L::Small>(term.clone());
    out.push_str(&format!("% small-step\n\\[\n{}\n\\]\n", trace.to_latex()));

    out
}

fn laws<L: Language>(term: &L::Term) -> String {
    let violated = law_violations::<L>(term);

    if violated.is_empty() {
        "all laws hold\n".to_string()
    } else {
        format!("violated: {}\n", violated.join(", "))
    }
}

fn push_section(out: &mut String, title: &str, body: Result<String, String>) {
    let text = match body {
        Ok(text) | Err(text) => text,
    };
    out.push_str(&format!("== {title} ==\n{text}"));
    if !text.ends_with('\n') {
        out.push('\n');
    }
    out.push('\n');
}

/// Todos os estágios. Cada um é independente (os tipos são apagados na
/// compilação), então um termo mal tipado ainda mostra como trava.
fn full<L: Language>(term: &L::Term) -> String {
    let mut out = String::new();

    push_section(&mut out, "term", Ok(format!("{term}")));
    push_section(&mut out, "type", typing::<L>(term));
    push_section(&mut out, "small-step", Ok(small_step::<L>(term)));
    push_section(&mut out, "big-step", big_step::<L>(term));
    push_section(&mut out, "machine code", compile::<L>(term));
    push_section(&mut out, "machine", machine::<L>(term, false));
    push_section(&mut out, "laws", Ok(laws::<L>(term)));

    out
}

// =============================================================================
// Runner
// =============================================================================

/// Uma linguagem com os tipos apagados.
pub trait Runner {
    fn name(&self) -> &'static str;
    fn run(&self, command: Command, source: &str) -> Result<String, String>;
}

pub struct Dispatch<L>(PhantomData<fn() -> L>);

impl<L> Dispatch<L> {
    pub fn new() -> Self {
        Self(PhantomData)
    }
}

impl<L> Default for Dispatch<L> {
    fn default() -> Self {
        Self::new()
    }
}

impl<L: Language> Runner for Dispatch<L> {
    fn name(&self) -> &'static str {
        L::NAME
    }

    fn run(&self, command: Command, source: &str) -> Result<String, String> {
        execute::<L>(command, source)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn commands_round_trip_through_their_names() {
        for command in Command::ALL {
            assert_eq!(command.name().parse::<Command>(), Ok(command));
        }
    }

    #[test]
    fn unknown_commands_list_the_valid_ones() {
        let error = "nope".parse::<Command>().unwrap_err();
        assert!(error.contains("nope") && error.contains("machine"));
    }
}