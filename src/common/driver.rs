//! O driver: executa um comando sobre o texto-fonte de qualquer
//! [`Language`], devolvendo o relatório como texto.
//!
//! [`Runner`] é a versão com tipos apagados (objeto-seguro), que o
//! registro e o CLI usam para guardar linguagens diferentes na mesma
//! lista.
//!
//! Os erros são `String`s já formatadas, com o nome do estágio
//! (`syntax error: ...`, `type error: ...`).

use std::marker::PhantomData;
use std::str::FromStr;

use crate::common::diagnostic::render;
use crate::common::document::{blocks_to_text, Block};
use crate::common::language::{applicable_laws, capabilities, law_violations, Capabilities, Language};
use crate::common::machine_language::{Compile, Vm};
use crate::common::semantics::{run_with_fuel, BigStep, Machine, Typing, DEFAULT_FUEL};
use crate::common::ToLatex;

// =============================================================================
// Options
// =============================================================================

/// Limites de uma execução.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Options {
    /// Máximo de passos da semântica estrutural e da máquina.
    pub fuel: usize,
    /// Máximo de passos mostrados em um trace (o resto é resumido).
    pub max_steps_shown: usize,
}

impl Default for Options {
    fn default() -> Self {
        Self { fuel: DEFAULT_FUEL, max_steps_shown: 100 }
    }
}

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

impl Command {
    /// O modo faz sentido para uma linguagem com estas capacidades?
    pub fn is_available(self, caps: Capabilities) -> bool {
        match self {
            Command::Type => caps.typing,
            Command::Small => caps.small_step,
            Command::Big => caps.big_step,
            Command::Compile | Command::Machine => caps.compile,
            Command::Parse | Command::Latex | Command::Laws | Command::Full => true,
        }
    }

    /// O que falta à linguagem `language` para este modo.
    pub fn requires(self) -> &'static str {
        match self {
            Command::Type => "a type system",
            Command::Small => "small-step semantics",
            Command::Big => "big-step semantics",
            Command::Compile | Command::Machine => "a compiler to the virtual machine",
            Command::Parse | Command::Latex | Command::Laws | Command::Full => "anything",
        }
    }

    /// A mensagem de um modo indisponível: `stlc does not define big-step semantics`.
    pub fn unavailable(self, language: &str) -> String {
        format!("{language} does not define {}", self.requires())
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
    execute_with::<L>(command, source, &Options::default())
}

pub fn execute_with<L: Language>(
    command: Command,
    source: &str,
    options: &Options,
) -> Result<String, String> {
    let term = L::parse(source)
        .map_err(|e| format!("syntax error: {}", render(source, 0, &e)))?;
    execute_term::<L>(command, &term, options)
}

/// Executa `command` sobre um termo já lido (o interpretador expande as
/// definições antes de chamar), devolvendo o texto do terminal.
pub fn execute_term<L: Language>(
    command: Command,
    term: &L::Term,
    options: &Options,
) -> Result<String, String> {
    execute_blocks::<L>(command, term, options).map(|blocks| blocks_to_text(&blocks))
}

/// Como [`execute_term`], mas devolve os blocos (texto, fórmulas, títulos),
/// que a página web renderiza com o KaTeX.
pub fn execute_blocks<L: Language>(
    command: Command,
    term: &L::Term,
    options: &Options,
) -> Result<Vec<Block>, String> {
    if !command.is_available(capabilities::<L>()) {
        return Err(command.unavailable(L::NAME));
    }

    match command {
        Command::Parse => Ok(vec![term_block::<L>(term)]),
        Command::Type => typing::<L>(term),
        Command::Small => Ok(small_step::<L>(term, options)),
        Command::Big => big_step::<L>(term),
        Command::Latex => Ok(latex::<L>(term, options)),
        Command::Compile => compile::<L>(term),
        Command::Machine => machine::<L>(term, true, options),
        Command::Laws => Ok(laws::<L>(term)),
        Command::Full => Ok(full::<L>(term, options)),
    }
}

fn term_block<L: Language>(term: &L::Term) -> Block {
    Block::math(format!("{term}\n"), term.to_latex(), term.to_latex())
}

fn typing<L: Language>(term: &L::Term) -> Result<Vec<Block>, String> {
    let derivation =
        <L::Typing as Typing>::check(term).map_err(|e| format!("type error: {e}"))?;

    Ok(vec![Block::math(
        format!("type: {}\n{}", derivation.conclusion.ty, derivation.to_text()),
        derivation.to_katex_tree(),
        derivation.to_latex_tree(),
    )])
}

fn small_step<L: Language>(term: &L::Term, options: &Options) -> Vec<Block> {
    let trace = run_with_fuel::<L::Small>(term.clone(), options.fuel);

    vec![Block::math(
        trace.to_text_limited(options.max_steps_shown),
        trace.to_katex_limited(options.max_steps_shown),
        trace.to_latex_limited(options.max_steps_shown),
    )]
}

fn big_step<L: Language>(term: &L::Term) -> Result<Vec<Block>, String> {
    let derivation =
        <L::Big as BigStep>::evaluate(term).map_err(|e| format!("evaluation error: {e}"))?;

    Ok(vec![Block::math(
        format!("value: {}\n{}", derivation.conclusion.value, derivation.to_text()),
        derivation.to_katex_tree(),
        derivation.to_latex_tree(),
    )])
}

fn compile<L: Language>(term: &L::Term) -> Result<Vec<Block>, String> {
    let program =
        <L::Compiler as Compile>::compile(term).map_err(|e| format!("compile error: {e}"))?;

    Ok(vec![Block::Text(program.to_string())])
}

fn machine<L: Language>(
    term: &L::Term,
    verbose: bool,
    options: &Options,
) -> Result<Vec<Block>, String> {
    let program =
        <L::Compiler as Compile>::compile(term).map_err(|e| format!("compile error: {e}"))?;
    let execution = Vm::execute_with_fuel(&program, options.fuel);

    let mut out = String::new();
    if verbose {
        out.push_str(&execution.trace.to_text_limited(options.max_steps_shown));
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

    Ok(vec![Block::Text(out)])
}

/// O modo `latex`: as três derivações como fórmulas, cujo `text` é o código
/// LaTeX (`mathpartir`) para colar em um `.tex`.
fn latex<L: Language>(term: &L::Term, options: &Options) -> Vec<Block> {
    let caps = capabilities::<L>();
    let mut out = Vec::new();

    if caps.typing {
        match <L::Typing as Typing>::check(term) {
            Ok(d) => {
                let tex = d.to_latex_tree();
                out.push(Block::math(
                    format!("% typing\n\\[\n{tex}\n\\]\n\n"),
                    d.to_katex_tree(),
                    tex,
                ));
            }
            Err(e) => out.push(Block::Error(format!("% type error: {e}\n\n"))),
        }
    }

    if caps.big_step {
        match <L::Big as BigStep>::evaluate(term) {
            Ok(d) => {
                let tex = d.to_latex_tree();
                out.push(Block::math(
                    format!("% big-step\n\\[\n{tex}\n\\]\n\n"),
                    d.to_katex_tree(),
                    tex,
                ));
            }
            Err(e) => out.push(Block::Error(format!("% evaluation error: {e}\n\n"))),
        }
    }

    if caps.small_step {
        let trace = run_with_fuel::<L::Small>(term.clone(), options.fuel);
        let tex = trace.to_latex_limited(options.max_steps_shown);
        out.push(Block::math(
            format!("% small-step\n\\[\n{tex}\n\\]\n"),
            trace.to_katex_limited(options.max_steps_shown),
            tex,
        ));
    }

    out
}

fn laws<L: Language>(term: &L::Term) -> Vec<Block> {
    if applicable_laws::<L>() == 0 {
        return vec![Block::Text(format!(
            "no laws apply: {} does not define enough semantics to relate\n",
            L::NAME
        ))];
    }

    let violated = law_violations::<L>(term);

    let text = if violated.is_empty() {
        "all laws hold\n".to_string()
    } else {
        format!("violated: {}\n", violated.join(", "))
    };
    vec![Block::Text(text)]
}

/// Uma seção de `full`: um título e o resultado do estágio (um erro de um
/// estágio vira um bloco de erro, e os demais continuam).
fn section(out: &mut Vec<Block>, title: &str, body: Result<Vec<Block>, String>) {
    out.push(Block::Heading(title.to_string()));
    match body {
        Ok(blocks) => out.extend(blocks),
        Err(text) => out.push(Block::Error(text)),
    }
}

/// Um estágio de `full` que a linguagem não define vira uma nota, não um erro.
fn stage<L: Language>(
    out: &mut Vec<Block>,
    title: &str,
    command: Command,
    body: impl FnOnce() -> Result<Vec<Block>, String>,
) {
    if command.is_available(capabilities::<L>()) {
        section(out, title, body());
    } else {
        section(out, title, Ok(vec![Block::Text(format!("not defined: {}\n", command.unavailable(L::NAME)))]));
    }
}

/// Todos os estágios. Cada um é independente (os tipos são apagados na
/// compilação), então um termo mal tipado ainda mostra como trava.
fn full<L: Language>(term: &L::Term, options: &Options) -> Vec<Block> {
    let mut out = Vec::new();

    section(&mut out, "term", Ok(vec![term_block::<L>(term)]));
    stage::<L>(&mut out, "type", Command::Type, || typing::<L>(term));
    stage::<L>(&mut out, "small-step", Command::Small, || Ok(small_step::<L>(term, options)));
    stage::<L>(&mut out, "big-step", Command::Big, || big_step::<L>(term));
    stage::<L>(&mut out, "machine code", Command::Compile, || compile::<L>(term));
    stage::<L>(&mut out, "machine", Command::Machine, || machine::<L>(term, false, options));
    section(&mut out, "laws", Ok(laws::<L>(term)));

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
