//! O interpretador interativo, genérico em qualquer [`Language`].
//!
//! [`Session`] guarda o estado de uma sessão (modo atual, limite de
//! passos, definições) e transforma cada linha digitada em uma [`Reply`].
//! Não faz entrada nem saída: o REPL do terminal e a página web (WASM)
//! são apenas camadas finas sobre [`Interpreter::submit`].
//!
//! Uma linha pode ser:
//!
//! - um termo, executado no modo atual (`full`, `type`, `small`, ...);
//! - uma definição `nome = termo` (linguagens com variáveis; o termo é
//!   expandido na hora e as definições são substituídas nas linhas
//!   seguintes);
//! - um comando `:help`, `:mode`, `:fuel`, `:defs`, `:reset`,
//!   `:examples`, `:example N`, `:quit`, ou `:<modo>` para trocar de modo.
//!
//! As entradas são limitadas ([`MAX_INPUT_CHARS`], [`MAX_NESTING`],
//! [`MAX_FUEL`]) porque o interpretador também roda no navegador, onde a
//! pilha é pequena e um laço longo congela a aba.

use std::fmt::Write as _;
use std::str::FromStr;

use crate::common::driver::{execute_term, Command, Options};
use crate::common::language::{Example, Language};

/// Tamanho máximo de uma linha, em caracteres.
pub const MAX_INPUT_CHARS: usize = 2_000;
/// Profundidade máxima de parênteses aninhados.
pub const MAX_NESTING: usize = 100;
/// Maior valor aceito por `:fuel`.
pub const MAX_FUEL: usize = 20_000;

// =============================================================================
// Reply
// =============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReplyKind {
    /// O resultado de executar um termo.
    Output,
    /// Um erro (sintaxe, tipo, avaliação, comando inválido).
    Error,
    /// Uma mensagem do próprio interpretador (ajuda, mudança de modo).
    Info,
}

impl ReplyKind {
    pub fn name(self) -> &'static str {
        match self {
            ReplyKind::Output => "output",
            ReplyKind::Error => "error",
            ReplyKind::Info => "info",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Reply {
    pub kind: ReplyKind,
    pub text: String,
    /// O usuário pediu para sair (`:quit`).
    pub quit: bool,
}

impl Reply {
    fn new(kind: ReplyKind, text: impl Into<String>) -> Self {
        Self { kind, text: text.into(), quit: false }
    }

    fn output(text: impl Into<String>) -> Self {
        Self::new(ReplyKind::Output, text)
    }

    fn error(text: impl Into<String>) -> Self {
        Self::new(ReplyKind::Error, text)
    }

    fn info(text: impl Into<String>) -> Self {
        Self::new(ReplyKind::Info, text)
    }
}

// =============================================================================
// Interpreter (com tipos apagados)
// =============================================================================

/// Uma sessão de uma linguagem qualquer, com os tipos apagados, para
/// guardar linguagens diferentes na mesma lista (ver `registry`).
pub trait Interpreter {
    fn language(&self) -> &'static str;
    fn description(&self) -> &'static str;
    fn examples(&self) -> &'static [Example];
    fn supports_definitions(&self) -> bool;

    fn mode(&self) -> Command;
    fn set_mode(&mut self, command: Command);

    fn fuel(&self) -> usize;
    /// Limita o valor a `1..=MAX_FUEL` e devolve o valor efetivo.
    fn set_fuel(&mut self, fuel: usize) -> usize;

    /// As definições, como pares `(nome, termo)`.
    fn definitions(&self) -> Vec<(String, String)>;

    fn submit(&mut self, line: &str) -> Reply;
}

// =============================================================================
// Session
// =============================================================================

pub struct Session<L: Language> {
    mode: Command,
    options: Options,
    definitions: Vec<(String, L::Term)>,
}

impl<L: Language> Session<L> {
    pub fn new() -> Self {
        Self {
            mode: Command::Full,
            options: Options::default(),
            definitions: Vec::new(),
        }
    }

    /// Substitui as definições no termo.
    fn expand(&self, term: L::Term) -> L::Term {
        self.definitions
            .iter()
            .fold(term, |term, (name, value)| L::substitute(&term, name, value))
    }

    fn parse(&self, source: &str) -> Result<L::Term, Reply> {
        L::parse(source)
            .map(|term| self.expand(term))
            .map_err(|e| Reply::error(format!("syntax error: {e}")))
    }

    fn evaluate(&self, source: &str) -> Reply {
        let term = match self.parse(source) {
            Ok(term) => term,
            Err(reply) => return reply,
        };

        match execute_term::<L>(self.mode, &term, &self.options) {
            Ok(text) => Reply::output(text),
            Err(text) => Reply::error(text),
        }
    }

    fn define(&mut self, name: &str, source: &str) -> Reply {
        if !L::SUPPORTS_DEFINITIONS {
            return Reply::error(format!(
                "{} has no variables, so definitions are not available",
                L::NAME
            ));
        }
        if source.is_empty() {
            return Reply::error(format!("missing term after `{name} =`"));
        }

        let term = match self.parse(source) {
            Ok(term) => term,
            Err(reply) => return reply,
        };

        let text = format!("{name} = {term}");
        match self.definitions.iter_mut().find(|(existing, _)| existing == name) {
            Some(entry) => entry.1 = term,
            None => self.definitions.push((name.to_string(), term)),
        }

        Reply::info(text)
    }

    fn change_mode(&mut self, name: &str) -> Reply {
        match Command::from_str(name) {
            Ok(command) => {
                self.mode = command;
                Reply::info(format!("mode: {}", command.name()))
            }
            Err(message) => Reply::error(message),
        }
    }

    fn change_fuel(&mut self, text: &str) -> Reply {
        match text.parse::<usize>() {
            Ok(n) if (1..=MAX_FUEL).contains(&n) => {
                self.options.fuel = n;
                Reply::info(format!("fuel: {n}"))
            }
            _ => Reply::error(format!("fuel must be a number between 1 and {MAX_FUEL}")),
        }
    }

    fn list_examples(&self) -> Reply {
        let examples = L::examples();
        if examples.is_empty() {
            return Reply::info(format!("{} has no examples", L::NAME));
        }

        let mut out = String::new();
        for (i, example) in examples.iter().enumerate() {
            let _ = writeln!(out, "{:>2}. {}\n      {}", i + 1, example.title, example.source);
        }
        out.push_str("run one with `:example N`\n");
        Reply::info(out)
    }

    fn run_example(&mut self, text: &str) -> Reply {
        let examples = L::examples();
        let chosen = text
            .parse::<usize>()
            .ok()
            .and_then(|n| n.checked_sub(1))
            .and_then(|i| examples.get(i));

        match chosen {
            Some(example) => {
                let mut reply = self.submit(example.source);
                reply.text = format!("> {}\n{}", example.source, reply.text);
                reply
            }
            None => Reply::error(format!(
                "no such example; use a number from 1 to {} (see :examples)",
                examples.len()
            )),
        }
    }

    fn list_definitions(&self) -> Reply {
        if self.definitions.is_empty() {
            return Reply::info("no definitions");
        }
        let mut out = String::new();
        for (name, term) in &self.definitions {
            let _ = writeln!(out, "{name} = {term}");
        }
        Reply::info(out)
    }

    fn help(&self) -> String {
        let modes: Vec<_> = Command::ALL.iter().map(|c| c.name()).collect();
        let mut out = String::new();

        let _ = writeln!(out, "{}: {}", L::NAME, L::DESCRIPTION);
        let _ = writeln!(out, "  <term>               run the term in the current mode ({})", self.mode.name());
        if L::SUPPORTS_DEFINITIONS {
            let _ = writeln!(out, "  name = <term>        define a name, usable in later lines");
        }
        let _ = writeln!(out, "  :mode <m>  or  :<m>  modes: {}", modes.join(", "));
        let _ = writeln!(out, "  :fuel <n>            step limit, 1..{MAX_FUEL} (now {})", self.options.fuel);
        if L::SUPPORTS_DEFINITIONS {
            let _ = writeln!(out, "  :defs  :reset        list / clear definitions");
        }
        let _ = writeln!(out, "  :examples            list examples; `:example N` runs one");
        let _ = writeln!(out, "  :help  :quit");
        out
    }

    fn meta(&mut self, text: &str) -> Reply {
        let mut words = text.split_whitespace();
        let name = words.next().unwrap_or("");
        let argument = words.next();

        match (name, argument) {
            ("help" | "h" | "?", _) => Reply::info(self.help()),
            ("quit" | "q" | "exit", _) => Reply { quit: true, ..Reply::info("bye") },

            ("mode", None) => Reply::info(format!("mode: {}", self.mode.name())),
            ("mode", Some(m)) => self.change_mode(m),

            ("fuel", None) => Reply::info(format!("fuel: {}", self.options.fuel)),
            ("fuel", Some(n)) => self.change_fuel(n),

            ("defs" | "definitions", _) => self.list_definitions(),
            ("reset", _) => {
                self.definitions.clear();
                Reply::info("definitions cleared")
            }

            ("examples", _) => self.list_examples(),
            ("example" | "load", Some(n)) => self.run_example(n),
            ("example" | "load", None) => self.list_examples(),

            (other, _) => match Command::from_str(other) {
                Ok(command) => {
                    self.mode = command;
                    Reply::info(format!("mode: {}", command.name()))
                }
                Err(_) => Reply::error(format!("unknown command `:{other}` (try :help)")),
            },
        }
    }
}

impl<L: Language> Default for Session<L> {
    fn default() -> Self {
        Self::new()
    }
}

/// Maior profundidade de parênteses aninhados na linha.
fn max_nesting(line: &str) -> usize {
    let (mut depth, mut max) = (0usize, 0usize);
    for c in line.chars() {
        match c {
            '(' => {
                depth += 1;
                max = max.max(depth);
            }
            ')' => depth = depth.saturating_sub(1),
            _ => {}
        }
    }
    max
}

/// `nome = termo` (e não `==`). O termo pode ser vazio; quem define avisa.
fn split_definition(line: &str) -> Option<(&str, &str)> {
    let end = line
        .find(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
        .unwrap_or(line.len());
    let name = &line[..end];

    if !name.chars().next()?.is_ascii_alphabetic() {
        return None;
    }

    let rest = line[end..].trim_start().strip_prefix('=')?;
    if rest.starts_with('=') {
        return None; // `==`: um operador, não uma definição
    }

    Some((name, rest.trim()))
}

impl<L: Language> Interpreter for Session<L> {
    fn language(&self) -> &'static str {
        L::NAME
    }

    fn description(&self) -> &'static str {
        L::DESCRIPTION
    }

    fn examples(&self) -> &'static [Example] {
        L::examples()
    }

    fn supports_definitions(&self) -> bool {
        L::SUPPORTS_DEFINITIONS
    }

    fn mode(&self) -> Command {
        self.mode
    }

    fn set_mode(&mut self, command: Command) {
        self.mode = command;
    }

    fn fuel(&self) -> usize {
        self.options.fuel
    }

    fn set_fuel(&mut self, fuel: usize) -> usize {
        self.options.fuel = fuel.clamp(1, MAX_FUEL);
        self.options.fuel
    }

    fn definitions(&self) -> Vec<(String, String)> {
        self.definitions
            .iter()
            .map(|(name, term)| (name.clone(), term.to_string()))
            .collect()
    }

    fn submit(&mut self, line: &str) -> Reply {
        let line = line.trim();

        if line.is_empty() {
            return Reply::info("");
        }
        if line.chars().count() > MAX_INPUT_CHARS {
            return Reply::error(format!("input too long (limit: {MAX_INPUT_CHARS} characters)"));
        }

        if let Some(command) = line.strip_prefix(':') {
            return self.meta(command);
        }

        if max_nesting(line) > MAX_NESTING {
            return Reply::error(format!("too many nested parentheses (limit: {MAX_NESTING})"));
        }

        match split_definition(line) {
            Some((name, source)) => self.define(name, source),
            None => self.evaluate(line),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nesting_is_the_deepest_run_of_open_parentheses() {
        assert_eq!(max_nesting("a b"), 0);
        assert_eq!(max_nesting("(a (b)) (c)"), 2);
        assert_eq!(max_nesting("((("), 3);
        assert_eq!(max_nesting(")))(("), 2);
    }

    #[test]
    fn definitions_are_recognized_but_equality_is_not() {
        assert_eq!(split_definition("id = λx:A. x"), Some(("id", "λx:A. x")));
        assert_eq!(split_definition("id=x"), Some(("id", "x")));
        assert_eq!(split_definition("f_1 =  "), Some(("f_1", "")));
        assert_eq!(split_definition("true == false"), None);
        assert_eq!(split_definition("1 = 2"), None);
        assert_eq!(split_definition("λx:A. x"), None);
        assert_eq!(split_definition("f x"), None);
    }
}
