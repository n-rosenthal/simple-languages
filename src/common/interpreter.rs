//! O interpretador interativo, genérico em qualquer [`Language`].
//!
//! [`Session`] guarda o estado de uma sessão (modo atual, limite de
//! passos, definições) e transforma cada linha digitada em uma [`Reply`].
//! Não faz entrada nem saída: o REPL do terminal e a página web (WASM)
//! são apenas camadas finas sobre [`Interpreter::submit`].
//!
//! Uma submissão é um *programa*: uma ou mais instruções separadas por `;;`
//! (as quebras de linha são espaço em branco, então uma instrução pode ocupar
//! várias linhas). Cada instrução pode ser:
//!
//! - um termo, executado no modo atual (`full`, `type`, `small`, ...);
//! - uma definição `nome = termo` (linguagens com variáveis; o termo é
//!   expandido na hora e as definições são substituídas nas instruções
//!   seguintes);
//! - um comando `:help`, `:mode`, `:fuel`, `:defs`, `:reset`,
//!   `:examples`, `:example N`, `:syntax`, `:rules`, `:quit`, ou `:<modo>`
//!   para trocar de modo.
//!
//! Um programa para na primeira instrução que falha. Os erros de sintaxe
//! mostram o trecho do fonte com um `^` sob o erro, com a linha e a coluna
//! contadas no programa inteiro.
//!
//! [`Interpreter::needs_more`] diz se o texto acabou cedo demais (um
//! parêntese aberto, um `λx:A.` sem corpo): o terminal e a página usam isso
//! para pedir mais uma linha em vez de mostrar um erro.
//!
//! As entradas são limitadas ([`MAX_INPUT_CHARS`] por instrução,
//! [`MAX_PROGRAM_CHARS`] e [`MAX_STATEMENTS`] no total, [`MAX_NESTING`],
//! [`MAX_FUEL`]) porque o interpretador também roda no navegador, onde a
//! pilha é pequena e um laço longo congela a aba.

use std::fmt::Write as _;
use std::str::FromStr;

use crate::common::diagnostic::{render, Diagnostic};
use crate::common::document::{blocks_to_text, Block};
use crate::common::driver::{execute_blocks, Command, Options};
use crate::common::language::{capabilities, rule_blocks, syntax_blocks, Example, Language};

/// Tamanho máximo de uma instrução, em caracteres.
pub const MAX_INPUT_CHARS: usize = 2_000;
/// Tamanho máximo de um programa (todas as instruções), em caracteres.
pub const MAX_PROGRAM_CHARS: usize = 20_000;
/// Número máximo de instruções num programa.
pub const MAX_STATEMENTS: usize = 100;
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
    /// A resposta como texto (o que o terminal imprime).
    pub text: String,
    /// A mesma resposta como blocos (títulos, texto, fórmulas); vazio nas
    /// respostas que são só uma mensagem. A página web renderiza os blocos.
    pub blocks: Vec<Block>,
    /// O usuário pediu para sair (`:quit`).
    pub quit: bool,
}

impl Reply {
    fn new(kind: ReplyKind, text: impl Into<String>) -> Self {
        Self { kind, text: text.into(), blocks: Vec::new(), quit: false }
    }

    fn with_blocks(kind: ReplyKind, blocks: Vec<Block>) -> Self {
        Self { kind, text: blocks_to_text(&blocks), blocks, quit: false }
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

    /// Os modos que a linguagem suporta (um sem big-step não tem `big`).
    fn modes(&self) -> Vec<Command>;

    /// Troca o modo, recusando um que a linguagem não suporta.
    fn select_mode(&mut self, command: Command) -> Result<(), String>;

    /// O texto acabou cedo demais, e mais linhas poderiam completá-lo?
    fn needs_more(&self, text: &str) -> bool;

    fn fuel(&self) -> usize;
    /// Limita o valor a `1..=MAX_FUEL` e devolve o valor efetivo.
    fn set_fuel(&mut self, fuel: usize) -> usize;

    /// As definições, como pares `(nome, termo)`.
    fn definitions(&self) -> Vec<(String, String)>;

    /// A gramática da linguagem, como blocos (vazio se não declarada).
    fn syntax(&self) -> Vec<Block>;

    /// As regras de tipagem, das duas semânticas e da máquina, como blocos.
    fn rules(&self) -> Vec<Block>;

    /// Executa um programa (uma ou mais instruções separadas por `;;`).
    fn submit(&mut self, text: &str) -> Reply;
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

    /// Lê `source`, que começa na posição `shift` de `whole` (para o erro
    /// mostrar a linha e a coluna do programa inteiro), e expande as definições.
    fn parse(&self, whole: &str, shift: usize, source: &str) -> Result<L::Term, Reply> {
        L::parse(source)
            .map(|term| self.expand(term))
            .map_err(|e| Reply::error(format!("syntax error: {}", render(whole, shift, &e))))
    }

    fn evaluate(&self, whole: &str, shift: usize, source: &str) -> Reply {
        let term = match self.parse(whole, shift, source) {
            Ok(term) => term,
            Err(reply) => return reply,
        };

        match execute_blocks::<L>(self.mode, &term, &self.options) {
            Ok(blocks) => Reply::with_blocks(ReplyKind::Output, blocks),
            Err(text) => Reply::error(text),
        }
    }

    fn define(&mut self, whole: &str, shift: usize, name: &str, source: &str) -> Reply {
        if !L::SUPPORTS_DEFINITIONS {
            return Reply::error(format!(
                "{} has no variables, so definitions are not available",
                L::NAME
            ));
        }
        if source.is_empty() {
            return Reply::error(format!("missing term after `{name} =`"));
        }

        let term = match self.parse(whole, shift, source) {
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

    fn switch_mode(&mut self, command: Command) -> Reply {
        match self.pick_mode(command) {
            Ok(()) => Reply::info(format!("mode: {}", command.name())),
            Err(message) => Reply::error(message),
        }
    }

    fn pick_mode(&mut self, command: Command) -> Result<(), String> {
        if !command.is_available(capabilities::<L>()) {
            return Err(command.unavailable(L::NAME));
        }
        self.mode = command;
        Ok(())
    }

    fn change_mode(&mut self, name: &str) -> Reply {
        match Command::from_str(name) {
            Ok(command) => self.switch_mode(command),
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

    fn show_syntax(&self) -> Reply {
        let blocks = syntax_blocks::<L>();
        if blocks.is_empty() {
            return Reply::info(format!("{} does not list its syntax", L::NAME));
        }
        Reply::with_blocks(ReplyKind::Info, blocks)
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

    fn available_modes(&self) -> Vec<Command> {
        let caps = capabilities::<L>();
        Command::ALL.into_iter().filter(|c| c.is_available(caps)).collect()
    }

    fn help(&self) -> String {
        let modes: Vec<_> = self.available_modes().iter().map(|c| c.name()).collect();
        let mut out = String::new();

        let _ = writeln!(out, "{}: {}", L::NAME, L::DESCRIPTION);
        let _ = writeln!(out, "  <term>               run the term in the current mode ({})", self.mode.name());
        let _ = writeln!(out, "  <a> ;; <b>           several statements; a statement may span lines");
        if L::SUPPORTS_DEFINITIONS {
            let _ = writeln!(out, "  name = <term>        define a name, usable in later lines");
        }
        let _ = writeln!(out, "  :mode <m>  or  :<m>  modes: {}", modes.join(", "));
        let _ = writeln!(out, "  :fuel <n>            step limit, 1..{MAX_FUEL} (now {})", self.options.fuel);
        if L::SUPPORTS_DEFINITIONS {
            let _ = writeln!(out, "  :defs  :reset        list / clear definitions");
        }
        let _ = writeln!(out, "  :examples            list examples; `:example N` runs one");
        let _ = writeln!(out, "  :syntax  :rules      the grammar / the inference rules (LaTeX in the terminal)");
        let _ = writeln!(out, "  :help  :quit");
        out
    }

    /// Uma instrução: um comando `:...`, uma definição ou um termo. `whole` é o
    /// programa inteiro, para os erros mostrarem a posição nele.
    fn statement(&mut self, whole: &str, statement: &Statement<'_>) -> Reply {
        let source = statement.source;

        if source.chars().count() > MAX_INPUT_CHARS {
            return Reply::error(format!("input too long (limit: {MAX_INPUT_CHARS} characters)"));
        }

        if let Some(command) = source.strip_prefix(':') {
            return self.meta(command);
        }

        if max_nesting(source) > MAX_NESTING {
            return Reply::error(format!("too many nested parentheses (limit: {MAX_NESTING})"));
        }

        match split_definition(source) {
            Some((name, body)) => {
                let shift = statement.offset + chars_before(source, body);
                self.define(whole, shift, name, body)
            }
            None => self.evaluate(whole, statement.offset, source),
        }
    }

    /// Várias instruções: cada uma mostra o que foi executado e o resultado, e
    /// o programa para na primeira que falha.
    fn program(&mut self, whole: &str, statements: &[Statement<'_>]) -> Reply {
        let mut blocks = Vec::new();
        let mut kind = ReplyKind::Info;
        let mut quit = false;

        for statement in statements {
            let first_line = statement.source.lines().next().unwrap_or("");
            let more = if statement.source.lines().count() > 1 { " …" } else { "" };
            blocks.push(Block::Text(format!("> {first_line}{more}\n")));

            let reply = self.statement(whole, statement);
            quit |= reply.quit;

            if reply.blocks.is_empty() {
                let text = reply.text;
                blocks.push(match reply.kind {
                    ReplyKind::Error => Block::Error(text),
                    _ => Block::Text(text),
                });
            } else {
                blocks.extend(reply.blocks);
            }

            match reply.kind {
                ReplyKind::Error => {
                    kind = ReplyKind::Error;
                    break;
                }
                ReplyKind::Output => kind = ReplyKind::Output,
                ReplyKind::Info => {}
            }
        }

        Reply { kind, text: blocks_to_text(&blocks), blocks, quit }
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

            ("syntax", _) => self.show_syntax(),
            ("rules", _) => Reply::with_blocks(ReplyKind::Info, rule_blocks::<L>()),

            ("examples", _) => self.list_examples(),
            ("example" | "load", Some(n)) => self.run_example(n),
            ("example" | "load", None) => self.list_examples(),

            (other, _) => match Command::from_str(other) {
                Ok(command) => self.switch_mode(command),
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

/// Uma instrução de um programa: o texto e a posição dele (em `char`s) no
/// programa inteiro.
struct Statement<'a> {
    source: &'a str,
    offset: usize,
}

/// Divide o programa em instruções por `;;`, ignorando as vazias. (`;;` não faz
/// parte da sintaxe de nenhuma linguagem, ao contrário de `;`, que o TAPL usa
/// para sequência.)
fn split_statements(text: &str) -> Vec<Statement<'_>> {
    let mut statements = Vec::new();
    let mut cursor = 0; // em bytes

    for piece in text.split(";;") {
        let source = piece.trim();
        if !source.is_empty() {
            let leading = piece.len() - piece.trim_start().len();
            statements.push(Statement {
                source,
                offset: text[..cursor + leading].chars().count(),
            });
        }
        cursor += piece.len() + 2;
    }

    statements
}

/// Quantos `char`s há em `outer` antes de `inner`, que é um pedaço de `outer`.
fn chars_before(outer: &str, inner: &str) -> usize {
    let byte = inner.as_ptr() as usize - outer.as_ptr() as usize;
    outer[..byte].chars().count()
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

    fn modes(&self) -> Vec<Command> {
        self.available_modes()
    }

    fn select_mode(&mut self, command: Command) -> Result<(), String> {
        self.pick_mode(command)
    }

    fn needs_more(&self, text: &str) -> bool {
        let text = text.trim();
        if text.is_empty() || text.starts_with(':') || text.ends_with(";;") {
            return false;
        }

        // só a última instrução pode estar incompleta
        let Some(last) = split_statements(text).pop() else {
            return false;
        };
        if last.source.starts_with(':') {
            return false;
        }

        let source = match split_definition(last.source) {
            Some((_, "")) => return true, // `f =` espera o termo
            Some((_, body)) => body,
            None => last.source,
        };

        L::parse(source).is_err_and(|e| e.is_incomplete())
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

    fn syntax(&self) -> Vec<Block> {
        syntax_blocks::<L>()
    }

    fn rules(&self) -> Vec<Block> {
        rule_blocks::<L>()
    }

    fn submit(&mut self, text: &str) -> Reply {
        let text = text.trim();

        if text.is_empty() {
            return Reply::info("");
        }
        if text.chars().count() > MAX_PROGRAM_CHARS {
            return Reply::error(format!("program too long (limit: {MAX_PROGRAM_CHARS} characters)"));
        }

        let statements = split_statements(text);
        match statements.len() {
            0 => Reply::info(""),
            1 => self.statement(text, &statements[0]),
            n if n > MAX_STATEMENTS => {
                Reply::error(format!("too many statements (limit: {MAX_STATEMENTS})"))
            }
            _ => self.program(text, &statements),
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
