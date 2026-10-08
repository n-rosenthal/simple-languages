//! O trait [`Language`]: o que uma linguagem do TAPL fornece ao
//! backbone.
//!
//! `Language` é um *pacote*: um tipo marcador por linguagem cujos tipos
//! associados apontam para os marcadores de cada semântica (`Typing`,
//! `Step`, `BigStep`, `Compile`). Com isso o driver, o CLI e o conjunto
//! de leis são escritos uma vez, para qualquer linguagem.
//!
//! `Type` e `Value` aparecem *duas vezes* de propósito: como tipos do
//! pacote (onde declaramos `Display + ToLatex`) e como parâmetros das
//! semânticas (`Typing<Type = Self::Type>`). O compilador só propaga
//! limites declarados em tipos associados do próprio `Self`.
//!
//! Linguagem sem tipos: use `type Typing = NoTyping<Self::Term>`.

use std::fmt::Display;

use crate::common::diagnostic::Diagnostic;
use crate::common::document::Block;
use crate::common::machine_language::{compilation_is_correct, Compile, MachineRule};
use crate::common::semantics::laws::{
    final_states_do_not_step, preservation, small_step_agrees_with_big_step,
    trace_is_connected, well_typed_evaluates, well_typed_never_gets_stuck,
};
use crate::common::semantics::{run, BigStep, Rule, Step, Typing};
use crate::common::ToLatex;

/// Um termo de exemplo, com um título, para o REPL e para a página web.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Example {
    pub title: &'static str,
    pub source: &'static str,
}

/// Uma categoria sintática: `t ::= x | λx:T. t | ...` (em LaTeX, modo matemático).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Syntax {
    /// O nome da categoria, para leitores: `termos`, `valores`, `tipos`.
    pub title: &'static str,
    /// A metavariável: `t`, `v`, `T`.
    pub meta: &'static str,
    pub productions: &'static [&'static str],
}

pub trait Language {
    /// Nome usado no CLI (`tapl lambda ...`).
    const NAME: &'static str;
    /// Uma linha descrevendo a linguagem.
    const DESCRIPTION: &'static str = "";
    /// A linguagem tem variáveis livres, de modo que o interpretador pode
    /// oferecer definições (`nome = termo`)?
    const SUPPORTS_DEFINITIONS: bool = false;

    type Term: Clone + PartialEq + Display + ToLatex;
    type Type: Clone + PartialEq + Display + ToLatex;
    /// O valor da semântica natural. Converte-se em termo para que se possa
    /// compará-lo ao resultado da semântica estrutural.
    type Value: Clone + Display + ToLatex + Into<Self::Term>;

    type SyntaxError: std::error::Error + Diagnostic;

    type Typing: Typing<Term = Self::Term, Type = Self::Type>;
    type Small: Step<State = Self::Term>;
    type Big: BigStep<Term = Self::Term, Value = Self::Value>;
    type Compiler: Compile<Source = Self::Term, Value = Self::Value>;

    /// Texto → termo (scanner, lexer e parser da linguagem).
    fn parse(source: &str) -> Result<Self::Term, Self::SyntaxError>;

    /// Exemplos mostrados no REPL (`:examples`) e na página web.
    fn examples() -> &'static [Example] {
        &[]
    }

    /// A gramática da linguagem, mostrada por `:syntax` e na página web.
    fn syntax() -> &'static [Syntax] {
        &[]
    }

    /// `[name ↦ value] term`: usada pelo interpretador para expandir
    /// definições. O padrão não substitui nada (linguagens sem variáveis).
    fn substitute(term: &Self::Term, _name: &str, _value: &Self::Term) -> Self::Term {
        term.clone()
    }
}

/// O que uma linguagem define: quais semânticas existem (as demais são
/// marcadores `NoTyping`, `NoSmallStep`, `NoBigStep` e `NoCompile`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Capabilities {
    pub typing: bool,
    pub small_step: bool,
    pub big_step: bool,
    pub compile: bool,
}

pub fn capabilities<L: Language>() -> Capabilities {
    Capabilities {
        typing: <L::Typing as Typing>::DEFINED,
        small_step: <L::Small as Step>::DEFINED,
        big_step: <L::Big as BigStep>::DEFINED,
        compile: <L::Compiler as Compile>::AVAILABLE,
    }
}

/// Roda as leis genéricas que se aplicam à linguagem sobre `term` e devolve os
/// nomes das que *falham* (vazio = tudo certo). Cada lei relaciona duas ou
/// mais semânticas e só roda se todas existirem. As que supõem um termo bem
/// tipado valem vacuamente para os que não são.
pub fn law_violations<L: Language>(term: &L::Term) -> Vec<&'static str> {
    let caps = capabilities::<L>();
    let mut violated = Vec::new();
    let mut check = |name: &'static str, holds: bool| {
        if !holds {
            violated.push(name);
        }
    };

    if caps.small_step {
        check(
            "final states do not step",
            final_states_do_not_step::<L::Small>(term),
        );
        check(
            "small-step traces are connected",
            trace_is_connected(&run::<L::Small>(term.clone())),
        );
    }
    if caps.small_step && caps.big_step {
        check(
            "small-step agrees with big-step",
            small_step_agrees_with_big_step::<L::Small, L::Big>(term),
        );
    }
    if caps.typing && caps.big_step {
        check(
            "well-typed terms evaluate (big-step)",
            well_typed_evaluates::<L::Typing, L::Big>(term),
        );
    }
    if caps.typing && caps.small_step {
        check(
            "progress",
            well_typed_never_gets_stuck::<L::Typing, L::Small>(term),
        );
        check("preservation", preservation::<L::Typing, L::Small>(term));
    }
    if caps.compile && caps.big_step {
        check(
            "compilation is correct",
            compilation_is_correct::<L::Compiler, L::Big>(term),
        );
    }

    violated
}

/// Quantas leis se aplicam a `L` (as de `law_violations`).
pub fn applicable_laws<L: Language>() -> usize {
    let caps = capabilities::<L>();
    let mut count = 0;
    if caps.small_step {
        count += 2;
    }
    if caps.small_step && caps.big_step {
        count += 1;
    }
    if caps.typing && caps.big_step {
        count += 1;
    }
    if caps.typing && caps.small_step {
        count += 2;
    }
    if caps.compile && caps.big_step {
        count += 1;
    }
    count
}

// =============================================================================
// Referência: sintaxe e regras
// =============================================================================

/// A gramática como uma tabela `array` alinhada em `::=`.
fn syntax_latex(items: &[Syntax]) -> String {
    let mut out = String::from(r"\begin{array}{llcl}");

    for (i, item) in items.iter().enumerate() {
        for (j, production) in item.productions.iter().enumerate() {
            let (title, meta, separator) = if j == 0 {
                (format!(r"\text{{{}}}", item.title), item.meta.to_string(), "::=")
            } else {
                (String::new(), String::new(), r"\mid")
            };

            let end = j + 1 == item.productions.len();
            let last_group = i + 1 == items.len();
            let line_break = match (end, last_group) {
                (true, true) => "",
                (true, false) => r" \\[0.7em]",
                (false, _) => r" \\",
            };

            out.push_str(&format!("\n{title} & {meta} & {separator} & {production}{line_break}"));
        }
    }

    out.push_str("\n\\end{array}");
    out
}

/// A gramática de `L`, como blocos para o terminal e para a página.
pub fn syntax_blocks<L: Language>() -> Vec<Block> {
    let items = L::syntax();
    if items.is_empty() {
        return Vec::new();
    }

    let latex = syntax_latex(items);
    vec![
        Block::Heading("sintaxe".to_string()),
        Block::math(latex.clone(), latex.clone(), latex),
    ]
}

fn rule_group<R: Rule>(out: &mut Vec<Block>, title: &str, judgment: &str, rules: &[R]) {
    if rules.is_empty() {
        return;
    }

    out.push(Block::Heading(title.to_string()));
    out.push(Block::math(judgment, judgment, judgment));

    for rule in rules {
        let (web, tex) = match rule.schema() {
            Some(schema) => (schema.to_katex(rule.name()), schema.to_latex(rule.name())),
            None => (rule.to_katex(), rule.to_latex()),
        };
        out.push(Block::math(tex.clone(), web, tex));
    }
}

/// As regras de `L`, agrupadas por julgamento: tipagem, as duas semânticas e
/// a máquina virtual (compartilhada por todas as linguagens).
pub fn rule_blocks<L: Language>() -> Vec<Block> {
    let caps = capabilities::<L>();
    let mut out = Vec::new();

    if caps.typing {
        rule_group(
            &mut out,
            "tipagem",
            r"\Gamma \vdash t : T",
            <<L::Typing as Typing>::Rule as Rule>::all(),
        );
    }
    if caps.small_step {
        rule_group(
            &mut out,
            "semântica estrutural (small-step)",
            r"t \to t'",
            <<L::Small as Step>::Rule as Rule>::all(),
        );
    }
    if caps.big_step {
        rule_group(
            &mut out,
            "semântica natural (big-step)",
            r"t \Downarrow v",
            <<L::Big as BigStep>::Rule as Rule>::all(),
        );
    }
    if caps.compile {
        rule_group(
            &mut out,
            "máquina virtual (memória μ omitida onde não é usada)",
            r"\langle c,\ s,\ e,\ f \rangle \to \langle c',\ s',\ e',\ f' \rangle",
            MachineRule::all(),
        );
    }

    out
}
