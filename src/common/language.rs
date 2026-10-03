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

use crate::common::machine_language::{compilation_is_correct, Compile};
use crate::common::semantics::laws::{
    final_states_do_not_step, preservation, small_step_agrees_with_big_step,
    trace_is_connected, well_typed_evaluates, well_typed_never_gets_stuck,
};
use crate::common::semantics::{run, BigStep, Step, Typing};
use crate::common::ToLatex;

/// Um termo de exemplo, com um título, para o REPL e para a página web.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Example {
    pub title: &'static str,
    pub source: &'static str,
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

    type SyntaxError: std::error::Error;

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

    /// `[name ↦ value] term`: usada pelo interpretador para expandir
    /// definições. O padrão não substitui nada (linguagens sem variáveis).
    fn substitute(term: &Self::Term, _name: &str, _value: &Self::Term) -> Self::Term {
        term.clone()
    }
}

/// Roda todas as leis genéricas sobre `term` e devolve os nomes das que
/// *falham* (vazio = tudo certo). As leis que supõem um termo bem tipado
/// valem vacuamente para os que não são.
pub fn law_violations<L: Language>(term: &L::Term) -> Vec<&'static str> {
    let mut violated = Vec::new();
    let mut check = |name: &'static str, holds: bool| {
        if !holds {
            violated.push(name);
        }
    };

    check(
        "final states do not step",
        final_states_do_not_step::<L::Small>(term),
    );
    check(
        "small-step traces are connected",
        trace_is_connected(&run::<L::Small>(term.clone())),
    );
    check(
        "small-step agrees with big-step",
        small_step_agrees_with_big_step::<L::Small, L::Big>(term),
    );
    check(
        "well-typed terms evaluate (big-step)",
        well_typed_evaluates::<L::Typing, L::Big>(term),
    );
    check(
        "progress",
        well_typed_never_gets_stuck::<L::Typing, L::Small>(term),
    );
    check("preservation", preservation::<L::Typing, L::Small>(term));
    check(
        "compilation is correct",
        compilation_is_correct::<L::Compiler, L::Big>(term),
    );

    violated
}
