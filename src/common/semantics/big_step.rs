//! Semântica natural (big-step): o julgamento `t ⇓ v`.
//!
//! Não há iteração: a avaliação é recursiva e produz uma [`Derivation`]
//! completa. Por isso não existe a noção de "travado". Se nenhuma
//! derivação existe, o resultado é um erro. Em big-step, um termo que
//! trava e um que diverge são indistinguíveis (TAPL, cap. 3); ambos
//! simplesmente não têm derivação.

use super::derivation::{Derivation, Eval};
use super::Rule;

/// A derivação de avaliação de uma linguagem `B`.
pub type EvalDerivation<B> = Derivation<
    Eval<<B as BigStep>::Term, <B as BigStep>::Value>,
    <B as BigStep>::Rule,
>;

pub trait BigStep {
    type Term: Clone;
    type Value: Clone;
    type Rule: Rule;
    type Error: std::error::Error;

    /// Deriva `t ⇓ v`.
    fn evaluate(term: &Self::Term) -> Result<EvalDerivation<Self>, Self::Error>;

    /// Só o valor, descartando a derivação.
    fn value_of(term: &Self::Term) -> Result<Self::Value, Self::Error> {
        Self::evaluate(term).map(|derivation| derivation.conclusion.value)
    }
}
