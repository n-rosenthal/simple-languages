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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::semantics::toy::*;

    #[test]
    fn evaluates_nested_additions() {
        let t = add(add(num(1), num(2)), num(3));
        assert_eq!(ToyBigStep::value_of(&t).unwrap(), Value::Num(6));
    }

    #[test]
    fn derivation_records_every_rule_application() {
        let d = ToyBigStep::evaluate(&add(num(1), num(2))).unwrap();

        assert_eq!(d.conclusion.value, Value::Num(3));
        assert_eq!(
            d.postorder_rules(),
            vec![EvalRule::Num, EvalRule::Num, EvalRule::Add]
        );
        assert_eq!(
            d.to_text(),
            "(1 + 2) ⇓ 3  [E-Add]\n  1 ⇓ 1  [E-Num]\n  2 ⇓ 2  [E-Num]\n"
        );
    }

    #[test]
    fn no_derivation_means_an_error_not_a_stuck_state() {
        assert!(ToyBigStep::evaluate(&add(num(1), boolean(true))).is_err());
    }
}
