//! O sistema de tipos: o julgamento `Γ ⊢ t : T`.
//!
//! Cada linguagem implementa [`Typing`]. O resultado de uma checagem
//! bem-sucedida é uma [`Derivation`] completa, não só o tipo: a ordem
//! pós-fixada das regras e a árvore em LaTeX saem dela.
//!
//! Se nenhuma derivação existe, o resultado é um erro de tipo. Isso é o
//! oposto da semântica estrutural, em que "nenhuma regra se aplica" é um
//! resultado normal (um termo travado).

use std::convert::Infallible;
use std::fmt;
use std::marker::PhantomData;

use crate::common::ToLatex;

use super::derivation::{Derivation, Typed};
use super::Rule;

/// A derivação de tipagem de uma linguagem `T`.
pub type TypingDerivation<T> = Derivation<
    Typed<<T as Typing>::Term, <T as Typing>::Type>,
    <T as Typing>::Rule,
>;

pub trait Typing {
    /// A linguagem tem sistema de tipos? Falso nos marcadores ([`NoTyping`]).
    const DEFINED: bool = true;

    type Term: Clone;
    type Type: Clone;
    type Rule: Rule;
    type Error: std::error::Error;

    /// Deriva `⊢ t : T` para um termo fechado (Γ vazio).
    ///
    /// Linguagens com contexto mantêm um `check_in` próprio, que estende Γ,
    /// e chamam-no a partir daqui com Γ vazio.
    fn check(term: &Self::Term) -> Result<TypingDerivation<Self>, Self::Error>;

    /// Só o tipo, descartando a derivação.
    fn type_of(term: &Self::Term) -> Result<Self::Type, Self::Error> {
        Self::check(term).map(|derivation| derivation.conclusion.ty)
    }

    /// O termo é bem tipado?
    fn is_well_typed(term: &Self::Term) -> bool {
        Self::check(term).is_ok()
    }
}

// =============================================================================
// Linguagens sem tipos
// =============================================================================

/// O único tipo de uma linguagem não-tipada.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Untyped;

impl fmt::Display for Untyped {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Untyped")
    }
}

impl ToLatex for Untyped {
    fn to_latex(&self) -> String {
        r"\mathsf{Untyped}".to_string()
    }
}

crate::rules! {
    pub enum NoTypingRule {
        Any => "T-Any" { [] => r"\vdash t : \mathsf{Untyped}" },
    }
}

/// `Typing` para linguagens sem sistema de tipos. Para o código genérico (as
/// leis, o driver) a tipagem simplesmente não existe (`DEFINED` é falso); se
/// alguém a chamar assim mesmo, todo termo é bem tipado, com o tipo
/// [`Untyped`]. Escreve-se `type Typing = NoTyping<Term>`.
pub struct NoTyping<T>(PhantomData<fn() -> T>);

impl<T: Clone> Typing for NoTyping<T> {
    const DEFINED: bool = false;

    type Term = T;
    type Type = Untyped;
    type Rule = NoTypingRule;
    type Error = Infallible;

    fn check(term: &T) -> Result<TypingDerivation<Self>, Infallible> {
        Ok(Derivation::axiom(
            Typed::closed(term.clone(), Untyped),
            NoTypingRule::Any,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::semantics::toy::{self, add, boolean, num, ToyTyping, TypingRule};

    #[test]
    fn literals() {
        assert_eq!(ToyTyping::type_of(&num(1)).unwrap(), toy::Type::Nat);
        assert_eq!(ToyTyping::type_of(&boolean(true)).unwrap(), toy::Type::Bool);
    }

    #[test]
    fn derivation_is_a_tree_and_postorder_matches_the_old_list() {
        let d = ToyTyping::check(&add(num(1), num(2))).unwrap();

        assert_eq!(d.size(), 3);
        assert_eq!(
            d.postorder_rules(),
            vec![TypingRule::Num, TypingRule::Num, TypingRule::Add]
        );
    }

    #[test]
    fn ill_typed_terms_have_no_derivation() {
        let bad = add(num(1), boolean(true));
        assert!(ToyTyping::check(&bad).is_err());
        assert!(!ToyTyping::is_well_typed(&bad));
    }

    #[test]
    fn the_conclusion_judges_the_whole_term() {
        let d = ToyTyping::check(&add(num(1), num(2))).unwrap();
        assert_eq!(d.conclusion.to_string(), "⊢ (1 + 2) : Nat");
        assert_eq!(d.conclusion.to_latex(), r"\vdash (1 + 2) : \mathsf{Nat}");
    }

    #[test]
    fn renders_latex_tree() {
        let tree = ToyTyping::check(&add(num(1), num(2))).unwrap().to_latex_tree();
        assert!(tree.starts_with(r"\inferrule*[right=\textsc{T-Add}]"));
    }

    #[test]
    fn untyped_languages_judge_everything_well_typed() {
        let t = add(num(1), boolean(true));

        assert!(NoTyping::<toy::Term>::is_well_typed(&t));
        assert_eq!(NoTyping::<toy::Term>::type_of(&t).unwrap(), Untyped);
    }
}
