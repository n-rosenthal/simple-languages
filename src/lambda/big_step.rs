//! Semântica natural de `lambda`: `t ⇓ v` (CBV, TAPL cap. 5).
//!
//! ```text
//!   λx:T. t ⇓ λx:T. t                                      (E-Abs)
//!
//!   t1 ⇓ λx:T. t12   t2 ⇓ v2   [x ↦ v2] t12 ⇓ v
//!   ----------------------------------------------         (E-App)
//!                    t1 t2 ⇓ v
//! ```
//!
//! O valor de um termo é outro termo (uma abstração). Em big-step, termo
//! travado e termo divergente não têm derivação; para que a recursão
//! não estoure a pilha, há um limite de profundidade ([`MAX_DEPTH`]).

use std::fmt;

use crate::common::semantics::{BigStep, Derivation, Eval, EvalDerivation};

use super::terms::Term;

/// Profundidade máxima da derivação. STLC sem constantes não tem termos
/// legítimos tão fundos; o limite protege contra divergência (ω).
pub const MAX_DEPTH: usize = 200;

crate::rules! {
    pub enum EvalRule {
        Abs => "E-Abs",
        App => "E-App",
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalError {
    /// Variável livre: não há regra para ela.
    UnboundVariable { name: String },
    /// A função não avaliou para uma abstração. Inalcançável enquanto os
    /// únicos valores forem abstrações; passa a existir com constantes.
    NotAFunction { found: Term },
    /// Limite de profundidade excedido (provável divergência).
    TooDeep { limit: usize },
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnboundVariable { name } => write!(f, "unbound variable `{name}`"),
            Self::NotAFunction { found } => write!(f, "cannot apply `{found}`: not a function"),
            Self::TooDeep { limit } => {
                write!(f, "evaluation exceeded the depth limit of {limit} (diverges?)")
            }
        }
    }
}

impl std::error::Error for EvalError {}

pub struct LambdaBigStep;

impl LambdaBigStep {
    fn eval(term: &Term, depth: usize) -> Result<EvalDerivation<Self>, EvalError> {
        if depth > MAX_DEPTH {
            return Err(EvalError::TooDeep { limit: MAX_DEPTH });
        }

        match term {
            Term::Var(name) => Err(EvalError::UnboundVariable { name: name.clone() }),

            // E-Abs
            Term::Lambda { .. } => Ok(Derivation::axiom(
                Eval { term: term.clone(), value: term.clone() },
                EvalRule::Abs,
            )),

            // E-App
            Term::App { func, arg } => {
                let func_derivation = Self::eval(func, depth + 1)?;
                let arg_derivation = Self::eval(arg, depth + 1)?;

                let (param, body) = match &func_derivation.conclusion.value {
                    Term::Lambda { param, body, .. } => (param.clone(), body.clone()),
                    other => return Err(EvalError::NotAFunction { found: other.clone() }),
                };

                let instantiated = body.substitute(&param, &arg_derivation.conclusion.value);
                let body_derivation = Self::eval(&instantiated, depth + 1)?;

                Ok(Derivation::node(
                    Eval {
                        term: term.clone(),
                        value: body_derivation.conclusion.value.clone(),
                    },
                    EvalRule::App,
                    vec![func_derivation, arg_derivation, body_derivation],
                ))
            }
        }
    }
}

impl BigStep for LambdaBigStep {
    type Term = Term;
    type Value = Term;
    type Rule = EvalRule;
    type Error = EvalError;

    fn evaluate(term: &Term) -> Result<EvalDerivation<Self>, EvalError> {
        Self::eval(term, 0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lambda::testing::parse;

    #[test]
    fn abstractions_evaluate_to_themselves() {
        let t = parse("λx:A. x");
        assert_eq!(LambdaBigStep::value_of(&t).unwrap(), t);
    }

    #[test]
    fn application() {
        let value = LambdaBigStep::value_of(&parse("(λf:A->A. f) (λy:A. y)")).unwrap();
        assert_eq!(value, parse("λy:A. y"));
    }

    #[test]
    fn the_derivation_has_three_premises_per_application() {
        let d = LambdaBigStep::evaluate(&parse("(λx:A->A. x) (λy:A. y)")).unwrap();

        assert_eq!(d.rule, EvalRule::App);
        assert_eq!(d.premises.len(), 3);
        assert_eq!(d.size(), 4);
        assert_eq!(
            d.postorder_rules(),
            vec![EvalRule::Abs, EvalRule::Abs, EvalRule::Abs, EvalRule::App]
        );
    }

    #[test]
    fn text_rendering() {
        let d = LambdaBigStep::evaluate(&parse("(λx:A->A. x) (λy:A. y)")).unwrap();

        assert!(d
            .to_text()
            .starts_with("(λx:A->A. x) (λy:A. y) ⇓ λy:A. y  [E-App]\n"));
    }

    #[test]
    fn free_variables_have_no_derivation() {
        assert_eq!(
            LambdaBigStep::evaluate(&parse("x")).unwrap_err(),
            EvalError::UnboundVariable { name: "x".into() }
        );
    }

    #[test]
    fn omega_hits_the_depth_limit() {
        let omega = parse("(λx:A. x x) (λx:A. x x)");

        assert_eq!(
            LambdaBigStep::evaluate(&omega).unwrap_err(),
            EvalError::TooDeep { limit: MAX_DEPTH }
        );
    }
}