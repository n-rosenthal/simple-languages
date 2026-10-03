//! Semântica natural de `stlc`: `t ⇓ v` (CBV, TAPL caps. 5 e 9).
//!
//! ```text
//!   true ⇓ true            false ⇓ false           λx:T. t ⇓ λx:T. t
//!     (E-True)               (E-False)                  (E-Abs)
//!
//!   t1 ⇓ true   t2 ⇓ v            t1 ⇓ false   t3 ⇓ v
//!   ------------------------      -------------------------
//!   if t1 then t2 else t3 ⇓ v     if t1 then t2 else t3 ⇓ v
//!          (E-IfTrue)                     (E-IfFalse)
//!
//!   t1 ⇓ λx:T. t12   t2 ⇓ v2   [x ↦ v2] t12 ⇓ v
//!   ----------------------------------------------         (E-App)
//!                    t1 t2 ⇓ v
//! ```
//!
//! O valor de um termo é outro termo (`true`, `false` ou uma abstração). Em
//! big-step, termo travado e termo divergente não têm derivação; para que a
//! recursão não estoure a pilha, há um limite de profundidade
//! ([`MAX_DEPTH`]).

use std::fmt;

use crate::common::semantics::{BigStep, Derivation, Eval, EvalDerivation};

use super::terms::Term;

/// Profundidade máxima da derivação. O limite protege contra divergência
/// (ω) e contra a pilha pequena do WebAssembly.
pub const MAX_DEPTH: usize = 200;

crate::rules! {
    pub enum EvalRule {
        True => "E-True",
        False => "E-False",
        IfTrue => "E-IfTrue",
        IfFalse => "E-IfFalse",
        Abs => "E-Abs",
        App => "E-App",
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalError {
    /// Variável livre: não há regra para ela.
    UnboundVariable { name: String },
    /// A função não avaliou para uma abstração (`true false`).
    NotAFunction { found: Term },
    /// A condição de um `if` não avaliou para um booleano.
    InvalidCondition { found: Term },
    /// Limite de profundidade excedido (provável divergência).
    TooDeep { limit: usize },
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnboundVariable { name } => write!(f, "unbound variable `{name}`"),
            Self::NotAFunction { found } => write!(f, "cannot apply `{found}`: not a function"),
            Self::InvalidCondition { found } => {
                write!(f, "if condition must be a Bool, found `{found}`")
            }
            Self::TooDeep { limit } => {
                write!(f, "evaluation exceeded the depth limit of {limit} (diverges?)")
            }
        }
    }
}

impl std::error::Error for EvalError {}

pub struct StlcBigStep;

impl StlcBigStep {
    fn eval(term: &Term, depth: usize) -> Result<EvalDerivation<Self>, EvalError> {
        if depth > MAX_DEPTH {
            return Err(EvalError::TooDeep { limit: MAX_DEPTH });
        }

        match term {
            Term::Var(name) => Err(EvalError::UnboundVariable { name: name.clone() }),

            // E-True / E-False / E-Abs
            Term::True => Ok(Derivation::axiom(
                Eval { term: term.clone(), value: term.clone() },
                EvalRule::True,
            )),
            Term::False => Ok(Derivation::axiom(
                Eval { term: term.clone(), value: term.clone() },
                EvalRule::False,
            )),
            Term::Lambda { .. } => Ok(Derivation::axiom(
                Eval { term: term.clone(), value: term.clone() },
                EvalRule::Abs,
            )),

            // E-IfTrue / E-IfFalse: só o ramo escolhido é avaliado
            Term::If { condition, then_branch, else_branch } => {
                let condition = Self::eval(condition, depth + 1)?;

                let (rule, branch) = match &condition.conclusion.value {
                    Term::True => (EvalRule::IfTrue, then_branch),
                    Term::False => (EvalRule::IfFalse, else_branch),
                    other => return Err(EvalError::InvalidCondition { found: other.clone() }),
                };

                let branch = Self::eval(branch, depth + 1)?;
                let value = branch.conclusion.value.clone();

                Ok(Derivation::node(
                    Eval { term: term.clone(), value },
                    rule,
                    vec![condition, branch],
                ))
            }

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

impl BigStep for StlcBigStep {
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
    use crate::stlc::testing::parse;

    fn value(source: &str) -> Term {
        StlcBigStep::value_of(&parse(source)).unwrap()
    }

    #[test]
    fn values_evaluate_to_themselves() {
        for source in ["λx:A. x", "true", "false"] {
            assert_eq!(value(source), parse(source), "{source}");
        }
    }

    #[test]
    fn application() {
        assert_eq!(value("(λf:A->A. f) (λy:A. y)"), parse("λy:A. y"));
    }

    #[test]
    fn negation() {
        assert_eq!(value("(λb:Bool. if b then false else true) true"), parse("false"));
        assert_eq!(value("(λb:Bool. if b then false else true) false"), parse("true"));
    }

    #[test]
    fn only_the_chosen_branch_is_evaluated() {
        // o ramo `else` não tem derivação, mas nunca é avaliado
        let d = StlcBigStep::evaluate(&parse("if true then false else (true true)")).unwrap();

        assert_eq!(d.conclusion.value, parse("false"));
        assert_eq!(d.rule, EvalRule::IfTrue);
        assert_eq!(d.premises.len(), 2);
    }

    #[test]
    fn the_derivation_has_three_premises_per_application() {
        let d = StlcBigStep::evaluate(&parse("(λx:A->A. x) (λy:A. y)")).unwrap();

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
        let d = StlcBigStep::evaluate(&parse("if true then false else true")).unwrap();

        assert_eq!(
            d.to_text(),
            "if true then false else true ⇓ false  [E-IfTrue]\n  true ⇓ true  [E-True]\n  false ⇓ false  [E-False]\n"
        );
    }

    #[test]
    fn free_variables_have_no_derivation() {
        assert_eq!(
            StlcBigStep::evaluate(&parse("x")).unwrap_err(),
            EvalError::UnboundVariable { name: "x".into() }
        );
    }

    #[test]
    fn applying_a_boolean_has_no_derivation() {
        assert_eq!(
            StlcBigStep::evaluate(&parse("true false")).unwrap_err(),
            EvalError::NotAFunction { found: parse("true") }
        );
    }

    #[test]
    fn a_non_boolean_condition_has_no_derivation() {
        assert_eq!(
            StlcBigStep::evaluate(&parse("if (λx:Bool. x) then true else false")).unwrap_err(),
            EvalError::InvalidCondition { found: parse("λx:Bool. x") }
        );
    }

    #[test]
    fn omega_hits_the_depth_limit() {
        let omega = parse("(λx:A. x x) (λx:A. x x)");

        assert_eq!(
            StlcBigStep::evaluate(&omega).unwrap_err(),
            EvalError::TooDeep { limit: MAX_DEPTH }
        );
    }
}
