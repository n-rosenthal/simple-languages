//! Semântica natural de `lambda`: `t ⇓ v` (call-by-value).
//!
//! ```text
//!                       t1 ⇓ λx. t12   t2 ⇓ v2   [x ↦ v2] t12 ⇓ v
//!   λx. t ⇓ λx. t       ----------------------------------------   (E-App)
//!      (E-Abs)                        t1 t2 ⇓ v
//! ```
//!
//! Termo travado e termo divergente não têm derivação. Para que a recursão
//! não estoure a pilha há um limite de profundidade ([`MAX_DEPTH`]).

use std::fmt;

use crate::common::semantics::{BigStep, Derivation, Eval, EvalDerivation};

use super::terms::Term;

/// Profundidade máxima da derivação (protege contra ω e contra a pilha
/// pequena do WebAssembly).
pub const MAX_DEPTH: usize = 200;

crate::rules! {
    pub enum EvalRule {
        Abs => "E-Abs" { [] => r"\lambda x.\, t \Downarrow \lambda x.\, t" },
        App => "E-App" {
            [
                r"t_1 \Downarrow \lambda x.\, t_{12}",
                r"t_2 \Downarrow v_2",
                r"[x \mapsto v_2]\, t_{12} \Downarrow v"
            ] => r"t_1\ t_2 \Downarrow v"
        },
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalError {
    /// Variável livre: não há regra para ela.
    UnboundVariable { name: String },
    /// A função não avaliou para uma abstração (não acontece com termos
    /// fechados: todo valor é uma abstração).
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

            Term::Lambda { .. } => Ok(Derivation::axiom(
                Eval { term: term.clone(), value: term.clone() },
                EvalRule::Abs,
            )),

            Term::App { func, arg } => {
                let func_derivation = Self::eval(func, depth + 1)?;
                let arg_derivation = Self::eval(arg, depth + 1)?;

                let (param, body) = match &func_derivation.conclusion.value {
                    Term::Lambda { param, body } => (param.clone(), body.clone()),
                    other => return Err(EvalError::NotAFunction { found: other.clone() }),
                };

                let instantiated = body.substitute(&param, &arg_derivation.conclusion.value);
                let body_derivation = Self::eval(&instantiated, depth + 1)?;

                Ok(Derivation::node(
                    Eval { term: term.clone(), value: body_derivation.conclusion.value.clone() },
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
