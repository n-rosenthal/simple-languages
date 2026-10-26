//! Semântica natural de `nameless_terms`: t ⇓ v (call-by-value).
//!
//! ```text
//!                    ---------------------- (E-Abs)
//!                    λ. t ⇓ λ. t
//!
//!   t1 ⇓ λ. t12    t2 ⇓ v2    t12[v2/0] ⇓ v
//!   ------------------------------------------------ (E-App)
//!                      t1 t2 ⇓ v
//! ```
//!
//! Os termos usam índices de De Bruijn. A substituição na regra E-App
//! utiliza `substitute_top`, que ajusta os índices para preservar o
//! significado das variáveis sob abstrações.
//!
//! Termos travados e termos divergentes não têm derivação. Para evitar
//! recursão ilimitada, impomos um limite de profundidade.

use std::fmt;

use crate::common::semantics::{BigStep, Derivation, Eval, EvalDerivation};

use super::substitution::substitute_top;
use super::terms::Term;

/// Profundidade máxima da derivação.
pub const MAX_DEPTH: usize = 200;

crate::rules! {
    pub enum EvalRule {
        Abs => "E-Abs" {
            [] => r"\lambda.\, t \Downarrow \lambda.\, t"
        },
        App => "E-App" {
            [
                r"t_1 \Downarrow \lambda.\, t_{12}",
                r"t_2 \Downarrow v_2",
                r"t_{12}[v_2/0] \Downarrow v"
            ] => r"t_1\ t_2 \Downarrow v"
        },
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalError {
    /// Variável livre ou índice incompatível com o contexto.
    UnboundVariable {
        index: usize,
    },

    /// A função avaliou para um termo que não é uma abstração.
    NotAFunction {
        found: Term,
    },

    /// Limite de profundidade excedido.
    TooDeep {
        limit: usize,
    },

    /// A substituição exigiu um deslocamento de índices inválido.
    SubstitutionFailed,
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnboundVariable { index } => {
                write!(f, "unbound De Bruijn index `{index}`")
            }
            Self::NotAFunction { found } => {
                write!(f, "cannot apply `{found}`: not a function")
            }
            Self::TooDeep { limit } => {
                write!(
                    f,
                    "evaluation exceeded the depth limit of {limit} (diverges?)"
                )
            }
            Self::SubstitutionFailed => {
                write!(f, "substitution failed: invalid De Bruijn index shift")
            }
        }
    }
}

impl std::error::Error for EvalError {}

pub struct NamelessTermsBigStep;

impl NamelessTermsBigStep {
    fn eval(term: &Term, depth: usize) -> Result<EvalDerivation<Self>, EvalError> {
        if depth > MAX_DEPTH {
            return Err(EvalError::TooDeep { limit: MAX_DEPTH });
        }

        match term {
            Term::Var(index) => Err(EvalError::UnboundVariable {
                index: *index,
            }),

            // E-Abs: abstrações são valores.
            Term::Abs(_) => Ok(Derivation::axiom(
                Eval {
                    term: term.clone(),
                    value: term.clone(),
                },
                EvalRule::Abs,
            )),

            // E-App: avaliar função, argumento e, por fim, o corpo
            // instanciado com o valor do argumento.
            Term::App(func, arg) => {
                let func_derivation = Self::eval(func, depth + 1)?;
                let arg_derivation = Self::eval(arg, depth + 1)?;

                let body = match &func_derivation.conclusion.value {
                    Term::Abs(body) => body,
                    other => {
                        return Err(EvalError::NotAFunction {
                            found: other.clone(),
                        });
                    }
                };

                let argument_value = &arg_derivation.conclusion.value;

                let instantiated = substitute_top(body, argument_value)
                    .ok_or(EvalError::SubstitutionFailed)?;

                let body_derivation = Self::eval(&instantiated, depth + 1)?;

                Ok(Derivation::node(
                    Eval {
                        term: term.clone(),
                        value: body_derivation.conclusion.value.clone(),
                    },
                    EvalRule::App,
                    vec![
                        func_derivation,
                        arg_derivation,
                        body_derivation,
                    ],
                ))
            }
        }
    }
}

impl BigStep for NamelessTermsBigStep {
    type Term = Term;
    type Value = Term;
    type Rule = EvalRule;
    type Error = EvalError;

    fn evaluate(term: &Term) -> Result<EvalDerivation<Self>, Self::Error> {
        Self::eval(term, 0)
    }
}