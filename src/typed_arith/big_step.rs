//! Semântica natural de `typed-arith`: `t ⇓ v` (TAPL, exercício 3.5.17).
//!
//! ```text
//!   v ⇓ v   (B-Value, para true, false e 0)
//!
//!   t1 ⇓ true   t2 ⇓ v2              t1 ⇓ false   t3 ⇓ v3
//!   -------------------------        -------------------------
//!   if t1 then t2 else t3 ⇓ v2       if t1 then t2 else t3 ⇓ v3
//!
//!   t1 ⇓ nv1                t1 ⇓ 0              t1 ⇓ succ nv1
//!   ----------------        -----------         ------------------
//!   succ t1 ⇓ succ nv1      pred t1 ⇓ 0         pred t1 ⇓ nv1
//!
//!   t1 ⇓ 0                  t1 ⇓ succ nv1
//!   ------------------      ----------------------
//!   iszero t1 ⇓ true        iszero t1 ⇓ false
//! ```
//!
//! O valor de um termo é outro termo. Um termo travado não tem derivação:
//! é um erro de avaliação.

use std::fmt;

use crate::common::semantics::{BigStep, Derivation, Eval, EvalDerivation};

use super::terms::Term;

crate::rules! {
    pub enum EvalRule {
        Value => "B-Value" { [] => r"v \Downarrow v" },
        IfTrue => "B-IfTrue" {
            [r"t_1 \Downarrow \mathsf{true}", r"t_2 \Downarrow v_2"]
                => r"\mathsf{if}\ t_1\ \mathsf{then}\ t_2\ \mathsf{else}\ t_3 \Downarrow v_2"
        },
        IfFalse => "B-IfFalse" {
            [r"t_1 \Downarrow \mathsf{false}", r"t_3 \Downarrow v_3"]
                => r"\mathsf{if}\ t_1\ \mathsf{then}\ t_2\ \mathsf{else}\ t_3 \Downarrow v_3"
        },
        Succ => "B-Succ" {
            [r"t_1 \Downarrow nv_1"] => r"\mathsf{succ}\ t_1 \Downarrow \mathsf{succ}\ nv_1"
        },
        PredZero => "B-PredZero" {
            [r"t_1 \Downarrow 0"] => r"\mathsf{pred}\ t_1 \Downarrow 0"
        },
        PredSucc => "B-PredSucc" {
            [r"t_1 \Downarrow \mathsf{succ}\ nv_1"] => r"\mathsf{pred}\ t_1 \Downarrow nv_1"
        },
        IsZeroZero => "B-IszeroZero" {
            [r"t_1 \Downarrow 0"] => r"\mathsf{iszero}\ t_1 \Downarrow \mathsf{true}"
        },
        IsZeroSucc => "B-IszeroSucc" {
            [r"t_1 \Downarrow \mathsf{succ}\ nv_1"] => r"\mathsf{iszero}\ t_1 \Downarrow \mathsf{false}"
        },
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalError {
    /// A condição de um `if` não avaliou para um booleano.
    InvalidCondition { found: Term },
    /// O argumento de `succ`, `pred` ou `iszero` não avaliou para um número.
    ExpectedNat { operator: &'static str, found: Term },
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidCondition { found } => {
                write!(f, "if condition must be a Bool, found `{found}`")
            }
            Self::ExpectedNat { operator, found } => {
                write!(f, "`{operator}` expects a number, found `{found}`")
            }
        }
    }
}

impl std::error::Error for EvalError {}

pub struct TypedArithBigStep;

fn axiom(term: &Term) -> EvalDerivation<TypedArithBigStep> {
    Derivation::axiom(Eval { term: term.clone(), value: term.clone() }, EvalRule::Value)
}

impl TypedArithBigStep {
    /// O operando de um operador unário, avaliado para um valor numérico.
    fn numeric(
        operand: &Term,
        operator: &'static str,
    ) -> Result<(EvalDerivation<Self>, usize), EvalError> {
        let derivation = Self::evaluate(operand)?;
        match derivation.conclusion.value.numeral_value() {
            Some(n) => Ok((derivation, n)),
            None => Err(EvalError::ExpectedNat {
                operator,
                found: derivation.conclusion.value,
            }),
        }
    }
}

impl BigStep for TypedArithBigStep {
    type Term = Term;
    type Value = Term;
    type Rule = EvalRule;
    type Error = EvalError;

    fn evaluate(term: &Term) -> Result<EvalDerivation<Self>, EvalError> {
        match term {
            Term::True | Term::False | Term::Zero => Ok(axiom(term)),

            Term::If { condition, then_branch, else_branch } => {
                let condition = Self::evaluate(condition)?;
                let (rule, branch) = match &condition.conclusion.value {
                    Term::True => (EvalRule::IfTrue, then_branch),
                    Term::False => (EvalRule::IfFalse, else_branch),
                    other => return Err(EvalError::InvalidCondition { found: other.clone() }),
                };
                let branch = Self::evaluate(branch)?;
                let value = branch.conclusion.value.clone();
                Ok(Derivation::node(Eval { term: term.clone(), value }, rule, vec![condition, branch]))
            }

            Term::Succ(t) => {
                let (inner, n) = Self::numeric(t, "succ")?;
                Ok(Derivation::node(
                    Eval { term: term.clone(), value: Term::numeral(n + 1) },
                    EvalRule::Succ,
                    vec![inner],
                ))
            }

            Term::Pred(t) => {
                let (inner, n) = Self::numeric(t, "pred")?;
                let (rule, value) = match n {
                    0 => (EvalRule::PredZero, Term::Zero),
                    _ => (EvalRule::PredSucc, Term::numeral(n - 1)),
                };
                Ok(Derivation::node(Eval { term: term.clone(), value }, rule, vec![inner]))
            }

            Term::IsZero(t) => {
                let (inner, n) = Self::numeric(t, "iszero")?;
                let (rule, value) = match n {
                    0 => (EvalRule::IsZeroZero, Term::True),
                    _ => (EvalRule::IsZeroSucc, Term::False),
                };
                Ok(Derivation::node(Eval { term: term.clone(), value }, rule, vec![inner]))
            }
        }
    }
}
