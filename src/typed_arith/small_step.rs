//! Semântica estrutural de `typed-arith` (TAPL, fig. 3-2).
//!
//! ```text
//!   if true then t2 else t3 → t2            (E-IfTrue)
//!   if false then t2 else t3 → t3           (E-IfFalse)
//!   t1 → t1'  ⟹  if t1 then t2 else t3 → if t1' then t2 else t3   (E-If)
//!   t1 → t1'  ⟹  succ t1 → succ t1'         (E-Succ)
//!   pred 0 → 0                               (E-PredZero)
//!   pred (succ nv1) → nv1                    (E-PredSucc)
//!   t1 → t1'  ⟹  pred t1 → pred t1'         (E-Pred)
//!   iszero 0 → true                          (E-IszeroZero)
//!   iszero (succ nv1) → false                (E-IszeroSucc)
//!   t1 → t1'  ⟹  iszero t1 → iszero t1'     (E-IsZero)
//! ```
//!
//! Os valores são `true`, `false` e os valores numéricos. Um termo como
//! `succ true` é uma forma normal que não é valor: está travado.

use crate::common::semantics::{Step, Transition};

use super::terms::Term;

crate::rules! {
    pub enum StepRule {
        IfTrue => "E-IfTrue" {
            [] => r"\mathsf{if}\ \mathsf{true}\ \mathsf{then}\ t_2\ \mathsf{else}\ t_3 \to t_2"
        },
        IfFalse => "E-IfFalse" {
            [] => r"\mathsf{if}\ \mathsf{false}\ \mathsf{then}\ t_2\ \mathsf{else}\ t_3 \to t_3"
        },
        If => "E-If" {
            [r"t_1 \to t_1'"]
                => r"\mathsf{if}\ t_1\ \mathsf{then}\ t_2\ \mathsf{else}\ t_3 \to \mathsf{if}\ t_1'\ \mathsf{then}\ t_2\ \mathsf{else}\ t_3"
        },
        Succ => "E-Succ" { [r"t_1 \to t_1'"] => r"\mathsf{succ}\ t_1 \to \mathsf{succ}\ t_1'" },
        PredZero => "E-PredZero" { [] => r"\mathsf{pred}\ 0 \to 0" },
        PredSucc => "E-PredSucc" {
            [] => r"\mathsf{pred}\ (\mathsf{succ}\ nv_1) \to nv_1"
        },
        Pred => "E-Pred" { [r"t_1 \to t_1'"] => r"\mathsf{pred}\ t_1 \to \mathsf{pred}\ t_1'" },
        IsZeroZero => "E-IszeroZero" { [] => r"\mathsf{iszero}\ 0 \to \mathsf{true}" },
        IsZeroSucc => "E-IszeroSucc" {
            [] => r"\mathsf{iszero}\ (\mathsf{succ}\ nv_1) \to \mathsf{false}"
        },
        IsZero => "E-IsZero" {
            [r"t_1 \to t_1'"] => r"\mathsf{iszero}\ t_1 \to \mathsf{iszero}\ t_1'"
        },
    }
}

pub struct TypedArithSmallStep;

impl TypedArithSmallStep {
    /// A regra mais externa e o termo seguinte (sem montar a `Transition`
    /// a cada nível da congruência).
    fn reduce(term: &Term) -> Option<(StepRule, Term)> {
        match term {
            Term::True | Term::False | Term::Zero => None,

            Term::If { condition, then_branch, else_branch } => match **condition {
                Term::True => Some((StepRule::IfTrue, (**then_branch).clone())),
                Term::False => Some((StepRule::IfFalse, (**else_branch).clone())),
                _ => {
                    let (_, inner) = Self::reduce(condition)?;
                    Some((
                        StepRule::If,
                        Term::if_then_else(inner, (**then_branch).clone(), (**else_branch).clone()),
                    ))
                }
            },

            // E-Succ
            Term::Succ(t) => {
                let (_, inner) = Self::reduce(t)?;
                Some((StepRule::Succ, Term::succ(inner)))
            }

            Term::Pred(t) => match &**t {
                Term::Zero => Some((StepRule::PredZero, Term::Zero)),
                Term::Succ(nv) if nv.is_numeric_value() => Some((StepRule::PredSucc, (**nv).clone())),
                _ => {
                    let (_, inner) = Self::reduce(t)?;
                    Some((StepRule::Pred, Term::pred(inner)))
                }
            },

            Term::IsZero(t) => match &**t {
                Term::Zero => Some((StepRule::IsZeroZero, Term::True)),
                Term::Succ(nv) if nv.is_numeric_value() => Some((StepRule::IsZeroSucc, Term::False)),
                _ => {
                    let (_, inner) = Self::reduce(t)?;
                    Some((StepRule::IsZero, Term::is_zero(inner)))
                }
            },
        }
    }
}

impl Step for TypedArithSmallStep {
    type State = Term;
    type Rule = StepRule;

    fn is_final(term: &Term) -> bool {
        term.is_value()
    }

    fn step(term: &Term) -> Option<Transition<StepRule, Term>> {
        let (rule, next) = Self::reduce(term)?;
        Some(Transition::new(rule, term.clone(), next))
    }
}
