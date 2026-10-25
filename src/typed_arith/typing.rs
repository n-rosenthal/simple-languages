//! Tipagem de `typed-arith` (TAPL, fig. 8-2): `⊢ t : T`.

use std::fmt;

use crate::common::semantics::{Derivation, Typed, Typing, TypingDerivation};

use super::terms::Term;
use super::types::Type;

crate::rules! {
    pub enum TypingRule {
        True => "T-True" { [] => r"\vdash \mathsf{true} : \mathsf{Bool}" },
        False => "T-False" { [] => r"\vdash \mathsf{false} : \mathsf{Bool}" },
        If => "T-If" {
            [r"\vdash t_1 : \mathsf{Bool}", r"\vdash t_2 : T", r"\vdash t_3 : T"]
                => r"\vdash \mathsf{if}\ t_1\ \mathsf{then}\ t_2\ \mathsf{else}\ t_3 : T"
        },
        Zero => "T-Zero" { [] => r"\vdash 0 : \mathsf{Nat}" },
        Succ => "T-Succ" {
            [r"\vdash t_1 : \mathsf{Nat}"] => r"\vdash \mathsf{succ}\ t_1 : \mathsf{Nat}"
        },
        Pred => "T-Pred" {
            [r"\vdash t_1 : \mathsf{Nat}"] => r"\vdash \mathsf{pred}\ t_1 : \mathsf{Nat}"
        },
        IsZero => "T-IsZero" {
            [r"\vdash t_1 : \mathsf{Nat}"] => r"\vdash \mathsf{iszero}\ t_1 : \mathsf{Bool}"
        },
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    /// A condição de um `if` não é `Bool`.
    InvalidCondition { found: Type },
    /// Os ramos de um `if` têm tipos diferentes.
    BranchTypeMismatch { then_type: Type, else_type: Type },
    /// O argumento de `succ`, `pred` ou `iszero` não é `Nat`.
    ExpectedNat { operator: &'static str, found: Type },
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidCondition { found } => {
                write!(f, "if condition must have type Bool, found `{found}`")
            }
            Self::BranchTypeMismatch { then_type, else_type } => write!(
                f,
                "if branches must have the same type, found `{then_type}` and `{else_type}`"
            ),
            Self::ExpectedNat { operator, found } => {
                write!(f, "`{operator}` expects a Nat, found `{found}`")
            }
        }
    }
}

impl std::error::Error for TypeError {}

pub struct TypedArithTyping;

impl TypedArithTyping {
    fn unary(
        term: &Term,
        operand: &Term,
        operator: &'static str,
        rule: TypingRule,
        result: Type,
    ) -> Result<TypingDerivation<Self>, TypeError> {
        let inner = Self::check(operand)?;
        if inner.conclusion.ty != Type::Nat {
            return Err(TypeError::ExpectedNat { operator, found: inner.conclusion.ty });
        }
        Ok(Derivation::node(Typed::closed(term.clone(), result), rule, vec![inner]))
    }
}

impl Typing for TypedArithTyping {
    type Term = Term;
    type Type = Type;
    type Rule = TypingRule;
    type Error = TypeError;

    fn check(term: &Term) -> Result<TypingDerivation<Self>, TypeError> {
        match term {
            Term::True => Ok(Derivation::axiom(Typed::closed(term.clone(), Type::Bool), TypingRule::True)),
            Term::False => Ok(Derivation::axiom(Typed::closed(term.clone(), Type::Bool), TypingRule::False)),
            Term::Zero => Ok(Derivation::axiom(Typed::closed(term.clone(), Type::Nat), TypingRule::Zero)),
            Term::If { condition, then_branch, else_branch } => {
                let condition = Self::check(condition)?;
                if condition.conclusion.ty != Type::Bool {
                    return Err(TypeError::InvalidCondition { found: condition.conclusion.ty });
                }
                let then_branch = Self::check(then_branch)?;
                let else_branch = Self::check(else_branch)?;
                if then_branch.conclusion.ty != else_branch.conclusion.ty {
                    return Err(TypeError::BranchTypeMismatch {
                        then_type: then_branch.conclusion.ty,
                        else_type: else_branch.conclusion.ty,
                    });
                }
                let ty = then_branch.conclusion.ty;
                Ok(Derivation::node(
                    Typed::closed(term.clone(), ty),
                    TypingRule::If,
                    vec![condition, then_branch, else_branch],
                ))
            }
            Term::Succ(t) => Self::unary(term, t, "succ", TypingRule::Succ, Type::Nat),
            Term::Pred(t) => Self::unary(term, t, "pred", TypingRule::Pred, Type::Nat),
            Term::IsZero(t) => Self::unary(term, t, "iszero", TypingRule::IsZero, Type::Bool),
        }
    }
}
