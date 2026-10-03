//! Tipagem de `arith`: o julgamento `⊢ t : T` (termos fechados, sem Γ).

use std::fmt;

use crate::common::semantics::{Derivation, Typed, Typing, TypingDerivation};

use super::terms::{BinaryOp, Term};
use super::types::Type;

crate::rules! {
    pub enum TypingRule {
        Integer => "T-Int",
        Boolean => "T-Bool",
        Add => "T-Add",
        Sub => "T-Sub",
        Mul => "T-Mul",
        LessThan => "T-Lt",
        Equal => "T-Eq",
        And => "T-And",
        Or => "T-Or",
        If => "T-If",
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    /// Operandos incompatíveis com o operador.
    InvalidBinaryOperands { op: BinaryOp, lhs: Type, rhs: Type },
    /// A condição de um `if` não é booleana.
    InvalidCondition { found: Type },
    /// Os ramos de um `if` têm tipos diferentes.
    BranchTypeMismatch { then_type: Type, else_type: Type },
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidBinaryOperands { op, lhs, rhs } => write!(
                f,
                "operator `{op}` cannot be applied to `{lhs}` and `{rhs}`"
            ),
            Self::InvalidCondition { found } => {
                write!(f, "if condition must have type Boolean, found `{found}`")
            }
            Self::BranchTypeMismatch { then_type, else_type } => write!(
                f,
                "if branches must have the same type, found `{then_type}` and `{else_type}`"
            ),
        }
    }
}

impl std::error::Error for TypeError {}

pub struct ArithTyping;

impl ArithTyping {
    /// A regra de tipagem de um operador, dados os tipos dos operandos.
    fn check_binary(op: BinaryOp, lhs: Type, rhs: Type) -> Result<(Type, TypingRule), TypeError> {
        use Type::{Boolean, Integer};

        match (op, lhs, rhs) {
            (BinaryOp::Add, Integer, Integer) => Ok((Integer, TypingRule::Add)),
            (BinaryOp::Sub, Integer, Integer) => Ok((Integer, TypingRule::Sub)),
            (BinaryOp::Mul, Integer, Integer) => Ok((Integer, TypingRule::Mul)),
            (BinaryOp::LessThan, Integer, Integer) => Ok((Boolean, TypingRule::LessThan)),
            (BinaryOp::Equal, l, r) if l == r => Ok((Boolean, TypingRule::Equal)),
            (BinaryOp::And, Boolean, Boolean) => Ok((Boolean, TypingRule::And)),
            (BinaryOp::Or, Boolean, Boolean) => Ok((Boolean, TypingRule::Or)),
            _ => Err(TypeError::InvalidBinaryOperands { op, lhs, rhs }),
        }
    }
}

impl Typing for ArithTyping {
    type Term = Term;
    type Type = Type;
    type Rule = TypingRule;
    type Error = TypeError;

    fn check(term: &Term) -> Result<TypingDerivation<Self>, TypeError> {
        match term {
            Term::Integer(_) => Ok(Derivation::axiom(
                Typed::closed(term.clone(), Type::Integer),
                TypingRule::Integer,
            )),

            Term::Boolean(_) => Ok(Derivation::axiom(
                Typed::closed(term.clone(), Type::Boolean),
                TypingRule::Boolean,
            )),

            Term::Binary { op, lhs, rhs } => {
                let left = Self::check(lhs)?;
                let right = Self::check(rhs)?;

                let (ty, rule) =
                    Self::check_binary(*op, left.conclusion.ty, right.conclusion.ty)?;

                Ok(Derivation::node(
                    Typed::closed(term.clone(), ty),
                    rule,
                    vec![left, right],
                ))
            }

            Term::If { condition, then_branch, else_branch } => {
                let condition = Self::check(condition)?;
                if condition.conclusion.ty != Type::Boolean {
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
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn int(n: i64) -> Term {
        Term::integer(n)
    }

    fn check(term: &Term) -> Result<(Type, Vec<TypingRule>), TypeError> {
        ArithTyping::check(term).map(|d| (d.conclusion.ty, d.postorder_rules()))
    }

    #[test]
    fn literals() {
        assert_eq!(check(&int(42)), Ok((Type::Integer, vec![TypingRule::Integer])));
        assert_eq!(
            check(&Term::boolean(true)),
            Ok((Type::Boolean, vec![TypingRule::Boolean]))
        );
    }

    #[test]
    fn addition_lists_rules_in_postorder() {
        let term = Term::binary(BinaryOp::Add, int(1), int(2));
        assert_eq!(
            check(&term),
            Ok((
                Type::Integer,
                vec![TypingRule::Integer, TypingRule::Integer, TypingRule::Add]
            ))
        );
    }

    #[test]
    fn comparison_yields_boolean() {
        let term = Term::binary(BinaryOp::LessThan, int(1), int(2));
        assert_eq!(check(&term).unwrap().0, Type::Boolean);
    }

    #[test]
    fn equality_accepts_matching_types_only() {
        let ints = Term::binary(BinaryOp::Equal, int(1), int(2));
        let bools = Term::binary(BinaryOp::Equal, Term::boolean(true), Term::boolean(false));
        let mixed = Term::binary(BinaryOp::Equal, int(1), Term::boolean(true));

        assert_eq!(check(&ints).unwrap().0, Type::Boolean);
        assert_eq!(check(&bools).unwrap().0, Type::Boolean);
        assert!(check(&mixed).is_err());
    }

    #[test]
    fn invalid_addition() {
        let term = Term::binary(BinaryOp::Add, int(1), Term::boolean(true));
        assert_eq!(
            check(&term),
            Err(TypeError::InvalidBinaryOperands {
                op: BinaryOp::Add,
                lhs: Type::Integer,
                rhs: Type::Boolean,
            })
        );
    }

    #[test]
    fn conditional() {
        let term = Term::if_then_else(
            Term::binary(BinaryOp::LessThan, int(1), int(2)),
            int(10),
            int(20),
        );
        assert_eq!(
            check(&term),
            Ok((
                Type::Integer,
                vec![
                    TypingRule::Integer,
                    TypingRule::Integer,
                    TypingRule::LessThan,
                    TypingRule::Integer,
                    TypingRule::Integer,
                    TypingRule::If,
                ]
            ))
        );
    }

    #[test]
    fn invalid_condition() {
        let term = Term::if_then_else(int(1), int(10), int(20));
        assert_eq!(
            check(&term),
            Err(TypeError::InvalidCondition { found: Type::Integer })
        );
    }

    #[test]
    fn mismatched_branches() {
        let term = Term::if_then_else(Term::boolean(true), int(10), Term::boolean(false));
        assert_eq!(
            check(&term),
            Err(TypeError::BranchTypeMismatch {
                then_type: Type::Integer,
                else_type: Type::Boolean,
            })
        );
    }

    #[test]
    fn the_derivation_is_a_tree() {
        let term = Term::if_then_else(Term::boolean(true), int(1), int(2));
        let d = ArithTyping::check(&term).unwrap();

        assert_eq!(d.rule, TypingRule::If);
        assert_eq!(d.premises.len(), 3);
        assert_eq!(d.size(), 4);
        assert_eq!(d.conclusion.to_string(), "⊢ if true then 1 else 2 : Integer");
    }
}
