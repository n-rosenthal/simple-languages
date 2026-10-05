//! Tipagem de `arith-extensions`: o julgamento `⊢ t : T` (termos fechados, sem Γ).

use std::fmt;

use crate::common::semantics::{Derivation, Typed, Typing, TypingDerivation};

use super::terms::{BinaryOp, Term};
use super::types::Type;

pub type Environment = crate::common::context::Context<Type>;

crate::rules! {
    pub enum TypingRule {
        Integer => "T-Int" { [] => r"\vdash n : \mathsf{Integer}" },
        Boolean => "T-Bool" { [] => r"\vdash b : \mathsf{Boolean}" },
        Arrow => "T-Arrow" { [r"\vdash t_1 : T_1", r"\vdash t_2 : T_2"] => r"\vdash t_1 \to t_2 : \mathsf{Arrow}(T_1, T_2)" },

        //  Naturals with Peano arithmetic
        Natural => "T-Nat" { [] => r"\vdash 0 : \mathsf{Natural}" },
        Succ => "T-Succ" { [r"\vdash t : \mathsf{Natural}"] => r"\vdash S(t) : \mathsf{Natural}" },
        Pred => "T-Pred" { [r"\vdash t : \mathsf{Natural}"] => r"\vdash P(t) : \mathsf{Natural}" },
        IsZero => "T-IsZero" { [r"\vdash t : \mathsf{Natural}"] => r"\vdash \mathsf{IsZero}(t) : \mathsf{Boolean}" },


        Add => "T-Add" {
            [r"\vdash t_1 : \mathsf{Integer}", r"\vdash t_2 : \mathsf{Integer}"]
                => r"\vdash t_1 + t_2 : \mathsf{Integer}"
        },
        Sub => "T-Sub" {
            [r"\vdash t_1 : \mathsf{Integer}", r"\vdash t_2 : \mathsf{Integer}"]
                => r"\vdash t_1 - t_2 : \mathsf{Integer}"
        },
        Mul => "T-Mul" {
            [r"\vdash t_1 : \mathsf{Integer}", r"\vdash t_2 : \mathsf{Integer}"]
                => r"\vdash t_1 \times t_2 : \mathsf{Integer}"
        },

        Div => "T-Div" {
            [r"\vdash t_1 : \mathsf{Integer}", r"\vdash t_2 : \mathsf{Integer}"]
                => r"\vdash t_1 / t_2 : \mathsf{Integer}"
        },
        Mod => "T-Mod" {
            [r"\vdash t_1 : \mathsf{Integer}", r"\vdash t_2 : \mathsf{Integer}"]
                => r"\vdash t_1 \bmod t_2 : \mathsf{Integer}"
        },

        LessThan => "T-Lt" {
            [r"\vdash t_1 : \mathsf{Integer}", r"\vdash t_2 : \mathsf{Integer}"]
                => r"\vdash t_1 < t_2 : \mathsf{Boolean}"
        },
        Equal => "T-Eq" {
            [r"\vdash t_1 : T", r"\vdash t_2 : T"] => r"\vdash t_1 = t_2 : \mathsf{Boolean}"
        },
        And => "T-And" {
            [r"\vdash t_1 : \mathsf{Boolean}", r"\vdash t_2 : \mathsf{Boolean}"]
                => r"\vdash t_1 \wedge t_2 : \mathsf{Boolean}"
        },
        Or => "T-Or" {
            [r"\vdash t_1 : \mathsf{Boolean}", r"\vdash t_2 : \mathsf{Boolean}"]
                => r"\vdash t_1 \vee t_2 : \mathsf{Boolean}"
        },
        If => "T-If" {
            [r"\vdash t_1 : \mathsf{Boolean}", r"\vdash t_2 : T", r"\vdash t_3 : T"]
                => r"\vdash \mathsf{if}\ t_1\ \mathsf{then}\ t_2\ \mathsf{else}\ t_3 : T"
        },

        // extensions
        // error types
        Exception => "T-Excep" { [] => r"\vdash \mathsf{error} : T" },
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

    // extensions
    /// O sucessor de um termo não natural.
    InvalidSuccessor { found: Type },

    /// O antecessor de um termo não natural.
    InvalidPredecessor { found: Type },

    /// O teste de zero de um termo não natural.
    InvalidIsZero { found: Type },

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

            Self::InvalidSuccessor { found } => {
                write!(f, "successor must have type Natural, found `{found}`")
            }

            Self::InvalidPredecessor { found } => {
                write!(f, "predecessor must have type Natural, found `{found}`")
            }

            Self::InvalidIsZero { found } => {
                write!(f, "iszero must have type Natural, found `{found}`")
            }
        }
    }
}

impl std::error::Error for TypeError {}

pub struct ArithTyping;

impl ArithTyping {
    /// A regra de tipagem de um operador, dados os tipos dos operandos.
    fn check_binary(
        op: BinaryOp,
        lhs: Type,
        rhs: Type,
    ) -> Result<(Type, TypingRule), TypeError> {
        use Type::{Boolean, Integer, Natural};

        match (op, &lhs, &rhs) {
            (BinaryOp::Add, Integer, Integer) => {
                Ok((Integer, TypingRule::Add))
            }

            (BinaryOp::Sub, Integer, Integer) => {
                Ok((Integer, TypingRule::Sub))
            }

            (BinaryOp::Mul, Integer, Integer) => {
                Ok((Integer, TypingRule::Mul))
            }

            (BinaryOp::LessThan, Integer, Integer) => {
                Ok((Boolean, TypingRule::LessThan))
            }

            (BinaryOp::Equal, l, r) if l == r => {
                Ok((Boolean, TypingRule::Equal))
            }

            (BinaryOp::And, Boolean, Boolean) => {
                Ok((Boolean, TypingRule::And))
            }

            (BinaryOp::Or, Boolean, Boolean) => {
                Ok((Boolean, TypingRule::Or))
            }

            // Extensions: integers
            (BinaryOp::Div, Integer, Integer) => {
                Ok((Integer, TypingRule::Div))
            }

            (BinaryOp::Mod, Integer, Integer) => {
                Ok((Integer, TypingRule::Mod))
            }

            // Extensions: naturals
            (BinaryOp::Add, Natural, Natural) => {
                Ok((Natural, TypingRule::Add))
            }

            (BinaryOp::Sub, Natural, Natural) => {
                Ok((Natural, TypingRule::Sub))
            }

            (BinaryOp::Mul, Natural, Natural) => {
                Ok((Natural, TypingRule::Mul))
            }

            (BinaryOp::LessThan, Natural, Natural) => {
                Ok((Boolean, TypingRule::LessThan))
            }

            (BinaryOp::Equal, Natural, Natural) => {
                Ok((Boolean, TypingRule::Equal))
            }

            // Extensions: natural division
            (BinaryOp::Div, Natural, Natural) => {
                Ok((Natural, TypingRule::Div))
            }

            // Extensions: natural remainder
            (BinaryOp::Mod, Natural, Natural) => {
                Ok((Natural, TypingRule::Mod))
            }

            _ => Err(TypeError::InvalidBinaryOperands { op, lhs, rhs }.into()),
        }
    }
}

impl Typing for ArithTyping {
    type Term = Term;
    type Type = Type;
    type Rule = TypingRule;
    type Error = TypeError;

    fn check(term: &Self::Term) -> Result<TypingDerivation<Self>, Self::Error> {
        let ctx = crate::common::Context::<Self::Type>::empty();
        Self::check_in(&ctx, term)
    }

    fn check_in(
        ctx: &crate::common::Context<Self::Type>,
        term: &Self::Term,
    ) -> Result<TypingDerivation<Self>, Self::Error> {
        match term {
            Term::Integer(_) => Ok(Derivation::axiom(
                Typed::closed(term.clone(), Type::Integer),
                TypingRule::Integer,
            )),

            Term::Boolean(_) => Ok(Derivation::axiom(
                Typed::closed(term.clone(), Type::Boolean),
                TypingRule::Boolean,
            )),

            Term::Zero => Ok(Derivation::axiom(
                Typed::closed(term.clone(), Type::Natural),
                TypingRule::Natural,
            )),

            Term::Succ(n) => {
                let n = Self::check_in(ctx, n)?;

                if n.conclusion.ty != Type::Natural {
                    return Err(TypeError::InvalidSuccessor {
                        found: n.conclusion.ty.clone(),
                    });
                }

                Ok(Derivation::node(
                    Typed::closed(term.clone(), Type::Natural),
                    TypingRule::Succ,
                    vec![n],
                ))
            }

            Term::Pred(n) => {
                let n = Self::check_in(ctx, n)?;

                if n.conclusion.ty != Type::Natural {
                    return Err(TypeError::InvalidPredecessor {
                        found: n.conclusion.ty.clone(),
                    });
                }

                Ok(Derivation::node(
                    Typed::closed(term.clone(), Type::Natural),
                    TypingRule::Pred,
                    vec![n],
                ))
            }

            Term::IsZero(n) => {
                let n = Self::check_in(ctx, n)?;

                if n.conclusion.ty != Type::Natural {
                    return Err(TypeError::InvalidIsZero {
                        found: n.conclusion.ty.clone(),
                    });
                }

                Ok(Derivation::node(
                    Typed::closed(term.clone(), Type::Boolean),
                    TypingRule::IsZero,
                    vec![n],
                ))
            }

            Term::Binary { op, lhs, rhs } => {
                let left = Self::check_in(ctx, lhs)?;
                let right = Self::check_in(ctx, rhs)?;

                let (ty, rule) = Self::check_binary(
                    *op,
                    left.conclusion.ty.clone(),
                    right.conclusion.ty.clone(),
                )?;

                Ok(Derivation::node(
                    Typed::closed(term.clone(), ty),
                    rule,
                    vec![left, right],
                ))
            }

            Term::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = Self::check_in(ctx, condition)?;

                if condition.conclusion.ty != Type::Boolean {
                    return Err(TypeError::InvalidCondition {
                        found: condition.conclusion.ty.clone(),
                    });
                }

                let then_branch = Self::check_in(ctx, then_branch)?;
                let else_branch = Self::check_in(ctx, else_branch)?;

                if then_branch.conclusion.ty != else_branch.conclusion.ty {
                    return Err(TypeError::BranchTypeMismatch {
                        then_type: then_branch.conclusion.ty.clone(),
                        else_type: else_branch.conclusion.ty.clone(),
                    });
                }

                let ty = then_branch.conclusion.ty.clone();

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
        ArithTyping::check(term).map(|d| {
            let ty = d.conclusion.ty.clone();
            let rules = d.postorder_rules();
            (ty, rules)
        })
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
