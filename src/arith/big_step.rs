//! Semântica natural de `arith`: `t ⇓ v`.
//!
//! Aqui "nenhuma regra se aplica" é um erro (não há derivação), ao
//! contrário da semântica estrutural, onde o termo simplesmente trava.

use std::fmt;

use crate::common::semantics::{BigStep, Derivation, Eval, EvalDerivation};

use super::terms::{BinaryOp, Term};
use super::values::{apply, Value};

crate::rules! {
    pub enum EvalRule {
        Integer => "E-Int" { [] => r"n \Downarrow n" },
        Boolean => "E-Bool" { [] => r"b \Downarrow b" },
        Add => "E-Add" {
            [r"t_1 \Downarrow n_1", r"t_2 \Downarrow n_2"] => r"t_1 + t_2 \Downarrow n_1 + n_2"
        },
        Sub => "E-Sub" {
            [r"t_1 \Downarrow n_1", r"t_2 \Downarrow n_2"] => r"t_1 - t_2 \Downarrow n_1 - n_2"
        },
        Mul => "E-Mul" {
            [r"t_1 \Downarrow n_1", r"t_2 \Downarrow n_2"] => r"t_1 \times t_2 \Downarrow n_1 \times n_2"
        },
        LessThan => "E-Lt" {
            [r"t_1 \Downarrow n_1", r"t_2 \Downarrow n_2"] => r"t_1 < t_2 \Downarrow (n_1 < n_2)"
        },
        Equal => "E-Eq" {
            [r"t_1 \Downarrow v_1", r"t_2 \Downarrow v_2"] => r"t_1 = t_2 \Downarrow (v_1 = v_2)"
        },
        And => "E-And" {
            [r"t_1 \Downarrow b_1", r"t_2 \Downarrow b_2"] => r"t_1 \wedge t_2 \Downarrow b_1 \wedge b_2"
        },
        Or => "E-Or" {
            [r"t_1 \Downarrow b_1", r"t_2 \Downarrow b_2"] => r"t_1 \vee t_2 \Downarrow b_1 \vee b_2"
        },
        IfTrue => "E-IfTrue" {
            [r"t_1 \Downarrow \mathsf{true}", r"t_2 \Downarrow v"]
                => r"\mathsf{if}\ t_1\ \mathsf{then}\ t_2\ \mathsf{else}\ t_3 \Downarrow v"
        },
        IfFalse => "E-IfFalse" {
            [r"t_1 \Downarrow \mathsf{false}", r"t_3 \Downarrow v"]
                => r"\mathsf{if}\ t_1\ \mathsf{then}\ t_2\ \mathsf{else}\ t_3 \Downarrow v"
        },
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalError {
    /// Operandos de tipo errado para o operador.
    InvalidOperands { op: BinaryOp, lhs: Value, rhs: Value },
    /// A condição de um `if` não avaliou para um booleano.
    InvalidCondition { found: Value },
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidOperands { op, lhs, rhs } => write!(
                f,
                "operator `{op}` cannot be applied to `{lhs}` and `{rhs}`"
            ),
            Self::InvalidCondition { found } => {
                write!(f, "if condition must be a Boolean, found `{found}`")
            }
        }
    }
}

impl std::error::Error for EvalError {}

fn rule_of(op: BinaryOp) -> EvalRule {
    match op {
        BinaryOp::Add => EvalRule::Add,
        BinaryOp::Sub => EvalRule::Sub,
        BinaryOp::Mul => EvalRule::Mul,
        BinaryOp::LessThan => EvalRule::LessThan,
        BinaryOp::Equal => EvalRule::Equal,
        BinaryOp::And => EvalRule::And,
        BinaryOp::Or => EvalRule::Or,
    }
}

pub struct ArithBigStep;

impl BigStep for ArithBigStep {
    type Term = Term;
    type Value = Value;
    type Rule = EvalRule;
    type Error = EvalError;

    fn evaluate(term: &Term) -> Result<EvalDerivation<Self>, EvalError> {
        match term {
            Term::Integer(n) => Ok(Derivation::axiom(
                Eval { term: term.clone(), value: Value::Integer(*n) },
                EvalRule::Integer,
            )),

            Term::Boolean(b) => Ok(Derivation::axiom(
                Eval { term: term.clone(), value: Value::Boolean(*b) },
                EvalRule::Boolean,
            )),

            Term::Binary { op, lhs, rhs } => {
                let left = Self::evaluate(lhs)?;
                let right = Self::evaluate(rhs)?;
                let (l, r) = (left.conclusion.value, right.conclusion.value);

                let value = apply(*op, l, r)
                    .ok_or(EvalError::InvalidOperands { op: *op, lhs: l, rhs: r })?;

                Ok(Derivation::node(
                    Eval { term: term.clone(), value },
                    rule_of(*op),
                    vec![left, right],
                ))
            }

            Term::If { condition, then_branch, else_branch } => {
                let condition = Self::evaluate(condition)?;

                let (rule, branch) = match condition.conclusion.value {
                    Value::Boolean(true) => (EvalRule::IfTrue, then_branch),
                    Value::Boolean(false) => (EvalRule::IfFalse, else_branch),
                    found => return Err(EvalError::InvalidCondition { found }),
                };

                let branch = Self::evaluate(branch)?;
                let value = branch.conclusion.value;

                Ok(Derivation::node(
                    Eval { term: term.clone(), value },
                    rule,
                    vec![condition, branch],
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

    #[test]
    fn evaluates_the_book_example() {
        // (2 * 3) + (4 - 5) ⇓ 5
        let term = Term::binary(
            BinaryOp::Add,
            Term::binary(BinaryOp::Mul, int(2), int(3)),
            Term::binary(BinaryOp::Sub, int(4), int(5)),
        );
        let d = ArithBigStep::evaluate(&term).unwrap();

        assert_eq!(d.conclusion.value, Value::Integer(5));
        assert_eq!(
            d.postorder_rules(),
            vec![
                EvalRule::Integer,
                EvalRule::Integer,
                EvalRule::Mul,
                EvalRule::Integer,
                EvalRule::Integer,
                EvalRule::Sub,
                EvalRule::Add,
            ]
        );
    }

    #[test]
    fn only_the_chosen_branch_is_evaluated() {
        // o ramo `else` está mal formado, mas nunca é avaliado
        let term = Term::if_then_else(
            Term::boolean(true),
            int(1),
            Term::binary(BinaryOp::Add, Term::boolean(true), int(1)),
        );
        let d = ArithBigStep::evaluate(&term).unwrap();

        assert_eq!(d.conclusion.value, Value::Integer(1));
        assert_eq!(d.rule, EvalRule::IfTrue);
        assert_eq!(d.premises.len(), 2);
    }

    #[test]
    fn invalid_operands_have_no_derivation() {
        let term = Term::binary(BinaryOp::Add, Term::boolean(true), int(1));
        assert_eq!(
            ArithBigStep::evaluate(&term).unwrap_err(),
            EvalError::InvalidOperands {
                op: BinaryOp::Add,
                lhs: Value::Boolean(true),
                rhs: Value::Integer(1),
            }
        );
    }

    #[test]
    fn invalid_condition_has_no_derivation() {
        let term = Term::if_then_else(int(1), int(2), int(3));
        assert_eq!(
            ArithBigStep::evaluate(&term).unwrap_err(),
            EvalError::InvalidCondition { found: Value::Integer(1) }
        );
    }

    #[test]
    fn overflow_wraps() {
        let term = Term::binary(BinaryOp::Add, int(i64::MAX), int(1));
        assert_eq!(
            ArithBigStep::value_of(&term).unwrap(),
            Value::Integer(i64::MIN)
        );
    }

    #[test]
    fn text_rendering() {
        let term = Term::binary(BinaryOp::Add, int(1), int(2));
        let d = ArithBigStep::evaluate(&term).unwrap();

        assert_eq!(
            d.to_text(),
            "(1 + 2) ⇓ 3  [E-Add]\n  1 ⇓ 1  [E-Int]\n  2 ⇓ 2  [E-Int]\n"
        );
    }
}
