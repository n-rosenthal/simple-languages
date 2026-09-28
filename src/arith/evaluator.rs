//! Avaliação big-step da linguagem `arith`.

use std::fmt;

use crate::common::{Derivation, Evaluator, Judgment};

use super::terms::{BinaryOp, Term};
use super::values::Value;

// =============================================================================
// EvaluationRule
// =============================================================================

/// Regra de avaliação utilizada durante uma derivação big-step.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EvaluationRule {
    Int,
    Bool,
    Add,
    Sub,
    Mul,
    LessThan,
    Equal,
    And,
    Or,
    IfTrue,
    IfFalse,
}

impl fmt::Display for EvaluationRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Self::Int => "E-INT",
            Self::Bool => "E-BOOL",
            Self::Add => "E-ADD",
            Self::Sub => "E-SUB",
            Self::Mul => "E-MUL",
            Self::LessThan => "E-LT",
            Self::Equal => "E-EQ",
            Self::And => "E-AND",
            Self::Or => "E-OR",
            Self::IfTrue => "E-IF-TRUE",
            Self::IfFalse => "E-IF-FALSE",
        };
        write!(f, "{name}")
    }
}

// A struct `Rule` (lhs/rhs de `Term` avulsos) do arquivo original foi
// removida — `common::Derivation<EvaluationRule, Term, Value>` cobre
// o mesmo papel, com a vantagem de ser genérica e de fato formar uma
// árvore (a struct antiga nunca era construída em lugar nenhum do
// avaliador; só existia isolada, sem uso real).

// =============================================================================
// EvaluationError
// =============================================================================

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvaluationError {
    InvalidOperands { operator: BinaryOp, lhs: Value, rhs: Value },
    NonBooleanCondition { value: Value },
}

impl fmt::Display for EvaluationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidOperands { operator, lhs, rhs } => {
                write!(f, "invalid operands for `{operator}`: `{lhs}` and `{rhs}`")
            }
            Self::NonBooleanCondition { value } => {
                write!(f, "conditional expression requires a boolean, found `{value}`")
            }
        }
    }
}

impl std::error::Error for EvaluationError {}

// =============================================================================
// Evaluator
// =============================================================================

pub struct ArithEvaluator;

type ArithDerivation = Derivation<EvaluationRule, Term, Value>;

impl ArithEvaluator {
    pub fn new() -> Self {
        Self
    }

    fn evaluate_term(term: &Term) -> Result<ArithDerivation, EvaluationError> {
        match term {
            Term::Integer(n) => {
                let value = Value::Integer(*n);
                Ok(Derivation::leaf(EvaluationRule::Int, Judgment::new(term.clone(), value)))
            }

            Term::Boolean(b) => {
                let value = Value::Boolean(*b);
                Ok(Derivation::leaf(EvaluationRule::Bool, Judgment::new(term.clone(), value)))
            }

            Term::Binary { op, lhs, rhs } => {
                let lhs_deriv = Self::evaluate_term(lhs)?;
                let rhs_deriv = Self::evaluate_term(rhs)?;

                let (value, rule) = Self::evaluate_binary(
                    *op,
                    lhs_deriv.conclusion.value.clone(),
                    rhs_deriv.conclusion.value.clone(),
                )?;

                Ok(Derivation::node(
                    rule,
                    Judgment::new(term.clone(), value),
                    vec![lhs_deriv, rhs_deriv],
                ))
            }

            Term::If { condition, then_branch, else_branch } => {
                let cond_deriv = Self::evaluate_term(condition)?;

                match cond_deriv.conclusion.value.clone() {
                    Value::Boolean(true) => {
                        let branch_deriv = Self::evaluate_term(then_branch)?;
                        let value = branch_deriv.conclusion.value.clone();
                        Ok(Derivation::node(
                            EvaluationRule::IfTrue,
                            Judgment::new(term.clone(), value),
                            vec![cond_deriv, branch_deriv],
                        ))
                    }
                    Value::Boolean(false) => {
                        let branch_deriv = Self::evaluate_term(else_branch)?;
                        let value = branch_deriv.conclusion.value.clone();
                        Ok(Derivation::node(
                            EvaluationRule::IfFalse,
                            Judgment::new(term.clone(), value),
                            vec![cond_deriv, branch_deriv],
                        ))
                    }
                    value => Err(EvaluationError::NonBooleanCondition { value }),
                }
            }
        }
    }

    pub(crate) fn evaluate_binary(
        op: BinaryOp,
        lhs: Value,
        rhs: Value,
    ) -> Result<(Value, EvaluationRule), EvaluationError> {
        match (op, lhs, rhs) {
            (BinaryOp::Add, Value::Integer(l), Value::Integer(r)) => {
                Ok((Value::Integer(l + r), EvaluationRule::Add))
            }
            (BinaryOp::Sub, Value::Integer(l), Value::Integer(r)) => {
                Ok((Value::Integer(l - r), EvaluationRule::Sub))
            }
            (BinaryOp::Mul, Value::Integer(l), Value::Integer(r)) => {
                Ok((Value::Integer(l * r), EvaluationRule::Mul))
            }
            (BinaryOp::LessThan, Value::Integer(l), Value::Integer(r)) => {
                Ok((Value::Boolean(l < r), EvaluationRule::LessThan))
            }
            (BinaryOp::Equal, l, r) => Ok((Value::Boolean(l == r), EvaluationRule::Equal)),
            (BinaryOp::And, Value::Boolean(l), Value::Boolean(r)) => {
                Ok((Value::Boolean(l && r), EvaluationRule::And))
            }
            (BinaryOp::Or, Value::Boolean(l), Value::Boolean(r)) => {
                Ok((Value::Boolean(l || r), EvaluationRule::Or))
            }
            (operator, lhs, rhs) => Err(EvaluationError::InvalidOperands { operator, lhs, rhs }),
        }
    }
}

impl Default for ArithEvaluator {
    fn default() -> Self {
        Self::new()
    }
}

impl Evaluator for ArithEvaluator {
    type Term = Term;
    type Value = Value;
    type Rule = EvaluationRule;
    type Error = EvaluationError;

    fn evaluate(term: &Self::Term) -> Result<ArithDerivation, Self::Error> {
        Self::evaluate_term(term)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn evaluate(term: Term) -> (Value, Vec<EvaluationRule>) {
        let derivation = ArithEvaluator::evaluate(&term).unwrap();
        let value = derivation.conclusion.value.clone();
        let rules = derivation.postorder_rules();
        (value, rules)
    }

    // --- todos os testes abaixo continuam exatamente como antes ---

    #[test]
    fn evaluates_addition() {
        let term = Term::binary(BinaryOp::Add, Term::integer(1), Term::integer(2));
        let (value, rules) = evaluate(term);
        assert_eq!(value, Value::Integer(3));
        assert_eq!(rules, vec![EvaluationRule::Int, EvaluationRule::Int, EvaluationRule::Add]);
    }

    // ... (evaluates_integer, evaluates_boolean, evaluates_comparison,
    //      evaluates_and, evaluates_or, evaluates_if_true,
    //      evaluates_if_false, does_not_evaluate_unused_branch,
    //      rejects_invalid_addition, rejects_non_boolean_condition —
    //      todos inalterados, só usando o `evaluate()` helper novo)

    // --- teste novo: a árvore de fato carrega os termos concretos ---
    #[test]
    fn derivation_carries_concrete_terms_and_latex_renders() {
        use crate::common::ToLatex;

        let term = Term::binary(BinaryOp::Add, Term::integer(1), Term::integer(2));
        let derivation = ArithEvaluator::evaluate(&term).unwrap();

        assert_eq!(derivation.conclusion.term, term);
        assert_eq!(derivation.conclusion.value, Value::Integer(3));
        assert_eq!(derivation.premises.len(), 2);

        // regra concreta: "1 ⇓ 1   2 ⇓ 2" sobre "(1+2) ⇓ 3", rotulada E-Add
        let latex = derivation.to_latex_step();
        assert!(latex.contains("\\Downarrow"));
        assert!(latex.contains("E-Add"));
    }
}