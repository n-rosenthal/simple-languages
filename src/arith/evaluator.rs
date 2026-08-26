//! Avaliação big-step da linguagem `arith`.

use std::fmt;

use crate::common::{
    Evaluator,
};

use super::terms::{
    BinaryOp,
    Term,
};

use super::values::Value;


// =============================================================================
// EvaluationRule e Rule
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
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
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

/// Representa uma regra de avaliação big-step com seus termos.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rule {
    pub rule: EvaluationRule,
    pub lhs:  Vec<Term>,   // premissas
    pub rhs:  Vec<Term>,   // conclusão
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self.rule {
            EvaluationRule::Int      => "E-INT",
            EvaluationRule::Bool     => "E-BOOL",
            EvaluationRule::Add      => "E-ADD",
            EvaluationRule::Sub      => "E-SUB",
            EvaluationRule::Mul      => "E-MUL",
            EvaluationRule::LessThan => "E-LT",
            EvaluationRule::Equal    => "E-EQ",
            EvaluationRule::And      => "E-AND",
            EvaluationRule::Or       => "E-OR",
            EvaluationRule::IfTrue   => "E-IF-TRUE",
            EvaluationRule::IfFalse  => "E-IF-FALSE",
        };

        // Converte as premissas para strings usando o Display de Term
        let premises: Vec<String> = self.lhs.iter().map(|t| format!("{}", t)).collect();
        let premises_str = if premises.is_empty() {
            String::new()
        } else {
            premises.join(" ∧ ")
        };

        // Converte a conclusão (usamos o primeiro elemento se houver; se vazio, exibe "()")
        let conclusion = if self.rhs.is_empty() {
            "()".to_string()
        } else {
            self.rhs.iter().map(|t| format!("{}", t)).collect::<Vec<_>>().join(", ")
        };

        if premises.is_empty() {
            write!(f, "[{}]  {}", name, conclusion)
        } else {
            write!(f, "[{}]  {} ⊢ {}", name, premises_str, conclusion)
        }
    }
}

// =============================================================================
// EvaluationError
// =============================================================================

/// Erros da avaliação big-step.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvaluationError {
    /// Um operador recebeu operandos de tipos inadequados.
    InvalidOperands {
        operator: BinaryOp,
        lhs: Value,
        rhs: Value,
    },

    /// A condição de um `if` não é booleana.
    NonBooleanCondition {
        value: Value,
    },
}

impl fmt::Display for EvaluationError {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        match self {
            Self::InvalidOperands {
                operator,
                lhs,
                rhs,
            } => {
                write!(
                    f,
                    "invalid operands for `{operator}`: \
                     `{lhs}` and `{rhs}`"
                )
            }

            Self::NonBooleanCondition { value } => {
                write!(
                    f,
                    "conditional expression requires \
                     a boolean, found `{value}`"
                )
            }
        }
    }
}

impl std::error::Error for EvaluationError {}


// =============================================================================
// Evaluator
// =============================================================================

/// Avaliador big-step de `arith`.
pub struct ArithEvaluator;

impl ArithEvaluator {
    pub fn new() -> Self {
        Self
    }

    // =========================================================================
    // evaluate_term
    // =========================================================================

    fn evaluate_term(
        term: &Term,
    ) -> Result<(Value, Vec<EvaluationRule>), EvaluationError> {
        match term {
            // -----------------------------------------------------------------
            // Integer
            // -----------------------------------------------------------------

            Term::Integer(value) => {
                Ok((
                    Value::Integer(*value),
                    vec![
                        EvaluationRule::Int
                    ],
                ))
            }

            // -----------------------------------------------------------------
            // Boolean
            // -----------------------------------------------------------------

            Term::Boolean(value) => {
                Ok((
                    Value::Boolean(*value),
                    vec![
                        EvaluationRule::Bool
                    ],
                ))
            }

            // -----------------------------------------------------------------
            // Binary
            // -----------------------------------------------------------------

            Term::Binary {
                op,
                lhs,
                rhs,
            } => {
                let (lhs_value, mut lhs_rules) =
                    Self::evaluate_term(lhs)?;

                let (rhs_value, rhs_rules) =
                    Self::evaluate_term(rhs)?;

                lhs_rules.extend(rhs_rules);

                let (value, rule) =
                    Self::evaluate_binary(
                        *op,
                        lhs_value,
                        rhs_value,
                    )?;

                lhs_rules.push(rule);

                Ok((
                    value,
                    lhs_rules,
                ))
            }

            // -----------------------------------------------------------------
            // If
            // -----------------------------------------------------------------

            Term::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let (
                    condition_value,
                    mut condition_rules,
                ) =
                    Self::evaluate_term(condition)?;

                match condition_value {
                    Value::Boolean(true) => {
                        let (
                            value,
                            branch_rules,
                        ) =
                            Self::evaluate_term(
                                then_branch
                            )?;

                        condition_rules
                            .extend(branch_rules);

                        condition_rules.push(
                            EvaluationRule::IfTrue
                        );

                        Ok((
                            value,
                            condition_rules,
                        ))
                    }

                    Value::Boolean(false) => {
                        let (
                            value,
                            branch_rules,
                        ) =
                            Self::evaluate_term(
                                else_branch
                            )?;

                        condition_rules
                            .extend(branch_rules);

                        condition_rules.push(
                            EvaluationRule::IfFalse
                        );

                        Ok((
                            value,
                            condition_rules,
                        ))
                    }

                    value => {
                        Err(
                            EvaluationError::
                                NonBooleanCondition {
                                    value,
                                }
                        )
                    }
                }
            }
        }
    }

    // =========================================================================
    // Binary evaluation
    // =========================================================================

    fn evaluate_binary(
        op: BinaryOp,
        lhs: Value,
        rhs: Value,
    ) -> Result<(Value, EvaluationRule), EvaluationError> {
        match (op, lhs, rhs) {
            // -----------------------------------------------------------------
            // Arithmetic
            // -----------------------------------------------------------------

            (
                BinaryOp::Add,
                Value::Integer(lhs),
                Value::Integer(rhs),
            ) => {
                Ok((
                    Value::Integer(lhs + rhs),
                    EvaluationRule::Add,
                ))
            }

            (
                BinaryOp::Sub,
                Value::Integer(lhs),
                Value::Integer(rhs),
            ) => {
                Ok((
                    Value::Integer(lhs - rhs),
                    EvaluationRule::Sub,
                ))
            }

            (
                BinaryOp::Mul,
                Value::Integer(lhs),
                Value::Integer(rhs),
            ) => {
                Ok((
                    Value::Integer(lhs * rhs),
                    EvaluationRule::Mul,
                ))
            }

            // -----------------------------------------------------------------
            // Comparison
            // -----------------------------------------------------------------

            (
                BinaryOp::LessThan,
                Value::Integer(lhs),
                Value::Integer(rhs),
            ) => {
                Ok((
                    Value::Boolean(lhs < rhs),
                    EvaluationRule::LessThan,
                ))
            }

            // -----------------------------------------------------------------
            // Equality
            // -----------------------------------------------------------------

            (
                BinaryOp::Equal,
                lhs,
                rhs,
            ) => {
                Ok((
                    Value::Boolean(lhs == rhs),
                    EvaluationRule::Equal,
                ))
            }

            // -----------------------------------------------------------------
            // Boolean operators
            // -----------------------------------------------------------------

            (
                BinaryOp::And,
                Value::Boolean(lhs),
                Value::Boolean(rhs),
            ) => {
                Ok((
                    Value::Boolean(lhs && rhs),
                    EvaluationRule::And,
                ))
            }

            (
                BinaryOp::Or,
                Value::Boolean(lhs),
                Value::Boolean(rhs),
            ) => {
                Ok((
                    Value::Boolean(lhs || rhs),
                    EvaluationRule::Or,
                ))
            }

            // -----------------------------------------------------------------
            // Invalid operands
            // -----------------------------------------------------------------

            (operator, lhs, rhs) => {
                Err(
                    EvaluationError::InvalidOperands {
                        operator,
                        lhs,
                        rhs,
                    }
                )
            }
        }
    }

    // =========================================================================
    // Public API
    // =========================================================================

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

    fn evaluate(
        term: &Self::Term,
    ) -> Result<(Self::Value, Vec<Self::Rule>), Self::Error> {
        Self::evaluate_term(term)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn evaluate(
        term: Term,
    ) -> (Value, Vec<EvaluationRule>) {
        ArithEvaluator::evaluate(&term)
            .unwrap()
    }

    #[test]
    fn evaluates_integer() {
        let (value, rules) =
            evaluate(
                Term::integer(42)
            );

        assert_eq!(
            value,
            Value::Integer(42)
        );

        assert_eq!(
            rules,
            vec![
                EvaluationRule::Int
            ]
        );
    }

    #[test]
    fn evaluates_boolean() {
        let (value, rules) =
            evaluate(
                Term::boolean(true)
            );

        assert_eq!(
            value,
            Value::Boolean(true)
        );

        assert_eq!(
            rules,
            vec![
                EvaluationRule::Bool
            ]
        );
    }

    #[test]
    fn evaluates_addition() {
        let term =
            Term::binary(
                BinaryOp::Add,
                Term::integer(1),
                Term::integer(2),
            );

        let (value, rules) =
            evaluate(term);

        assert_eq!(
            value,
            Value::Integer(3)
        );

        assert_eq!(
            rules,
            vec![
                EvaluationRule::Int,
                EvaluationRule::Int,
                EvaluationRule::Add,
            ]
        );
    }

    #[test]
    fn evaluates_precedence_result() {
        let term =
            Term::binary(
                BinaryOp::Add,
                Term::integer(1),
                Term::binary(
                    BinaryOp::Mul,
                    Term::integer(2),
                    Term::integer(3),
                ),
            );

        let (value, _) =
            evaluate(term);

        assert_eq!(
            value,
            Value::Integer(7)
        );
    }

    #[test]
    fn evaluates_comparison() {
        let term =
            Term::binary(
                BinaryOp::LessThan,
                Term::integer(1),
                Term::integer(2),
            );

        let (value, rules) =
            evaluate(term);

        assert_eq!(
            value,
            Value::Boolean(true)
        );

        assert_eq!(
            rules,
            vec![
                EvaluationRule::Int,
                EvaluationRule::Int,
                EvaluationRule::LessThan,
            ]
        );
    }

    #[test]
    fn evaluates_equality() {
        let term =
            Term::binary(
                BinaryOp::Equal,
                Term::integer(10),
                Term::integer(10),
            );

        let (value, _) =
            evaluate(term);

        assert_eq!(
            value,
            Value::Boolean(true)
        );
    }

    #[test]
    fn evaluates_and() {
        let term =
            Term::binary(
                BinaryOp::And,
                Term::boolean(true),
                Term::boolean(false),
            );

        let (value, _) =
            evaluate(term);

        assert_eq!(
            value,
            Value::Boolean(false)
        );
    }

    #[test]
    fn evaluates_or() {
        let term =
            Term::binary(
                BinaryOp::Or,
                Term::boolean(true),
                Term::boolean(false),
            );

        let (value, _) =
            evaluate(term);

        assert_eq!(
            value,
            Value::Boolean(true)
        );
    }

    #[test]
    fn evaluates_if_true() {
        let term =
            Term::if_then_else(
                Term::boolean(true),
                Term::integer(10),
                Term::integer(20),
            );

        let (value, rules) =
            evaluate(term);

        assert_eq!(
            value,
            Value::Integer(10)
        );

        assert_eq!(
            rules,
            vec![
                EvaluationRule::Bool,
                EvaluationRule::Int,
                EvaluationRule::IfTrue,
            ]
        );
    }

    #[test]
    fn evaluates_if_false() {
        let term =
            Term::if_then_else(
                Term::boolean(false),
                Term::integer(10),
                Term::integer(20),
            );

        let (value, rules) =
            evaluate(term);

        assert_eq!(
            value,
            Value::Integer(20)
        );

        assert_eq!(
            rules,
            vec![
                EvaluationRule::Bool,
                EvaluationRule::Int,
                EvaluationRule::IfFalse,
            ]
        );
    }

    #[test]
    fn does_not_evaluate_unused_branch() {
        let term =
            Term::if_then_else(
                Term::boolean(true),
                Term::integer(10),
                Term::binary(
                    BinaryOp::Add,
                    Term::boolean(true),
                    Term::integer(1),
                ),
            );

        let (value, rules) =
            evaluate(term);

        assert_eq!(
            value,
            Value::Integer(10)
        );

        assert_eq!(
            rules,
            vec![
                EvaluationRule::Bool,
                EvaluationRule::Int,
                EvaluationRule::IfTrue,
            ]
        );
    }

    #[test]
    fn rejects_invalid_addition() {
        let term =
            Term::binary(
                BinaryOp::Add,
                Term::boolean(true),
                Term::integer(1),
            );

        let result =
            ArithEvaluator::evaluate(&term);

        assert!(matches!(
            result,
            Err(
                EvaluationError::InvalidOperands {
                    operator: BinaryOp::Add,
                    ..
                }
            )
        ));
    }

    #[test]
    fn rejects_non_boolean_condition() {
        let term =
            Term::if_then_else(
                Term::integer(1),
                Term::integer(2),
                Term::integer(3),
            );

        let result =
            ArithEvaluator::evaluate(&term);

        assert!(matches!(
            result,
            Err(
                EvaluationError::NonBooleanCondition {
                    ..
                }
            )
        ));
    }
}
