//! Regras e implementação do typechecker para `arith`.

use std::fmt;

use crate::common::TypeChecker;

use super::terms::{BinaryOp, Term};
use super::types::Type;

// =============================================================================
// TypingRule
// =============================================================================

/// Uma regra de tipagem utilizada durante uma derivação.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypingRule {
    /// Regra para literais inteiros.
    Integer,

    /// Regra para literais booleanos.
    Boolean,

    /// Regra para adição.
    Add,

    /// Regra para subtração.
    Sub,

    /// Regra para multiplicação.
    Mul,

    /// Regra para comparação menor que.
    LessThan,

    /// Regra para igualdade.
    Equal,

    /// Regra para conjunção.
    And,

    /// Regra para disjunção.
    Or,

    /// Regra para expressões condicionais.
    If,
}

impl fmt::Display for TypingRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Self::Integer => "T-INT",
            Self::Boolean => "T-BOOL",
            Self::Add => "T-ADD",
            Self::Sub => "T-SUB",
            Self::Mul => "T-MUL",
            Self::LessThan => "T-LT",
            Self::Equal => "T-EQ",
            Self::And => "T-AND",
            Self::Or => "T-OR",
            Self::If => "T-IF",
        };

        write!(f, "{name}")
    }
}


// =============================================================================
// TypeError
// =============================================================================

/// Erro produzido pelo typechecker.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    /// Operandos incompatíveis com o operador.
    InvalidBinaryOperands {
        op: BinaryOp,
        lhs: Type,
        rhs: Type,
    },

    /// Condição de um `if` não é booleana.
    InvalidCondition {
        found: Type,
    },

    /// Os ramos de um `if` possuem tipos diferentes.
    BranchTypeMismatch {
        then_type: Type,
        else_type: Type,
    },
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidBinaryOperands { op, lhs, rhs } => {
                write!(
                    f,
                    "operator `{op}` cannot be applied to \
                     `{lhs}` and `{rhs}`"
                )
            }

            Self::InvalidCondition { found } => {
                write!(
                    f,
                    "if condition must have type Boolean, \
                     found `{found}`"
                )
            }

            Self::BranchTypeMismatch {
                then_type,
                else_type,
            } => {
                write!(
                    f,
                    "if branches must have the same type, \
                     found `{then_type}` and `{else_type}`"
                )
            }
        }
    }
}

impl std::error::Error for TypeError {}


// =============================================================================
// TypeChecker
// =============================================================================

/// Typechecker da linguagem `arith`.
pub struct ArithTypeChecker;

impl ArithTypeChecker {
    /// Cria um novo typechecker.
    pub fn new() -> Self {
        Self
    }

    /// Verifica um termo e retorna seu tipo juntamente com a derivação.
    pub fn check_term(
        &self,
        term: &Term,
    ) -> Result<(Type, Vec<TypingRule>), TypeError> {
        match term {
            // -----------------------------------------------------------------
            // T-INT
            // -----------------------------------------------------------------

            Term::Integer(_) => {
                Ok((
                    Type::Integer,
                    vec![TypingRule::Integer],
                ))
            }

            // -----------------------------------------------------------------
            // T-BOOL
            // -----------------------------------------------------------------

            Term::Boolean(_) => {
                Ok((
                    Type::Boolean,
                    vec![TypingRule::Boolean],
                ))
            }

            // -----------------------------------------------------------------
            // Binary operators
            // -----------------------------------------------------------------

            Term::Binary { op, lhs, rhs } => {
                let (lhs_type, mut lhs_rules) =
                    self.check_term(lhs)?;

                let (rhs_type, rhs_rules) =
                    self.check_term(rhs)?;

                lhs_rules.extend(rhs_rules);

                let (result_type, rule) =
                    Self::check_binary(
                        *op,
                        lhs_type,
                        rhs_type,
                    )?;

                lhs_rules.push(rule);

                Ok((result_type, lhs_rules))
            }

            // -----------------------------------------------------------------
            // T-IF
            // -----------------------------------------------------------------

            Term::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let (condition_type, mut condition_rules) =
                    self.check_term(condition)?;

                if condition_type != Type::Boolean {
                    return Err(TypeError::InvalidCondition {
                        found: condition_type,
                    });
                }

                let (then_type, then_rules) =
                    self.check_term(then_branch)?;

                let (else_type, else_rules) =
                    self.check_term(else_branch)?;

                if then_type != else_type {
                    return Err(TypeError::BranchTypeMismatch {
                        then_type,
                        else_type,
                    });
                }

                condition_rules.extend(then_rules);
                condition_rules.extend(else_rules);
                condition_rules.push(TypingRule::If);

                Ok((then_type, condition_rules))
            }
        }
    }

    /// Verifica a regra correspondente a uma operação binária.
    fn check_binary(
        op: BinaryOp,
        lhs: Type,
        rhs: Type,
    ) -> Result<(Type, TypingRule), TypeError> {
        match op {
            BinaryOp::Add
                if lhs == Type::Integer
                    && rhs == Type::Integer =>
            {
                Ok((Type::Integer, TypingRule::Add))
            }

            BinaryOp::Sub
                if lhs == Type::Integer
                    && rhs == Type::Integer =>
            {
                Ok((Type::Integer, TypingRule::Sub))
            }

            BinaryOp::Mul
                if lhs == Type::Integer
                    && rhs == Type::Integer =>
            {
                Ok((Type::Integer, TypingRule::Mul))
            }

            BinaryOp::LessThan
                if lhs == Type::Integer
                    && rhs == Type::Integer =>
            {
                Ok((Type::Boolean, TypingRule::LessThan))
            }

            BinaryOp::Equal
                if lhs == rhs =>
            {
                Ok((Type::Boolean, TypingRule::Equal))
            }

            BinaryOp::And
                if lhs == Type::Boolean
                    && rhs == Type::Boolean =>
            {
                Ok((Type::Boolean, TypingRule::And))
            }

            BinaryOp::Or
                if lhs == Type::Boolean
                    && rhs == Type::Boolean =>
            {
                Ok((Type::Boolean, TypingRule::Or))
            }

            _ => Err(TypeError::InvalidBinaryOperands {
                op,
                lhs,
                rhs,
            }),
        }
    }
}

impl Default for ArithTypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeChecker for ArithTypeChecker {
    type Term = Term;
    type Type = Type;
    type Rule = TypingRule;
    type Error = TypeError;

    fn check(
        term: &Self::Term,
    ) -> Result<(Self::Type, Vec<Self::Rule>), Self::Error> {
        Self::new().check_term(term)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn integer_literal() {
        let term = Term::integer(42);

        let result =
            ArithTypeChecker::new()
                .check_term(&term)
                .unwrap();

        assert_eq!(result.0, Type::Integer);
        assert_eq!(
            result.1,
            vec![TypingRule::Integer]
        );
    }

    #[test]
    fn boolean_literal() {
        let term = Term::boolean(true);

        let result =
            ArithTypeChecker::new()
                .check_term(&term)
                .unwrap();

        assert_eq!(result.0, Type::Boolean);
        assert_eq!(
            result.1,
            vec![TypingRule::Boolean]
        );
    }

    #[test]
    fn addition() {
        let term = Term::binary(
            BinaryOp::Add,
            Term::integer(1),
            Term::integer(2),
        );

        let result =
            ArithTypeChecker::new()
                .check_term(&term)
                .unwrap();

        assert_eq!(result.0, Type::Integer);

        assert_eq!(
            result.1,
            vec![
                TypingRule::Integer,
                TypingRule::Integer,
                TypingRule::Add,
            ]
        );
    }

    #[test]
    fn less_than() {
        let term = Term::binary(
            BinaryOp::LessThan,
            Term::integer(1),
            Term::integer(2),
        );

        let result =
            ArithTypeChecker::new()
                .check_term(&term)
                .unwrap();

        assert_eq!(result.0, Type::Boolean);
    }

    #[test]
    fn equality_of_integers() {
        let term = Term::binary(
            BinaryOp::Equal,
            Term::integer(1),
            Term::integer(2),
        );

        let result =
            ArithTypeChecker::new()
                .check_term(&term)
                .unwrap();

        assert_eq!(result.0, Type::Boolean);
    }

    #[test]
    fn equality_of_booleans() {
        let term = Term::binary(
            BinaryOp::Equal,
            Term::boolean(true),
            Term::boolean(false),
        );

        let result =
            ArithTypeChecker::new()
                .check_term(&term)
                .unwrap();

        assert_eq!(result.0, Type::Boolean);
    }

    #[test]
    fn invalid_addition() {
        let term = Term::binary(
            BinaryOp::Add,
            Term::integer(1),
            Term::boolean(true),
        );

        let result =
            ArithTypeChecker::new()
                .check_term(&term);

        assert!(matches!(
            result,
            Err(TypeError::InvalidBinaryOperands {
                op: BinaryOp::Add,
                lhs: Type::Integer,
                rhs: Type::Boolean,
            })
        ));
    }

    #[test]
    fn conditional() {
        let term = Term::if_then_else(
            Term::binary(
                BinaryOp::LessThan,
                Term::integer(1),
                Term::integer(2),
            ),
            Term::integer(10),
            Term::integer(20),
        );

        let result =
            ArithTypeChecker::new()
                .check_term(&term)
                .unwrap();

        assert_eq!(result.0, Type::Integer);

        assert_eq!(
            result.1,
            vec![
                TypingRule::Integer,
                TypingRule::Integer,
                TypingRule::LessThan,
                TypingRule::Integer,
                TypingRule::Integer,
                TypingRule::If,
            ]
        );
    }

    #[test]
    fn invalid_condition() {
        let term = Term::if_then_else(
            Term::integer(1),
            Term::integer(10),
            Term::integer(20),
        );

        let result =
            ArithTypeChecker::new()
                .check_term(&term);

        assert!(matches!(
            result,
            Err(TypeError::InvalidCondition {
                found: Type::Integer,
            })
        ));
    }

    #[test]
    fn mismatched_branches() {
        let term = Term::if_then_else(
            Term::boolean(true),
            Term::integer(10),
            Term::boolean(false),
        );

        let result =
            ArithTypeChecker::new()
                .check_term(&term);

        assert!(matches!(
            result,
            Err(TypeError::BranchTypeMismatch {
                then_type: Type::Integer,
                else_type: Type::Boolean,
            })
        ));
    }
}
