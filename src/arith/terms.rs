//! Termos da linguagem `arith`.
//!
//! Um termo representa uma expressão sintaticamente válida da linguagem.

use std::fmt;

// =============================================================================
// Operadores binários
// =============================================================================

/// Operadores binários da linguagem.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    LessThan,
    Equal,
    And,
    Or,
}

impl BinaryOp {
    /// Retorna a representação textual do operador.
    pub fn symbol(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::LessThan => "<",
            Self::Equal => "==",
            Self::And => "&&",
            Self::Or => "||",
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.symbol())
    }
}

// =============================================================================
// Term
// =============================================================================

/// Termos da linguagem `arith`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    /// Literal inteiro.
    Integer(i64),

    /// Literal booleano.
    Boolean(bool),

    /// Operação binária.
    Binary {
        /// Operador da expressão.
        op: BinaryOp,

        /// Operando esquerdo.
        lhs: Box<Term>,

        /// Operando direito.
        rhs: Box<Term>,
    },

    /// Expressão condicional.
    If {
        /// Condição.
        condition: Box<Term>,

        /// Ramo executado quando a condição é verdadeira.
        then_branch: Box<Term>,

        /// Ramo executado quando a condição é falsa.
        else_branch: Box<Term>,
    },
}

impl Term {
    // -------------------------------------------------------------------------
    // Construtores
    // -------------------------------------------------------------------------

    /// Cria um literal inteiro.
    pub fn integer(value: i64) -> Self {
        Self::Integer(value)
    }

    /// Cria um literal booleano.
    pub fn boolean(value: bool) -> Self {
        Self::Boolean(value)
    }

    /// Cria uma expressão binária.
    pub fn binary(
        op: BinaryOp,
        lhs: Self,
        rhs: Self,
    ) -> Self {
        Self::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    /// Cria uma expressão condicional.
    pub fn if_then_else(
        condition: Self,
        then_branch: Self,
        else_branch: Self,
    ) -> Self {
        Self::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        }
    }

    // -------------------------------------------------------------------------
    // Predicados
    // -------------------------------------------------------------------------

    /// Indica se o termo é uma forma final.
    ///
    /// Em `arith`, os únicos valores são literais inteiros e booleanos.
    pub fn is_value(&self) -> bool {
        matches!(
            self,
            Self::Integer(_) | Self::Boolean(_)
        )
    }
}

// =============================================================================
// Display
// =============================================================================

/// Representação textual de um termo.
impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer(value) => {
                write!(f, "{value}")
            }

            Self::Boolean(value) => {
                write!(f, "{value}")
            }

            Self::Binary { op, lhs, rhs } => {
                write!(f, "({lhs} {op} {rhs})")
            }

            Self::If {
                condition,
                then_branch,
                else_branch,
            } => {
                write!(
                    f,
                    "if {condition} then {then_branch} else {else_branch}"
                )
            }
        }
    }
}
