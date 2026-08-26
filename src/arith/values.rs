//! Valores da linguagem `arith`.
//!
//! Um valor é uma forma final de um termo, isto é, uma expressão
//! que não pode mais ser avaliada.

use std::fmt;

use super::types::Type;

/// Valores produzidos pela avaliação de termos.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
}

impl Value {
    /// Retorna o tipo do valor.
    pub fn ty(&self) -> Type {
        match self {
            Self::Integer(_) => Type::Integer,
            Self::Boolean(_) => Type::Boolean,
        }
    }
}

/// Representação textual de um valor em `arith`.
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer(value) => write!(f, "{value}"),
            Self::Boolean(value) => write!(f, "{value}"),
        }
    }
}
