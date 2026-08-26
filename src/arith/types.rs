//! Tipos da linguagem `arith`.

use std::fmt;

/// Tipo de um termo na linguagem `arith`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Integer,
    Boolean,
}

/// Representação textual de um tipo em `arith`.
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer => write!(f, "Integer"),
            Self::Boolean => write!(f, "Boolean"),
        }
    }
}
