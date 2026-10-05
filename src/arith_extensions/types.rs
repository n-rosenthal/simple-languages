///! Tipos de `arith-extensions`: inteiros, booleanos, naturais e funções.

use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Integer,
    Boolean,

    // Extensions
    Natural,

    Arrow(Box<Type>, Box<Type>),

    Exception(String),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Integer => f.write_str("Integer"),
            Type::Boolean => f.write_str("Boolean"),
            Type::Natural => f.write_str("Natural"),
            Type::Arrow(from, to) => {
                write!(f, "({from} → {to})")
            },
            Type::Exception(msg) => write!(f, "Exception({msg})"),
        }
    }
}