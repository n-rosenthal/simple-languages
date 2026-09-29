//! `simple-languages/lambda/types.rs` defines the type system for the lambda calculus language.
//! 
//! Author:     n-rosenthal
//! Date:       2026-09-28
//! Version:    0.1.0

use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// The type of boolean values.
    Boolean,

    /// Function type (arrow)
    Arrow(Box<Type>, Box<Type>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Boolean => write!(f, "Boolean"),
            Self::Arrow(lhs, rhs) => write!(f, "({lhs} -> {rhs})"),
        }
    }
}