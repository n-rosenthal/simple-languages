//! Tokens de `arith`.

use crate::common::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArithTokenType {
    Integer,
    Boolean,
    If,
    Then,
    Else,
    Plus,
    Minus,
    Star,
    LessThan,
    /// `==`
    Equal,
    /// `&&`
    And,
    /// `||`
    Or,
    LeftParen,
    RightParen,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArithToken {
    pub kind: ArithTokenType,
    pub lexeme: String,
    pub span: Span,
}

impl ArithToken {
    pub fn new(kind: ArithTokenType, lexeme: impl Into<String>, span: Span) -> Self {
        Self { kind, lexeme: lexeme.into(), span }
    }
}
