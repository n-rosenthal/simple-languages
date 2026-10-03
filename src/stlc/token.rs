//! Tokens de `stlc` (cálculo λ simplesmente tipado, com `Bool`).

use crate::common::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StlcTokenType {
    /// `λ` ou `\`
    Lambda,
    /// `.`
    Dot,
    /// `:`
    Colon,
    /// `->`
    Arrow,
    /// `(`
    LParen,
    /// `)`
    RParen,
    /// Variáveis e nomes de tipos base (`x`, `f1`, `Bool`, `A`).
    Identifier,
    /// `true`
    True,
    /// `false`
    False,
    /// `if`
    If,
    /// `then`
    Then,
    /// `else`
    Else,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StlcToken {
    pub kind: StlcTokenType,
    pub lexeme: String,
    pub span: Span,
}

impl StlcToken {
    pub fn new(kind: StlcTokenType, lexeme: impl Into<String>, span: Span) -> Self {
        Self { kind, lexeme: lexeme.into(), span }
    }
}
