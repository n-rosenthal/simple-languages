//! Tokens de `lambda` (cálculo λ simplesmente tipado).

use crate::common::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LambdaTokenType {
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
    /// Variáveis e nomes de tipos base (`x`, `f1`, `Bool`, `Nat`).
    Identifier,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LambdaToken {
    pub kind: LambdaTokenType,
    pub lexeme: String,
    pub span: Span,
}

impl LambdaToken {
    pub fn new(kind: LambdaTokenType, lexeme: impl Into<String>, span: Span) -> Self {
        Self { kind, lexeme: lexeme.into(), span }
    }
}
