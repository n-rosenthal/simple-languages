//! Tokens de `stlc` (cálculo λ simplesmente tipado, com `Bool`).

use crate::common::frontend::Token;

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

pub type StlcToken = Token<StlcTokenType>;
