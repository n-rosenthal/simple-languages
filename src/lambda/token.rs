//! Tokens de `lambda`.

use crate::common::frontend::Token;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LambdaTokenType {
    /// `λ` ou `\`
    Lambda,
    /// `.`
    Dot,
    LParen,
    RParen,
    Identifier,
}

pub type LambdaToken = Token<LambdaTokenType>;
