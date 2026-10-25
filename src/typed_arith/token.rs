//! Tokens de `typed-arith`.

use crate::common::frontend::Token;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypedArithTokenType {
    /// Um numeral: `0`, `3` (açúcar para `succ (succ (succ 0))`).
    Integer,
    True,
    False,
    If,
    Then,
    Else,
    Succ,
    Pred,
    IsZero,
    LParen,
    RParen,
}

pub type TypedArithToken = Token<TypedArithTokenType>;
