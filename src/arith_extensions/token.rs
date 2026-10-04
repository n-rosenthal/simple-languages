//! Tokens de `arith-extensions`.

use crate::common::frontend::Token;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArithTokenType {
    Integer,
    Boolean,
    If,
    Then,
    Else,
    Plus,
    Minus,

    Slash,        /// `/`
    Percent,        /// `%`

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

pub type ArithToken = Token<ArithTokenType>;
