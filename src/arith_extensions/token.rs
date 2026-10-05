///! Tokens de `arith-extensions`.
/// 
/// 
/// t ::= 0
///    | succ t
///    | pred t
///    | iszero t
///    | true
///    | false
///    | if t then t else t
///    | ...
/// term       ::= if term then term else term | binary(0)
/// binary(n)  ::= binary(n+1) (OP_n binary(n+1))*
/// primary    ::= INTEGER | BOOLEAN | 0 | "(" term ")"
/// unary      ::= "succ" unary
///              | "pred" unary
///              | "iszero" unary
///              | primary

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


    // extensions: typing

    Natural,
    Arrow,
    Semicolon,
    Colon,

    Zero,
    Succ,
    Pred,
    IsZero,
}

pub type ArithToken = Token<ArithTokenType>;
