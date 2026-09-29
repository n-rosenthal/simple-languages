//! `simple-languages/lambda/token.rs` defines the `Token` type, which represents the tokens of the lambda calculus language.
//!
//! Author:     n-rosenthal
//! Date:       2026-09-28
//! Version:    0.1.0
//! 

// std::fmt is used to implement the `Display` trait
use std::fmt;

use crate::common::{
    Span,       //  Span represents the location of a token in the source code
    Token,      //  Token represents a token in the source code
    TokenType,  //  TokenType is a trait that defines the behavior of token types
};

// === 
// LambdaTokenType
// ===

/// token categories recognized by the `lambda` language.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LambdaTokenType {
    // Literals
    Identifier,           // x, identifier (variables)

    // Keywords
    Lambda,             // λ, lambda abstraction
    Dot,                // ., dot in lambda abstraction
    Colon,              // :, colon in type annotations
    Arrow,              // ->, arrow in type annotations
    LeftParen,          // (, left parenthesis
    RightParen,         // ), right parenthesis

    If,                 // if, conditional
    Then,               // then, conditional
    Else,               // else, conditional

    // Types
    Boolean,          // bool, boolean type
    Integer,          // int, integer type
}

// Implement the `TokenType` trait for `LambdaTokenType`
// This means that `LambdaTokenType` can be used as a token type in the `Token` struct.
impl TokenType for LambdaTokenType {}

// Implement the `Display` trait for `LambdaTokenType`
// Display is used to convert the token type to a string representation
impl fmt::Display for LambdaTokenType {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let text = match self {
            Self::Identifier => "var",

            Self::Lambda => "λ",
            Self::Dot => ".",
            Self::LeftParen => "(",
            Self::RightParen => ")",
            Self::Colon => ":",
            Self::Arrow => "->",


            Self::If => "if",
            Self::Then => "then",
            Self::Else => "else",

            Self::Boolean => "boolean",
            Self::Integer => "integer",
        };
        write!(f, "{}", text)
    }
}

// ===
//  LambdaToken
//  Provides a concrete implementation of the `Token` struct for the `lambda` language.
// ===

/// Concrete implementation of the `Token` struct for the `lambda` language.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LambdaToken {
    //  The category of the token, represented by the `LambdaTokenType` enum.
    pub kind: LambdaTokenType,

    //  The text of the token, represented as a string.
    pub lexeme: String,
    
    //  The location of the token in the source code, represented by the `Span` struct.
    pub span: Span,
}

// Implement the `Token` trait for `LambdaToken`
// This means that `LambdaToken` can be used as a token in the `Token` struct.
impl LambdaToken {
    /// Creates a new `LambdaToken` with the given kind, lexeme, and span.
    pub fn new(
        kind: LambdaTokenType,
        lexeme: impl Into<String>,
        span: Span
    )   ->  Self {
            Self { 
                kind,
                lexeme: lexeme.into(),
                span
            }
    }
}

// Implement the `Token` trait for `LambdaToken`
// This means that `LambdaToken` can be used as a token in the `Token` struct.
impl Token for LambdaToken {
    type Type = LambdaTokenType;

    fn kind(&self) -> Self::Type {
        self.kind
    }

    fn span(&self) -> Span {
        self.span
    }

    fn lexeme(&self) -> &str {
        &self.lexeme
    }
}