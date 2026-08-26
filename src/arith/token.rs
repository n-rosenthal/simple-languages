//! Tokens da linguagem `arith`.

use std::fmt;

use crate::common::{
    Span,
    Token,
    TokenType,
};

// =============================================================================
// ArithTokenType
// =============================================================================

/// Categorias de tokens reconhecidas pela linguagem `arith`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArithTokenType {
    // Literals
    Integer,
    Boolean,

    // Keywords
    If,
    Then,
    Else,

    // Operators
    Plus,
    Minus,
    Star,
    LessThan,
    Equal,
    And,
    Or,

    // Delimiters
    LeftParen,
    RightParen,
}

impl TokenType for ArithTokenType {}

impl fmt::Display for ArithTokenType {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let text = match self {
            Self::Integer => "integer",
            Self::Boolean => "boolean",

            Self::If => "if",
            Self::Then => "then",
            Self::Else => "else",

            Self::Plus => "+",
            Self::Minus => "-",
            Self::Star => "*",
            Self::LessThan => "<",
            Self::Equal => "==",
            Self::And => "&&",
            Self::Or => "||",

            Self::LeftParen => "(",
            Self::RightParen => ")",
        };

        write!(f, "{text}")
    }
}

// =============================================================================
// ArithToken
// =============================================================================

/// Token concreto da linguagem `arith`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArithToken {
    /// Categoria do token.
    pub kind: ArithTokenType,

    /// Texto original correspondente ao token.
    pub lexeme: String,

    /// Posição do token dentro da entrada.
    pub span: Span,
}

impl ArithToken {
    pub fn new(
        kind: ArithTokenType,
        lexeme: impl Into<String>,
        span: Span,
    ) -> Self {
        Self {
            kind,
            lexeme: lexeme.into(),
            span,
        }
    }
}

impl Token for ArithToken {
    type Type = ArithTokenType;

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