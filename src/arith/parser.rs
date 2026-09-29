//! Parser da linguagem `arith`.

use std::fmt;

use crate::common::{
    Parser,
    Span,
};

use super::terms::{
    BinaryOp,
    Term,
};

use super::token::{
    ArithToken,
    ArithTokenType,
};


// =============================================================================
// TokenStream
// =============================================================================

/// Stream de tokens utilizado pelo parser.
pub struct TokenStream<'a> {
    tokens: &'a [ArithToken],
    position: usize,
}

impl<'a> TokenStream<'a> {
    pub fn new(tokens: &'a [ArithToken]) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    /// Retorna o token atual sem consumi-lo.
    pub fn peek(&self) -> Option<&'a ArithToken> {
        self.tokens.get(self.position)
    }

    /// Consome o token atual.
    pub fn next(&mut self) -> Option<&'a ArithToken> {
        let token = self.tokens.get(self.position);

        if token.is_some() {
            self.position += 1;
        }

        token
    }

    /// Verifica se chegamos ao fim dos tokens.
    pub fn is_at_end(&self) -> bool {
        self.position >= self.tokens.len()
    }

    /// Retorna a posição atual.
    pub fn position(&self) -> usize {
        self.position
    }

    /// Consome um token do tipo esperado.
    pub fn expect(
        &mut self,
        expected: ArithTokenType,
    ) -> Result<ArithToken, ParseError> {
        match self.next() {
            Some(token) if token.kind == expected => {
                Ok(token.clone())
            }

            Some(token) => Err(
                ParseError::UnexpectedToken {
                    expected: Some(expected),
                    found: token.kind,
                    span: token.span,
                },
            ),

            None => Err(
                ParseError::UnexpectedEnd {
                    expected: Some(expected),
                },
            ),
        }
    }
}


// =============================================================================
// ParseError
// =============================================================================

/// Erros produzidos pelo parser.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    /// Um token diferente do esperado foi encontrado.
    UnexpectedToken {
        expected: Option<ArithTokenType>,
        found: ArithTokenType,
        span: Span,
    },

    /// O parser chegou ao fim da entrada antes de terminar a expressão.
    UnexpectedEnd {
        expected: Option<ArithTokenType>,
    },

    /// Restaram tokens depois da expressão principal.
    UnexpectedTrailingToken {
        found: ArithTokenType,
        span: Span,
    },
}

impl fmt::Display for ParseError {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        match self {
            Self::UnexpectedToken {
                expected,
                found,
                span,
            } => {
                match expected {
                    Some(expected) => write!(
                        f,
                        "expected `{expected:?}`, \
                         found `{found:?}` at \
                         {}..{}",
                        span.start,
                        span.end
                    ),

                    None => write!(
                        f,
                        "unexpected token `{found:?}` \
                         at {}..{}",
                        span.start,
                        span.end
                    ),
                }
            }

            Self::UnexpectedEnd { expected } => {
                match expected {
                    Some(expected) => write!(
                        f,
                        "expected `{expected:?}`, \
                         found end of input"
                    ),

                    None => {
                        write!(
                            f,
                            "unexpected end of input"
                        )
                    }
                }
            }

            Self::UnexpectedTrailingToken {
                found,
                span,
            } => {
                write!(
                    f,
                    "unexpected trailing token \
                     `{found:?}` at {}..{}",
                    span.start,
                    span.end
                )
            }
        }
    }
}

impl std::error::Error for ParseError {}


// =============================================================================
// Parser
// =============================================================================

/// Parser recursive-descent de `arith`.
pub struct ArithParser;

impl ArithParser {
    pub fn new() -> Self {
        Self
    }

    // =========================================================================
    // term
    // =========================================================================

    /// term
    ///
    ///     term ::= conditional
    fn parse_term(
        stream: &mut TokenStream<'_>,
    ) -> Result<Term, ParseError> {
        Self::parse_conditional(stream)
    }

    // =========================================================================
    // conditional
    // =========================================================================

    /// conditional
    ///
    ///     conditional
    ///         ::= "if" term "then" term "else" term
    ///          |  equality
    fn parse_conditional(
        stream: &mut TokenStream<'_>,
    ) -> Result<Term, ParseError> {
        let is_if = matches!(
            stream.peek().map(|token| token.kind),
            Some(ArithTokenType::If)
        );

        if !is_if {
            return Self::parse_equality(stream);
        }

        stream.next();

        let condition =
            Self::parse_term(stream)?;

        stream.expect(
            ArithTokenType::Then
        )?;

        let then_branch =
            Self::parse_term(stream)?;

        stream.expect(
            ArithTokenType::Else
        )?;

        let else_branch =
            Self::parse_term(stream)?;

        Ok(
            Term::if_then_else(
                condition,
                then_branch,
                else_branch,
            )
        )
    }

    // =========================================================================
    // equality
    // =========================================================================

    /// equality
    ///
    ///     equality ::= comparison ("==" comparison)*
    fn parse_equality(
        stream: &mut TokenStream<'_>,
    ) -> Result<Term, ParseError> {
        let mut lhs =
            Self::parse_comparison(stream)?;

        while matches!(
            stream.peek().map(|token| token.kind),
            Some(ArithTokenType::Equal)
        ) {
            stream.next();

            let rhs =
                Self::parse_comparison(stream)?;

            lhs = Term::binary(
                BinaryOp::Equal,
                lhs,
                rhs,
            );
        }

        Ok(lhs)
    }

    // =========================================================================
    // comparison
    // =========================================================================

    /// comparison
    ///
    ///     comparison ::= additive ("<" additive)*
    fn parse_comparison(
        stream: &mut TokenStream<'_>,
    ) -> Result<Term, ParseError> {
        let mut lhs =
            Self::parse_additive(stream)?;

        while matches!(
            stream.peek().map(|token| token.kind),
            Some(ArithTokenType::LessThan)
        ) {
            stream.next();

            let rhs =
                Self::parse_additive(stream)?;

            lhs = Term::binary(
                BinaryOp::LessThan,
                lhs,
                rhs,
            );
        }

        Ok(lhs)
    }

    // =========================================================================
    // additive
    // =========================================================================

    /// additive
    ///
    ///     additive ::= multiplicative
    ///                  (("+" | "-") multiplicative)*
    fn parse_additive(
        stream: &mut TokenStream<'_>,
    ) -> Result<Term, ParseError> {
        let mut lhs =
            Self::parse_multiplicative(stream)?;

        loop {
            let op = match stream.peek().map(|token| token.kind) {
                Some(ArithTokenType::Plus) => {
                    BinaryOp::Add
                }

                Some(ArithTokenType::Minus) => {
                    BinaryOp::Sub
                }

                _ => break,
            };

            stream.next();

            let rhs =
                Self::parse_multiplicative(stream)?;

            lhs = Term::binary(
                op,
                lhs,
                rhs,
            );
        }

        Ok(lhs)
    }

    // =========================================================================
    // multiplicative
    // =========================================================================

    /// multiplicative
    ///
    ///     multiplicative ::= primary ("*" primary)*
    fn parse_multiplicative(
        stream: &mut TokenStream<'_>,
    ) -> Result<Term, ParseError> {
        let mut lhs =
            Self::parse_primary(stream)?;

        while matches!(
            stream.peek().map(|token| token.kind),
            Some(ArithTokenType::Star)
        ) {
            stream.next();

            let rhs =
                Self::parse_primary(stream)?;

            lhs = Term::binary(
                BinaryOp::Mul,
                lhs,
                rhs,
            );
        }

        Ok(lhs)
    }

    // =========================================================================
    // primary
    // =========================================================================

    /// primary
    ///
    ///     primary
    ///         ::= INTEGER
    ///          |  BOOLEAN
    ///          |  "(" term ")"
    fn parse_primary(
        stream: &mut TokenStream<'_>,
    ) -> Result<Term, ParseError> {
        let token = match stream.next() {
            Some(token) => token.clone(),

            None => {
                return Err(
                    ParseError::UnexpectedEnd {
                        expected: None,
                    }
                );
            }
        };

        match token.kind {
            ArithTokenType::Integer => {
                let value =
                    token.lexeme.parse::<i64>()
                        .map_err(|_| {
                            ParseError::UnexpectedToken {
                                expected: Some(
                                    ArithTokenType::Integer
                                ),
                                found: token.kind,
                                span: token.span,
                            }
                        })?;

                Ok(Term::integer(value))
            }

            ArithTokenType::Boolean => {
                let value =
                    token.lexeme == "true";

                Ok(Term::boolean(value))
            }

            ArithTokenType::LeftParen => {
                let term =
                    Self::parse_term(stream)?;

                stream.expect(
                    ArithTokenType::RightParen
                )?;

                Ok(term)
            }

            found => Err(
                ParseError::UnexpectedToken {
                    expected: None,
                    found,
                    span: token.span,
                }
            ),
        }
    }

    // =========================================================================
    // parse
    // =========================================================================

    pub fn parse_tokens(
        &self,
        tokens: &[ArithToken],
    ) -> Result<Term, ParseError> {
        let mut stream =
            TokenStream::new(tokens);

        let term =
            Self::parse_term(&mut stream)?;

        if let Some(token) = stream.peek() {
            return Err(
                ParseError::UnexpectedTrailingToken {
                    found: token.kind,
                    span: token.span,
                }
            );
        }

        Ok(term)
    }
}

impl Default for ArithParser {
    fn default() -> Self {
        Self::new()
    }
}

impl Parser for ArithParser {
    type Token = ArithToken;
    type Term = Term;
    type Error = ParseError;

    fn parse(
        input: &[Self::Token],
    ) -> Result<Self::Term, Self::Error> {
        Self::new().parse_tokens(input)
    }
}