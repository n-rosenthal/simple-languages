//! Parser de `arith`: descida recursiva com uma tabela de precedência.
//!
//! ```text
//! term        ::= "if" term "then" term "else" term | binary(0)
//! binary(n)   ::= binary(n+1) (OP_n binary(n+1))*          (assoc. à esquerda)
//! primary     ::= INTEGER | BOOLEAN | "(" term ")"
//! ```
//!
//! Níveis de precedência, do mais fraco para o mais forte:
//! `||`, `&&`, `==`, `<`, `+ -`, `*`. Um `if` como operando precisa de
//! parênteses.

use std::fmt;

use crate::common::{Parser, Span};

use super::terms::{BinaryOp, Term};
use super::token::{ArithToken, ArithTokenType};

use ArithTokenType as T;

const LEVELS: &[&[(ArithTokenType, BinaryOp)]] = &[
    &[(T::Or, BinaryOp::Or)],
    &[(T::And, BinaryOp::And)],
    &[(T::Equal, BinaryOp::Equal)],
    &[(T::LessThan, BinaryOp::LessThan)],
    &[(T::Plus, BinaryOp::Add), (T::Minus, BinaryOp::Sub)],
    &[(T::Star, BinaryOp::Mul)],
];

// =============================================================================
// TokenStream
// =============================================================================

pub struct TokenStream<'a> {
    tokens: &'a [ArithToken],
    position: usize,
}

impl<'a> TokenStream<'a> {
    pub fn new(tokens: &'a [ArithToken]) -> Self {
        Self { tokens, position: 0 }
    }

    pub fn peek(&self) -> Option<&'a ArithToken> {
        self.tokens.get(self.position)
    }

    pub fn peek_kind(&self) -> Option<ArithTokenType> {
        self.peek().map(|token| token.kind)
    }

    pub fn next(&mut self) -> Option<&'a ArithToken> {
        let token = self.tokens.get(self.position);
        if token.is_some() {
            self.position += 1;
        }
        token
    }

    pub fn expect(&mut self, expected: ArithTokenType) -> Result<ArithToken, ParseError> {
        match self.next() {
            Some(token) if token.kind == expected => Ok(token.clone()),
            Some(token) => Err(ParseError::UnexpectedToken {
                expected: Some(expected),
                found: token.kind,
                span: token.span,
            }),
            None => Err(ParseError::UnexpectedEnd { expected: Some(expected) }),
        }
    }
}

// =============================================================================
// ParseError
// =============================================================================

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    UnexpectedToken {
        expected: Option<ArithTokenType>,
        found: ArithTokenType,
        span: Span,
    },
    UnexpectedEnd {
        expected: Option<ArithTokenType>,
    },
    UnexpectedTrailingToken {
        found: ArithTokenType,
        span: Span,
    },
    /// Um literal que não cabe em `i64`.
    InvalidInteger {
        lexeme: String,
        span: Span,
    },
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedToken { expected: Some(expected), found, span } => write!(
                f,
                "expected `{expected:?}`, found `{found:?}` at {}..{}",
                span.start, span.end
            ),
            Self::UnexpectedToken { expected: None, found, span } => write!(
                f,
                "unexpected token `{found:?}` at {}..{}",
                span.start, span.end
            ),
            Self::UnexpectedEnd { expected: Some(expected) } => {
                write!(f, "expected `{expected:?}`, found end of input")
            }
            Self::UnexpectedEnd { expected: None } => write!(f, "unexpected end of input"),
            Self::UnexpectedTrailingToken { found, span } => write!(
                f,
                "unexpected trailing token `{found:?}` at {}..{}",
                span.start, span.end
            ),
            Self::InvalidInteger { lexeme, span } => write!(
                f,
                "integer `{lexeme}` at {}..{} does not fit in 64 bits",
                span.start, span.end
            ),
        }
    }
}

impl std::error::Error for ParseError {}

// =============================================================================
// Parser
// =============================================================================

pub struct ArithParser;

impl ArithParser {
    pub fn new() -> Self {
        Self
    }

    /// term ::= "if" term "then" term "else" term | binary(0)
    fn parse_term(stream: &mut TokenStream<'_>) -> Result<Term, ParseError> {
        if stream.peek_kind() != Some(T::If) {
            return Self::parse_binary(stream, 0);
        }

        stream.next();
        let condition = Self::parse_term(stream)?;
        stream.expect(T::Then)?;
        let then_branch = Self::parse_term(stream)?;
        stream.expect(T::Else)?;
        let else_branch = Self::parse_term(stream)?;

        Ok(Term::if_then_else(condition, then_branch, else_branch))
    }

    /// binary(n) ::= binary(n+1) (OP_n binary(n+1))*
    fn parse_binary(stream: &mut TokenStream<'_>, level: usize) -> Result<Term, ParseError> {
        if level == LEVELS.len() {
            return Self::parse_primary(stream);
        }

        let mut lhs = Self::parse_binary(stream, level + 1)?;

        while let Some(op) = stream.peek_kind().and_then(|kind| {
            LEVELS[level]
                .iter()
                .find(|(token, _)| *token == kind)
                .map(|(_, op)| *op)
        }) {
            stream.next();
            let rhs = Self::parse_binary(stream, level + 1)?;
            lhs = Term::binary(op, lhs, rhs);
        }

        Ok(lhs)
    }

    /// primary ::= INTEGER | BOOLEAN | "(" term ")"
    fn parse_primary(stream: &mut TokenStream<'_>) -> Result<Term, ParseError> {
        let token = match stream.next() {
            Some(token) => token.clone(),
            None => return Err(ParseError::UnexpectedEnd { expected: None }),
        };

        match token.kind {
            T::Integer => token
                .lexeme
                .parse::<i64>()
                .map(Term::integer)
                .map_err(|_| ParseError::InvalidInteger {
                    lexeme: token.lexeme.clone(),
                    span: token.span,
                }),

            T::Boolean => Ok(Term::boolean(token.lexeme == "true")),

            T::LeftParen => {
                let term = Self::parse_term(stream)?;
                stream.expect(T::RightParen)?;
                Ok(term)
            }

            found => Err(ParseError::UnexpectedToken {
                expected: None,
                found,
                span: token.span,
            }),
        }
    }

    pub fn parse_tokens(&self, tokens: &[ArithToken]) -> Result<Term, ParseError> {
        let mut stream = TokenStream::new(tokens);
        let term = Self::parse_term(&mut stream)?;

        if let Some(token) = stream.peek() {
            return Err(ParseError::UnexpectedTrailingToken {
                found: token.kind,
                span: token.span,
            });
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

    fn parse(input: &[ArithToken]) -> Result<Term, ParseError> {
        Self::new().parse_tokens(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arith::{ArithLexer, ArithScanner};
    use crate::common::{Lexer, Scanner};

    fn parse(source: &str) -> Result<Term, ParseError> {
        let lines = ArithScanner::scan(source).unwrap();
        let tokens = ArithLexer::analyze(&lines).unwrap();
        ArithParser::parse(&tokens)
    }

    fn int(n: i64) -> Term {
        Term::integer(n)
    }

    fn bin(op: BinaryOp, l: Term, r: Term) -> Term {
        Term::binary(op, l, r)
    }

    #[test]
    fn literals() {
        assert_eq!(parse("42").unwrap(), int(42));
        assert_eq!(parse("true").unwrap(), Term::boolean(true));
    }

    #[test]
    fn multiplication_binds_tighter_than_addition() {
        assert_eq!(
            parse("1 + 2 * 3").unwrap(),
            bin(BinaryOp::Add, int(1), bin(BinaryOp::Mul, int(2), int(3)))
        );
    }

    #[test]
    fn subtraction_is_left_associative() {
        assert_eq!(
            parse("10 - 3 - 2").unwrap(),
            bin(BinaryOp::Sub, bin(BinaryOp::Sub, int(10), int(3)), int(2))
        );
    }

    #[test]
    fn comparison_binds_tighter_than_equality() {
        assert_eq!(
            parse("1 < 2 == true").unwrap(),
            bin(
                BinaryOp::Equal,
                bin(BinaryOp::LessThan, int(1), int(2)),
                Term::boolean(true)
            )
        );
    }

    #[test]
    fn and_binds_tighter_than_or() {
        assert_eq!(
            parse("true || false && false").unwrap(),
            bin(
                BinaryOp::Or,
                Term::boolean(true),
                bin(BinaryOp::And, Term::boolean(false), Term::boolean(false))
            )
        );
    }

    #[test]
    fn parentheses_override_precedence() {
        assert_eq!(
            parse("(1 + 2) * 3").unwrap(),
            bin(BinaryOp::Mul, bin(BinaryOp::Add, int(1), int(2)), int(3))
        );
    }

    #[test]
    fn conditional() {
        assert_eq!(
            parse("if 1 < 2 then 10 else 20").unwrap(),
            Term::if_then_else(bin(BinaryOp::LessThan, int(1), int(2)), int(10), int(20))
        );
    }

    #[test]
    fn nested_conditionals() {
        assert_eq!(
            parse("if true then if false then 1 else 2 else 3").unwrap(),
            Term::if_then_else(
                Term::boolean(true),
                Term::if_then_else(Term::boolean(false), int(1), int(2)),
                int(3)
            )
        );
    }

    #[test]
    fn if_as_operand_needs_parentheses() {
        assert!(parse("1 + if true then 2 else 3").is_err());
        assert!(parse("1 + (if true then 2 else 3)").is_ok());
    }

    #[test]
    fn rejects_malformed_input() {
        for bad in ["", "1 +", "(1", "1)", "1 2", "if true then 1", "+ 1", "if then 1 else 2"] {
            assert!(parse(bad).is_err(), "should reject {bad:?}");
        }
    }

    #[test]
    fn huge_integers_are_rejected() {
        assert!(matches!(
            parse("99999999999999999999"),
            Err(ParseError::InvalidInteger { .. })
        ));
    }

    #[test]
    fn print_then_parse_roundtrips() {
        let samples = [
            bin(BinaryOp::Add, int(1), bin(BinaryOp::Mul, int(2), int(3))),
            Term::if_then_else(bin(BinaryOp::LessThan, int(1), int(2)), int(3), int(4)),
            bin(
                BinaryOp::Add,
                Term::if_then_else(Term::boolean(true), int(1), int(2)),
                int(3),
            ),
            bin(
                BinaryOp::Or,
                bin(BinaryOp::And, Term::boolean(true), Term::boolean(false)),
                Term::boolean(true),
            ),
        ];
        for term in samples {
            assert_eq!(parse(&term.to_string()).unwrap(), term, "printed: {term}");
        }
    }
}
