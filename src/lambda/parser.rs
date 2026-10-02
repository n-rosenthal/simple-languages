//! Parser da linguagem `lambda` (cálculo λ simplesmente tipado).
//!
//! Gramática:
//!
//! ```text
//! term        ::= atom atom*                      (aplicação, assoc. à esquerda)
//! atom        ::= IDENT | "(" term ")" | abstraction
//! abstraction ::= "λ" IDENT ":" type "." term     (o corpo vai o mais à direita possível)
//! type        ::= atype ("->" type)?              (assoc. à direita)
//! atype       ::= IDENT | "(" type ")"
//! ```

use std::fmt;

use crate::common::{Parser, Span};

use super::terms::Term;
use super::token::{LambdaToken, LambdaTokenType};
use super::types::Type;

// =============================================================================
// TokenStream
// =============================================================================

pub struct TokenStream<'a> {
    tokens: &'a [LambdaToken],
    position: usize,
}

impl<'a> TokenStream<'a> {
    pub fn new(tokens: &'a [LambdaToken]) -> Self {
        Self { tokens, position: 0 }
    }

    pub fn peek(&self) -> Option<&'a LambdaToken> {
        self.tokens.get(self.position)
    }

    pub fn peek_kind(&self) -> Option<LambdaTokenType> {
        self.peek().map(|token| token.kind)
    }

    pub fn next(&mut self) -> Option<&'a LambdaToken> {
        let token = self.tokens.get(self.position);
        if token.is_some() {
            self.position += 1;
        }
        token
    }

    pub fn is_at_end(&self) -> bool {
        self.position >= self.tokens.len()
    }

    pub fn position(&self) -> usize {
        self.position
    }

    pub fn expect(&mut self, expected: LambdaTokenType) -> Result<LambdaToken, ParseError> {
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
        expected: Option<LambdaTokenType>,
        found: LambdaTokenType,
        span: Span,
    },
    UnexpectedEnd {
        expected: Option<LambdaTokenType>,
    },
    UnexpectedTrailingToken {
        found: LambdaTokenType,
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
        }
    }
}

impl std::error::Error for ParseError {}

// =============================================================================
// Parser
// =============================================================================

pub struct LambdaParser;

impl LambdaParser {
    pub fn new() -> Self {
        Self
    }

    /// term ::= atom atom*
    fn parse_term(stream: &mut TokenStream<'_>) -> Result<Term, ParseError> {
        let mut lhs = Self::parse_atom(stream)?;

        while matches!(
            stream.peek_kind(),
            Some(LambdaTokenType::Identifier)
                | Some(LambdaTokenType::LParen)
                | Some(LambdaTokenType::Lambda)
        ) {
            let rhs = Self::parse_atom(stream)?;
            lhs = Term::app(lhs, rhs);
        }

        Ok(lhs)
    }

    /// atom ::= IDENT | "(" term ")" | abstraction
    fn parse_atom(stream: &mut TokenStream<'_>) -> Result<Term, ParseError> {
        if stream.peek_kind() == Some(LambdaTokenType::Lambda) {
            return Self::parse_abstraction(stream);
        }

        let token = match stream.next() {
            Some(token) => token.clone(),
            None => return Err(ParseError::UnexpectedEnd { expected: None }),
        };

        match token.kind {
            LambdaTokenType::Identifier => Ok(Term::variable(token.lexeme)),

            LambdaTokenType::LParen => {
                let term = Self::parse_term(stream)?;
                stream.expect(LambdaTokenType::RParen)?;
                Ok(term)
            }

            found => Err(ParseError::UnexpectedToken {
                expected: None,
                found,
                span: token.span,
            }),
        }
    }

    /// abstraction ::= "λ" IDENT ":" type "." term
    fn parse_abstraction(stream: &mut TokenStream<'_>) -> Result<Term, ParseError> {
        stream.expect(LambdaTokenType::Lambda)?;
        let param = stream.expect(LambdaTokenType::Identifier)?;
        stream.expect(LambdaTokenType::Colon)?;
        let ty = Self::parse_type(stream)?;
        stream.expect(LambdaTokenType::Dot)?;
        let body = Self::parse_term(stream)?;

        Ok(Term::lambda(param.lexeme, ty, body))
    }

    /// type ::= atype ("->" type)?
    fn parse_type(stream: &mut TokenStream<'_>) -> Result<Type, ParseError> {
        let from = Self::parse_atomic_type(stream)?;

        if stream.peek_kind() == Some(LambdaTokenType::Arrow) {
            stream.next();
            let to = Self::parse_type(stream)?; // associativo à direita
            return Ok(Type::arrow(from, to));
        }

        Ok(from)
    }

    /// atype ::= IDENT | "(" type ")"
    fn parse_atomic_type(stream: &mut TokenStream<'_>) -> Result<Type, ParseError> {
        let token = match stream.next() {
            Some(token) => token.clone(),
            None => return Err(ParseError::UnexpectedEnd { expected: None }),
        };

        match token.kind {
            LambdaTokenType::Identifier => Ok(Type::base(token.lexeme)),

            LambdaTokenType::LParen => {
                let ty = Self::parse_type(stream)?;
                stream.expect(LambdaTokenType::RParen)?;
                Ok(ty)
            }

            found => Err(ParseError::UnexpectedToken {
                expected: None,
                found,
                span: token.span,
            }),
        }
    }

    pub fn parse_tokens(&self, tokens: &[LambdaToken]) -> Result<Term, ParseError> {
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

impl Default for LambdaParser {
    fn default() -> Self {
        Self::new()
    }
}

impl Parser for LambdaParser {
    type Token = LambdaToken;
    type Term = Term;
    type Error = ParseError;

    fn parse(input: &[Self::Token]) -> Result<Self::Term, Self::Error> {
        Self::new().parse_tokens(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::{Lexer, Scanner};
    use crate::lambda::lexer::LambdaLexer;
    use crate::lambda::scanner::LambdaScanner;

    fn var(s: &str) -> Term {
        Term::variable(s)
    }

    fn base(s: &str) -> Type {
        Type::base(s)
    }

    fn parse_str(src: &str) -> Result<Term, ParseError> {
        let lines = LambdaScanner::scan(src).unwrap();
        let tokens = LambdaLexer::analyze(&lines).unwrap();
        LambdaParser::parse(&tokens)
    }

    #[test]
    fn identity() {
        assert_eq!(
            parse_str("λx:Bool. x").unwrap(),
            Term::lambda("x", base("Bool"), var("x")),
        );
    }

    #[test]
    fn application_is_left_associative() {
        assert_eq!(
            parse_str("f x y").unwrap(),
            Term::app(Term::app(var("f"), var("x")), var("y")),
        );
    }

    #[test]
    fn arrow_is_right_associative() {
        assert_eq!(
            parse_str("λx:A->B->C. x").unwrap(),
            Term::lambda(
                "x",
                Type::arrow(base("A"), Type::arrow(base("B"), base("C"))),
                var("x"),
            ),
        );
    }

    #[test]
    fn parenthesized_arrow_on_the_left() {
        assert_eq!(
            parse_str("λx:(A->B)->C. x").unwrap(),
            Term::lambda(
                "x",
                Type::arrow(Type::arrow(base("A"), base("B")), base("C")),
                var("x"),
            ),
        );
    }

    #[test]
    fn abstraction_extends_as_far_right_as_possible() {
        assert_eq!(
            parse_str("λx:A. f x").unwrap(),
            Term::lambda("x", base("A"), Term::app(var("f"), var("x"))),
        );
    }

    #[test]
    fn parentheses_override_grouping() {
        assert_eq!(
            parse_str("(λx:A. x) y").unwrap(),
            Term::app(Term::lambda("x", base("A"), var("x")), var("y")),
        );
    }

    #[test]
    fn lambda_as_argument() {
        assert_eq!(
            parse_str("f λx:A. x").unwrap(),
            Term::app(var("f"), Term::lambda("x", base("A"), var("x"))),
        );
    }

    #[test]
    fn rejects_malformed_input() {
        for bad in ["λx. x", "λ:A. x", "λx:A x", "(x", "x)", "λx:A.", "", "λx:->A. x"] {
            assert!(parse_str(bad).is_err(), "should reject {bad:?}");
        }
    }

    #[test]
    fn print_then_parse_roundtrips() {
        let samples = [
            Term::lambda("x", base("Bool"), var("x")),
            Term::app(Term::app(var("f"), var("x")), var("y")),
            Term::app(var("f"), Term::app(var("x"), var("y"))),
            Term::lambda(
                "f",
                Type::arrow(base("A"), base("B")),
                Term::lambda("x", base("A"), Term::app(var("f"), var("x"))),
            ),
            Term::app(Term::lambda("x", base("A"), var("x")), var("y")),
        ];
        for t in samples {
            assert_eq!(parse_str(&t.to_string()).unwrap(), t, "printed: {t}");
        }
    }
}
