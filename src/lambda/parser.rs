//! Parser de `lambda`.
//!
//! ```text
//! term        ::= atom atom*                (aplicação, assoc. à esquerda)
//! atom        ::= IDENT | "(" term ")" | abstraction
//! abstraction ::= "λ" IDENT "." term        (o corpo vai o mais à direita possível)
//! ```

use crate::common::frontend::{delimited, left_chain, parse_complete};
use crate::common::Parser;

use super::terms::Term;
use super::token::{LambdaToken, LambdaTokenType};

use LambdaTokenType as T;

pub type TokenStream<'a> = crate::common::frontend::TokenStream<'a, LambdaTokenType>;
pub type ParseError = crate::common::frontend::ParseError<LambdaTokenType>;

/// Os tokens que começam um átomo, e portanto continuam uma aplicação.
const APPLICATION_STARTS: &[LambdaTokenType] = &[T::Identifier, T::LParen, T::Lambda];

pub struct LambdaParser;

impl LambdaParser {
    pub fn new() -> Self {
        Self
    }

    /// term ::= atom atom*
    fn parse_term(stream: &mut TokenStream<'_>) -> Result<Term, ParseError> {
        left_chain(stream, APPLICATION_STARTS, &Self::parse_atom, &Term::app)
    }

    fn parse_atom(stream: &mut TokenStream<'_>) -> Result<Term, ParseError> {
        match stream.peek_kind() {
            Some(T::Lambda) => {
                stream.next();
                let param = stream.expect(T::Identifier)?;
                stream.expect(T::Dot)?;
                let body = Self::parse_term(stream)?;
                Ok(Term::lambda(param.lexeme, body))
            }
            Some(T::LParen) => delimited(stream, T::LParen, T::RParen, Self::parse_term),
            Some(T::Identifier) => Ok(Term::variable(stream.advance()?.lexeme)),
            _ => Err(stream.unexpected()),
        }
    }

    pub fn parse_tokens(&self, tokens: &[LambdaToken]) -> Result<Term, ParseError> {
        parse_complete(tokens, Self::parse_term)
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
