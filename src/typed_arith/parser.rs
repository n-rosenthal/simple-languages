//! Parser de `typed-arith`.
//!
//! ```text
//! term    ::= prefix* atom                 (prefix: succ | pred | iszero)
//! atom    ::= NUMERAL | "true" | "false" | "(" term ")"
//!           | "if" term "then" term "else" term
//! ```
//!
//! Um numeral `n` é açúcar para `succ^n 0`. Os operadores prefixos se
//! aplicam a um átomo: `succ succ 0` é `succ (succ 0)`.

use crate::common::frontend::{delimited, parse_complete, parse_integer, parse_prefix, ParseError as Error};
use crate::common::Parser;

use super::terms::Term;
use super::token::{TypedArithToken, TypedArithTokenType};

use TypedArithTokenType as T;

pub type TokenStream<'a> = crate::common::frontend::TokenStream<'a, TypedArithTokenType>;
pub type ParseError = Error<TypedArithTokenType>;

/// O maior numeral aceito: `n` vira `n` nós `succ`, e termos muito
/// profundos estouram a pilha de qualquer avaliador recursivo.
pub const MAX_NUMERAL: i64 = 100;

#[derive(Debug, Clone, Copy)]
enum Prefix {
    Succ,
    Pred,
    IsZero,
}

const PREFIXES: &[(TypedArithTokenType, Prefix)] =
    &[(T::Succ, Prefix::Succ), (T::Pred, Prefix::Pred), (T::IsZero, Prefix::IsZero)];

pub struct TypedArithParser;

impl TypedArithParser {
    pub fn new() -> Self {
        Self
    }

    /// term ::= prefix* atom
    fn parse_term(stream: &mut TokenStream<'_>) -> Result<Term, ParseError> {
        parse_prefix(stream, PREFIXES, &Self::parse_atom, &|op, term| match op {
            Prefix::Succ => Term::succ(term),
            Prefix::Pred => Term::pred(term),
            Prefix::IsZero => Term::is_zero(term),
        })
    }

    fn parse_atom(stream: &mut TokenStream<'_>) -> Result<Term, ParseError> {
        match stream.peek_kind() {
            Some(T::Integer) => {
                let token = stream.advance()?;
                let n = parse_integer(&token)?;
                if n > MAX_NUMERAL {
                    return Err(ParseError::IntegerTooLarge {
                        lexeme: token.lexeme,
                        span: token.span,
                        max: MAX_NUMERAL,
                    });
                }
                Ok(Term::numeral(n as usize))
            }
            Some(T::True) => stream.advance().map(|_| Term::True),
            Some(T::False) => stream.advance().map(|_| Term::False),
            Some(T::If) => {
                stream.next();
                let condition = Self::parse_term(stream)?;
                stream.expect(T::Then)?;
                let then_branch = Self::parse_term(stream)?;
                stream.expect(T::Else)?;
                let else_branch = Self::parse_term(stream)?;
                Ok(Term::if_then_else(condition, then_branch, else_branch))
            }
            Some(T::LParen) => delimited(stream, T::LParen, T::RParen, Self::parse_term),
            _ => Err(stream.unexpected()),
        }
    }

    pub fn parse_tokens(&self, tokens: &[TypedArithToken]) -> Result<Term, ParseError> {
        parse_complete(tokens, Self::parse_term)
    }
}

impl Default for TypedArithParser {
    fn default() -> Self {
        Self::new()
    }
}

impl Parser for TypedArithParser {
    type Token = TypedArithToken;
    type Term = Term;
    type Error = ParseError;

    fn parse(input: &[Self::Token]) -> Result<Self::Term, Self::Error> {
        Self::new().parse_tokens(input)
    }
}
