//! Parser de `arith-extensions`: descida recursiva com uma tabela de precedência.
//!
//! ```text
//! term        ::= "if" term "then" term "else" term | binary(0)
//! binary(n)   ::= binary(n+1) (OP_n binary(n+1))*          (assoc. à esquerda)
//! primary     ::= INTEGER | BOOLEAN | "(" term ")"
//! ```
//!
//! Níveis de precedência, do mais fraco para o mais forte:
//! `||`, `&&`, `==`, `<`, `+ -`, `*`, `/`, `%`. Um `if` como operando precisa de
//! parênteses.

use crate::common::frontend::{parse_binary, parse_complete, parse_integer};
use crate::common::Parser;

use super::terms::{BinaryOp, Term};
use super::token::{ArithToken, ArithTokenType};

use ArithTokenType as T;

pub type TokenStream<'a> = crate::common::frontend::TokenStream<'a, ArithTokenType>;
pub type ParseError = crate::common::frontend::ParseError<ArithTokenType>;

const LEVELS: &[&[(ArithTokenType, BinaryOp)]] = &[
    &[(T::Or, BinaryOp::Or)],
    &[(T::And, BinaryOp::And)],
    &[(T::Equal, BinaryOp::Equal)],
    &[(T::LessThan, BinaryOp::LessThan)],
    &[(T::Plus, BinaryOp::Add), (T::Minus, BinaryOp::Sub)],
    &[(T::Slash, BinaryOp::Div), (T::Percent, BinaryOp::Mod)],
    &[(T::Star, BinaryOp::Mul)],
];

pub struct ArithParser;

impl ArithParser {
    pub fn new() -> Self {
        Self
    }

    /// term ::= "if" term "then" term "else" term | binary(0)
    fn parse_term(stream: &mut TokenStream<'_>) -> Result<Term, ParseError> {
        if stream.peek_kind() != Some(T::If) {
            return parse_binary(stream, LEVELS, &Self::parse_primary, &Term::binary);
        }

        stream.next();
        let condition = Self::parse_term(stream)?;
        stream.expect(T::Then)?;
        let then_branch = Self::parse_term(stream)?;
        stream.expect(T::Else)?;
        let else_branch = Self::parse_term(stream)?;

        Ok(Term::if_then_else(condition, then_branch, else_branch))
    }

    /// primary ::= INTEGER | BOOLEAN | "(" term ")"
    fn parse_primary(stream: &mut TokenStream<'_>) -> Result<Term, ParseError> {
        let token = stream.advance()?;

        match token.kind {
            T::Integer => parse_integer(&token).map(Term::integer),

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
        parse_complete(tokens, Self::parse_term)
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
    use crate::arith_extensions::{ArithLexer, ArithScanner};
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
