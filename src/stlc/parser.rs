//! Parser da linguagem `stlc` (cálculo λ simplesmente tipado, com `Bool`).
//!
//! Gramática:
//!
//! ```text
//! term        ::= atom atom*                      (aplicação, assoc. à esquerda)
//! atom        ::= IDENT | "true" | "false" | "(" term ")" | abstraction | conditional
//! abstraction ::= "λ" IDENT ":" type "." term     (o corpo vai o mais à direita possível)
//! conditional ::= "if" term "then" term "else" term
//! type        ::= atype ("->" type)?              (assoc. à direita)
//! atype       ::= IDENT | "(" type ")"
//! ```

use crate::common::frontend::parse_complete;
use crate::common::Parser;

use super::terms::Term;
use super::token::{StlcToken, StlcTokenType};
use super::types::Type;

pub type TokenStream<'a> = crate::common::frontend::TokenStream<'a, StlcTokenType>;
pub type ParseError = crate::common::frontend::ParseError<StlcTokenType>;

// =============================================================================
// Parser
// =============================================================================

pub struct StlcParser;

impl StlcParser {
    pub fn new() -> Self {
        Self
    }

    /// term ::= atom atom*
    fn parse_term(stream: &mut TokenStream<'_>) -> Result<Term, ParseError> {
        let mut lhs = Self::parse_atom(stream)?;

        while matches!(
            stream.peek_kind(),
            Some(StlcTokenType::Identifier)
                | Some(StlcTokenType::LParen)
                | Some(StlcTokenType::Lambda)
                | Some(StlcTokenType::True)
                | Some(StlcTokenType::False)
                | Some(StlcTokenType::If)
        ) {
            let rhs = Self::parse_atom(stream)?;
            lhs = Term::app(lhs, rhs);
        }

        Ok(lhs)
    }

    /// atom ::= IDENT | "true" | "false" | "(" term ")" | abstraction | conditional
    fn parse_atom(stream: &mut TokenStream<'_>) -> Result<Term, ParseError> {
        match stream.peek_kind() {
            Some(StlcTokenType::Lambda) => return Self::parse_abstraction(stream),
            Some(StlcTokenType::If) => return Self::parse_conditional(stream),
            _ => {}
        }

        let token = stream.advance()?;

        match token.kind {
            StlcTokenType::Identifier => Ok(Term::variable(token.lexeme)),
            StlcTokenType::True => Ok(Term::boolean(true)),
            StlcTokenType::False => Ok(Term::boolean(false)),

            StlcTokenType::LParen => {
                let term = Self::parse_term(stream)?;
                stream.expect(StlcTokenType::RParen)?;
                Ok(term)
            }

            found => Err(ParseError::UnexpectedToken {
                expected: None,
                found,
                span: token.span,
            }),
        }
    }

    /// conditional ::= "if" term "then" term "else" term
    fn parse_conditional(stream: &mut TokenStream<'_>) -> Result<Term, ParseError> {
        stream.expect(StlcTokenType::If)?;
        let condition = Self::parse_term(stream)?;
        stream.expect(StlcTokenType::Then)?;
        let then_branch = Self::parse_term(stream)?;
        stream.expect(StlcTokenType::Else)?;
        let else_branch = Self::parse_term(stream)?;

        Ok(Term::if_then_else(condition, then_branch, else_branch))
    }

    /// abstraction ::= "λ" IDENT ":" type "." term
    fn parse_abstraction(stream: &mut TokenStream<'_>) -> Result<Term, ParseError> {
        stream.expect(StlcTokenType::Lambda)?;
        let param = stream.expect(StlcTokenType::Identifier)?;
        stream.expect(StlcTokenType::Colon)?;
        let ty = Self::parse_type(stream)?;
        stream.expect(StlcTokenType::Dot)?;
        let body = Self::parse_term(stream)?;

        Ok(Term::lambda(param.lexeme, ty, body))
    }

    /// type ::= atype ("->" type)?
    fn parse_type(stream: &mut TokenStream<'_>) -> Result<Type, ParseError> {
        let from = Self::parse_atomic_type(stream)?;

        if stream.peek_kind() == Some(StlcTokenType::Arrow) {
            stream.next();
            let to = Self::parse_type(stream)?; // associativo à direita
            return Ok(Type::arrow(from, to));
        }

        Ok(from)
    }

    /// atype ::= IDENT | "(" type ")"
    fn parse_atomic_type(stream: &mut TokenStream<'_>) -> Result<Type, ParseError> {
        let token = stream.advance()?;

        match token.kind {
            StlcTokenType::Identifier => Ok(Type::base(token.lexeme)),

            StlcTokenType::LParen => {
                let ty = Self::parse_type(stream)?;
                stream.expect(StlcTokenType::RParen)?;
                Ok(ty)
            }

            found => Err(ParseError::UnexpectedToken {
                expected: None,
                found,
                span: token.span,
            }),
        }
    }

    pub fn parse_tokens(&self, tokens: &[StlcToken]) -> Result<Term, ParseError> {
        parse_complete(tokens, Self::parse_term)
    }
}

impl Default for StlcParser {
    fn default() -> Self {
        Self::new()
    }
}

impl Parser for StlcParser {
    type Token = StlcToken;
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
    use crate::stlc::lexer::StlcLexer;
    use crate::stlc::scanner::StlcScanner;

    fn var(s: &str) -> Term {
        Term::variable(s)
    }

    fn base(s: &str) -> Type {
        Type::base(s)
    }

    fn parse_str(src: &str) -> Result<Term, ParseError> {
        let lines = StlcScanner::scan(src).unwrap();
        let tokens = StlcLexer::analyze(&lines).unwrap();
        StlcParser::parse(&tokens)
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
    fn boolean_literals_and_the_bool_type() {
        assert_eq!(parse_str("true").unwrap(), Term::boolean(true));
        assert_eq!(
            parse_str("λb:Bool. b").unwrap(),
            Term::lambda("b", Type::Bool, var("b")),
        );
    }

    #[test]
    fn conditional() {
        assert_eq!(
            parse_str("if a then b else c").unwrap(),
            Term::if_then_else(var("a"), var("b"), var("c")),
        );
    }

    #[test]
    fn conditional_branches_extend_as_far_as_possible() {
        // o ramo `else` engole a aplicação inteira
        assert_eq!(
            parse_str("if a then b else f x").unwrap(),
            Term::if_then_else(var("a"), var("b"), Term::app(var("f"), var("x"))),
        );
    }

    #[test]
    fn nested_conditionals_need_no_parentheses() {
        assert_eq!(
            parse_str("if a then if b then c else d else e").unwrap(),
            Term::if_then_else(
                var("a"),
                Term::if_then_else(var("b"), var("c"), var("d")),
                var("e"),
            ),
        );
    }

    #[test]
    fn booleans_and_conditionals_as_arguments() {
        assert_eq!(
            parse_str("f true (if a then b else c)").unwrap(),
            Term::app(
                Term::app(var("f"), Term::boolean(true)),
                Term::if_then_else(var("a"), var("b"), var("c")),
            ),
        );
    }

    #[test]
    fn rejects_malformed_input() {
        for bad in [
            "λx. x", "λ:A. x", "λx:A x", "(x", "x)", "λx:A.", "", "λx:->A. x",
            "if a then b", "if a else b", "if then b else c", "then", "else",
        ] {
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
            Term::boolean(true),
            Term::if_then_else(var("a"), Term::boolean(true), Term::boolean(false)),
            Term::app(
                Term::if_then_else(var("a"), var("f"), var("g")),
                Term::if_then_else(var("b"), var("x"), var("y")),
            ),
            Term::lambda(
                "b",
                Type::Bool,
                Term::if_then_else(var("b"), Term::boolean(false), Term::boolean(true)),
            ),
            Term::if_then_else(
                Term::if_then_else(var("a"), var("b"), var("c")),
                Term::lambda("x", base("A"), var("x")),
                var("y"),
            ),
        ];
        for t in samples {
            assert_eq!(parse_str(&t.to_string()).unwrap(), t, "printed: {t}");
        }
    }
}
