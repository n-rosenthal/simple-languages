//! Parser de `nameless_terms`.
//!
//! A sintaxe de entrada utiliza nomes, mas a representação produzida
//! utiliza índices de De Bruijn.
//!
//! ```text
//! term        ::= atom atom*
//! atom        ::= IDENT | "(" term ")" | abstraction
//! abstraction ::= "λ" IDENT "." term
//! ```
//!
//! Aplicações associam à esquerda, e o corpo de uma abstração se estende
//! o mais à direita possível.
//!
//! Somente termos fechados são aceitos. Identificadores que não correspondam
//! a nenhuma abstração no contexto lexical são rejeitados.
//!
//! Exemplos:
//!
//! ```text
//! \x. x       -> λ. 0
//! \x. \y. x   -> λ. λ. 1
//! \x. \y. y   -> λ. λ. 0
//! \x. x x     -> λ. (0 0)
//! ```
//!
//! Uma variável é convertida para o índice correspondente à distância
//! até sua abstração mais próxima. O índice 0 representa o binder mais
//! próximo, o índice 1 representa o próximo binder externo, etc.

use crate::common::frontend::{delimited, parse_complete};
use crate::common::Parser;

use super::terms::Term;
use super::token::{NamelessTermsToken, NamelessTermsTokenType};

use NamelessTermsTokenType as T;

pub type TokenStream<'a> =
    crate::common::frontend::TokenStream<'a, NamelessTermsTokenType>;

pub type ParseError =
    crate::common::frontend::ParseError<NamelessTermsTokenType>;

/// Os tokens que começam um átomo e, portanto, podem continuar uma aplicação.
const APPLICATION_STARTS: &[NamelessTermsTokenType] =
    &[T::Identifier, T::LParen, T::Lambda];

/// Parser de termos lambda na representação nameless.
///
/// O contexto contém os nomes das abstrações ativas, ordenados
/// da mais externa para a mais interna.
///
/// Os nomes existem somente durante o parsing: não são armazenados
/// na representação interna `Term`.
pub struct NamelessTermsParser;

impl NamelessTermsParser {
    pub fn new() -> Self {
        Self
    }

    /// Analisa um termo completo.
    ///
    /// A conversão para índices de De Bruijn acontece durante o parsing.
    /// Variáveis livres são rejeitadas.
    pub fn parse_tokens(
        &self,
        tokens: &[NamelessTermsToken],
    ) -> Result<Term, ParseError> {
        let mut context = Vec::new();

        parse_complete(tokens, |stream| {
            Self::parse_term(stream, &mut context)
        })
    }

    /// term ::= atom atom*
    ///
    /// Implementa aplicações associativas à esquerda.
    fn parse_term(
        stream: &mut TokenStream<'_>,
        context: &mut Vec<String>,
    ) -> Result<Term, ParseError> {
        let mut term = Self::parse_atom(stream, context)?;

        while matches!(stream.peek_kind(), Some(kind) if APPLICATION_STARTS.contains(&kind))
        {
            let argument = Self::parse_atom(stream, context)?;
            term = Term::app(term, argument);
        }

        Ok(term)
    }

    /// Analisa um átomo.
    ///
    /// A abstração adiciona temporariamente seu parâmetro ao contexto.
    /// Depois de analisar o corpo, remove esse parâmetro, mesmo quando
    /// a análise falha.
    fn parse_atom(
        stream: &mut TokenStream<'_>,
        context: &mut Vec<String>,
    ) -> Result<Term, ParseError> {
        match stream.peek_kind() {
            Some(T::Lambda) => {
                stream.next();

                let parameter = stream.expect(T::Identifier)?;
                stream.expect(T::Dot)?;

                context.push(parameter.lexeme.to_string());

                let body_result = Self::parse_term(stream, context);

                context.pop();

                let body = body_result?;

                Ok(Term::abs(body))
            }

            Some(T::LParen) => {
                delimited(stream, T::LParen, T::RParen, |stream| {
                    Self::parse_term(stream, context)
                })
            }

            Some(T::Identifier) => {
                let token = stream.advance()?;
                let name = token.lexeme;

                match Self::resolve_variable(context, name.as_ref()) {
                    Some(index) => Ok(Term::var(index)),
                    None => {
                        // O identificador não possui um binder no contexto.
                        // O parser compartilhado fornece o erro sintático
                        // disponível para essa situação.
                        Err(stream.unexpected())
                    }
                }
            }

            _ => Err(stream.unexpected()),
        }
    }

    /// Resolve um identificador para seu índice de De Bruijn.
    ///
    /// O contexto está ordenado do binder mais externo para o mais interno.
    /// A busca reversa encontra a abstração mais próxima, respeitando
    /// sombreamento de identificadores.
    ///
    /// Retorna `None` para variáveis livres.
    fn resolve_variable(
        context: &[String],
        name: &str,
    ) -> Option<usize> {
        context
            .iter()
            .rev()
            .position(|bound_name| bound_name == name)
    }
}

impl Default for NamelessTermsParser {
    fn default() -> Self {
        Self::new()
    }
}

impl Parser for NamelessTermsParser {
    type Token = NamelessTermsToken;
    type Term = Term;
    type Error = ParseError;

    fn parse(input: &[Self::Token]) -> Result<Self::Term, Self::Error> {
        Self::new().parse_tokens(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::{Lexer, SourceLine};
    use crate::nameless_terms::lexer::NamelessTermsLexer;

    fn parse(input: &str) -> Result<Term, ParseError> {
        let lines = vec![SourceLine::new(input)];
        let tokens = NamelessTermsLexer::analyze(&lines)?;
        NamelessTermsParser::new().parse_tokens(&tokens)
    }

    #[test]
    fn parses_identity() {
        let term = parse(r"\x. x").unwrap();

        assert_eq!(term, Term::abs(Term::var(0)));
        assert!(term.is_closed());
    }

    #[test]
    fn parses_outer_variable() {
        let term = parse(r"\x. \y. x").unwrap();

        assert_eq!(
            term,
            Term::abs(Term::abs(Term::var(1)))
        );
        assert!(term.is_closed());
    }

    #[test]
    fn parses_inner_variable() {
        let term = parse(r"\x. \y. y").unwrap();

        assert_eq!(
            term,
            Term::abs(Term::abs(Term::var(0)))
        );
        assert!(term.is_closed());
    }

    #[test]
    fn parses_application() {
        let term = parse(r"\x. x x").unwrap();

        assert_eq!(
            term,
            Term::abs(
                Term::app(Term::var(0), Term::var(0))
            )
        );
    }

    #[test]
    fn application_associates_to_the_left() {
        let term = parse(r"\x. \y. \z. x y z").unwrap();

        let expected = Term::abs(Term::abs(Term::abs(
            Term::app(
                Term::app(Term::var(2), Term::var(1)),
                Term::var(0),
            ),
        )));

        assert_eq!(term, expected);
    }

    #[test]
    fn parses_shadowed_identifiers() {
        let term = parse(r"\x. \x. x").unwrap();

        assert_eq!(
            term,
            Term::abs(Term::abs(Term::var(0)))
        );
    }

    #[test]
    fn parses_parenthesized_application() {
        let term = parse(r"(\x. x) (\y. y)").unwrap();

        assert_eq!(
            term,
            Term::app(
                Term::abs(Term::var(0)),
                Term::abs(Term::var(0)),
            )
        );
    }

    #[test]
    fn rejects_free_variable() {
        assert!(parse("x").is_err());
    }

    #[test]
    fn rejects_free_variable_inside_abstraction() {
        assert!(parse(r"\x. y").is_err());
    }

    #[test]
    fn rejects_unclosed_term_even_with_bound_variables() {
        assert!(parse(r"\x. x y").is_err());
    }
}
