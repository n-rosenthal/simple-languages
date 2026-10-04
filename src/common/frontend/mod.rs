//! O front-end genérico: do texto ao termo.
//!
//! ```text
//!   &str ──Scanner──▶ [SourceLine] ──lex(LexSpec)──▶ [Token<K>] ──Parser──▶ Term
//! ```
//!
//! Uma linguagem nova escreve só o que é dela:
//!
//! 1. uma enum `K` com os tipos de token e uma [`LexSpec`] (uma tabela);
//! 2. as funções da gramática, sobre um [`TokenStream`] (e, para operadores
//!    binários, [`parse_binary`] com uma tabela de precedência);
//! 3. `impl Lexer` (uma linha, chamando [`lex`]) e `impl Parser` (uma linha,
//!    chamando [`parse_complete`]).
//!
//! O scanner ([`crate::common::source::LineScanner`]), os erros e a junção
//! das três fases ([`parse_source`]) são compartilhados.

mod lexer;
mod parser;

use std::fmt;

use crate::common::{Lexer, Parser, Scanner};

pub use lexer::{lex, LexError, LexSpec, Token, Words};
pub use parser::{parse_binary, parse_complete, parse_integer, ParseError, TokenStream};

/// O erro de qualquer fase do front-end.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FrontendError<S, L, P> {
    Scan(S),
    Lex(L),
    Parse(P),
}

impl<S: fmt::Display, L: fmt::Display, P: fmt::Display> fmt::Display for FrontendError<S, L, P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Scan(e) => write!(f, "{e}"),
            Self::Lex(e) => write!(f, "{e}"),
            Self::Parse(e) => write!(f, "{e}"),
        }
    }
}

impl<S, L, P> std::error::Error for FrontendError<S, L, P>
where
    S: fmt::Debug + fmt::Display,
    L: fmt::Debug + fmt::Display,
    P: fmt::Debug + fmt::Display,
{
}

/// Texto → termo: roda o scanner, o lexer e o parser de uma linguagem.
pub fn parse_source<Sc, Lx, Pr>(
    source: &str,
) -> Result<Pr::Term, FrontendError<Sc::Error, Lx::Error, Pr::Error>>
where
    Sc: Scanner,
    Lx: Lexer,
    Pr: Parser<Token = Lx::Token>,
{
    let lines = Sc::scan(source).map_err(FrontendError::Scan)?;
    let tokens = Lx::analyze(&lines).map_err(FrontendError::Lex)?;
    Pr::parse(&tokens).map_err(FrontendError::Parse)
}
