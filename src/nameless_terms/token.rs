//! Tokens de `nameless_terms`.
//!
//! A sintaxe de entrada utiliza identificadores para expressar abstrações
//! lambda. O parser converte os termos nomeados para a representação interna
//! nameless, baseada em índices de De Bruijn.

use crate::common::frontend::Token;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NamelessTermsTokenType {
    Lambda,
    Dot,
    LParen,
    RParen,
    Identifier,
}

pub type NamelessTermsToken = Token<NamelessTermsTokenType>;