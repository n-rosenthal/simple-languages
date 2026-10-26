//! Lexer para o cálculo lambda com índices de De Bruijn.
//!
//! A sintaxe de entrada ainda utiliza identificadores:
//!
//! - `λx. x`
//! - `\x. x`
//! - `λx. λy. x y`
//! - `(λx. x) (λy. y)`
//!
//! O parser transforma os identificadores em índices de De Bruijn.
//! Este módulo apenas reconhece os tokens.

use crate::common::frontend::{self, LexError, LexSpec, Token, Words};
use crate::common::SourceLine;

use super::token::NamelessTermsTokenType;

/// Especificação léxica de `nameless_terms`.
///
/// O lexer compartilhado cuida do reconhecimento dos símbolos,
/// das palavras, dos espaços em branco e das posições dos tokens.
pub const SPEC: LexSpec<NamelessTermsTokenType> = LexSpec {
    keywords: &[],

    symbols: &[
        ("λ", NamelessTermsTokenType::Lambda),
        ("\\", NamelessTermsTokenType::Lambda),
        (".", NamelessTermsTokenType::Dot),
        ("(", NamelessTermsTokenType::LParen),
        (")", NamelessTermsTokenType::RParen),
    ],

    integer: None,

    words: Words::Identifier(NamelessTermsTokenType::Identifier),

    line_comment: None,
    block_comment: None,

    ..LexSpec::EMPTY
};

/// Lexer da linguagem `nameless_terms`.
pub struct NamelessTermsLexer;

impl NamelessTermsLexer {
    /// Analisa as linhas de entrada e retorna os tokens.
    pub fn lex(
        input: &[SourceLine],
    ) -> Result<Vec<Token<NamelessTermsTokenType>>, LexError> {
        frontend::lex(&SPEC, input)
    }
}