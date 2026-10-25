//! Lexer de `lambda`: uma tabela para o lexer genérico.
//!
//! Tokens: identificadores (uma letra ASCII seguida de letras, dígitos ou
//! `_`), `λ` ou `\`, `.` e parênteses. Não há palavras reservadas.

use crate::common::frontend::{lex, LexSpec, Words};
use crate::common::{Lexer, SourceLine};

use super::token::{LambdaToken, LambdaTokenType};

pub use crate::common::frontend::LexError;

use LambdaTokenType as T;

const SPEC: LexSpec<LambdaTokenType> = LexSpec {
    symbols: &[("λ", T::Lambda), ("\\", T::Lambda), (".", T::Dot), ("(", T::LParen), (")", T::RParen)],
    words: Words::Identifier(T::Identifier),
    ..LexSpec::EMPTY
};

pub struct LambdaLexer;

impl LambdaLexer {
    pub fn new() -> Self {
        Self
    }
}

impl Default for LambdaLexer {
    fn default() -> Self {
        Self::new()
    }
}

impl Lexer for LambdaLexer {
    type Token = LambdaToken;
    type Error = LexError;

    fn analyze(input: &[SourceLine]) -> Result<Vec<LambdaToken>, LexError> {
        lex(&SPEC, input)
    }
}
