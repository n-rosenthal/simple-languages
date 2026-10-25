//! Lexer de `typed-arith`: uma tabela para o lexer genérico.
//!
//! Tokens: numerais, `true`/`false`, `if`/`then`/`else`, `succ`/`pred`/`iszero`
//! e parênteses. Qualquer outra palavra é um erro: a linguagem não tem variáveis.

use crate::common::frontend::{lex, LexSpec, Words};
use crate::common::{Lexer, SourceLine};

use super::token::{TypedArithToken, TypedArithTokenType};

pub use crate::common::frontend::LexError;

use TypedArithTokenType as T;

const SPEC: LexSpec<TypedArithTokenType> = LexSpec {
    keywords: &[
        ("true", T::True),
        ("false", T::False),
        ("if", T::If),
        ("then", T::Then),
        ("else", T::Else),
        ("succ", T::Succ),
        ("pred", T::Pred),
        ("iszero", T::IsZero),
    ],
    symbols: &[("(", T::LParen), (")", T::RParen)],
    integer: Some(T::Integer),
    words: Words::Reject("typed-arith has no variables"),
    ..LexSpec::EMPTY
};

pub struct TypedArithLexer;

impl TypedArithLexer {
    pub fn new() -> Self {
        Self
    }
}

impl Default for TypedArithLexer {
    fn default() -> Self {
        Self::new()
    }
}

impl Lexer for TypedArithLexer {
    type Token = TypedArithToken;
    type Error = LexError;

    fn analyze(input: &[SourceLine]) -> Result<Vec<TypedArithToken>, LexError> {
        lex(&SPEC, input)
    }
}
