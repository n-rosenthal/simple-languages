//! `lambda` como instância de [`Language`].

use std::fmt;

use crate::common::language::Language;
use crate::common::{Lexer, Parser, Scanner};

use super::big_step::LambdaBigStep;
use super::compile::LambdaCompiler;
use super::lexer::{LambdaLexer, LexError};
use super::parser::{LambdaParser, ParseError};
use super::scanner::{LambdaScanner, ScanError};
use super::small_step::LambdaSmallStep;
use super::terms::Term;
use super::typing::LambdaTyping;
use super::types::Type;

/// O erro de qualquer estágio do front-end.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SyntaxError {
    Scan(ScanError),
    Lex(LexError),
    Parse(ParseError),
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Scan(e) => write!(f, "{e}"),
            Self::Lex(e) => write!(f, "{e}"),
            Self::Parse(e) => write!(f, "{e}"),
        }
    }
}

impl std::error::Error for SyntaxError {}

impl From<ScanError> for SyntaxError {
    fn from(e: ScanError) -> Self {
        Self::Scan(e)
    }
}

impl From<LexError> for SyntaxError {
    fn from(e: LexError) -> Self {
        Self::Lex(e)
    }
}

impl From<ParseError> for SyntaxError {
    fn from(e: ParseError) -> Self {
        Self::Parse(e)
    }
}

/// O cálculo λ simplesmente tipado (TAPL, caps. 5 e 9), call-by-value.
pub struct Lambda;

impl Language for Lambda {
    const NAME: &'static str = "lambda";

    type Term = Term;
    type Type = Type;
    type Value = Term;

    type SyntaxError = SyntaxError;

    type Typing = LambdaTyping;
    type Small = LambdaSmallStep;
    type Big = LambdaBigStep;
    type Compiler = LambdaCompiler;

    fn parse(source: &str) -> Result<Term, SyntaxError> {
        let lines = LambdaScanner::scan(source)?;
        let tokens = LambdaLexer::analyze(&lines)?;
        Ok(LambdaParser::parse(&tokens)?)
    }
}
