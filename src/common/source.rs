//! Front-end: posições no fonte e os traits de cada fase.
//!
//! `Scanner` divide o texto em linhas, `Lexer` produz tokens e `Parser`
//! produz termos. Cada linguagem implementa os três.

/// Um intervalo `start..end` em `char`s dentro de uma linha.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

/// Uma linha do fonte. `number` começa em 1.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceLine {
    pub number: usize,
    pub text: String,
}

pub trait Scanner {
    type Error: std::error::Error;

    fn scan(input: &str) -> Result<Vec<SourceLine>, Self::Error>;
}

pub trait Lexer {
    type Token;
    type Error: std::error::Error;

    fn analyze(input: &[SourceLine]) -> Result<Vec<Self::Token>, Self::Error>;
}

pub trait Parser {
    type Token;
    type Term;
    type Error: std::error::Error;

    fn parse(input: &[Self::Token]) -> Result<Self::Term, Self::Error>;
}