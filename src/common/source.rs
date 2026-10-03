//! Front-end: posições no fonte e os traits de cada fase.
//!
//! `Scanner` divide o texto em linhas, `Lexer` produz tokens e `Parser`
//! produz termos. Cada linguagem implementa os três.
//!
//! O scanner é igual para todas as linguagens ([`LineScanner`]).

use std::fmt;

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

// =============================================================================
// Scanner compartilhado
// =============================================================================

/// Erros produzidos pelo [`LineScanner`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScanError {
    UnexpectedControlCharacter { character: char, line: usize },
}

impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedControlCharacter { character, line } => write!(
                f,
                "unexpected control character {:?} on line {line}",
                character
            ),
        }
    }
}

impl std::error::Error for ScanError {}

/// Divide o texto em linhas numeradas a partir de 1. Linhas em branco são
/// mantidas (a numeração acompanha o arquivo) e caracteres de controle,
/// exceto tabulação, são rejeitados.
pub struct LineScanner;

impl LineScanner {
    pub fn new() -> Self {
        Self
    }
}

impl Default for LineScanner {
    fn default() -> Self {
        Self::new()
    }
}

impl Scanner for LineScanner {
    type Error = ScanError;

    fn scan(input: &str) -> Result<Vec<SourceLine>, ScanError> {
        let mut lines = Vec::new();

        // `str::lines` divide em `\n` e remove um `\r` final, então
        // terminações Unix e Windows funcionam.
        for (index, text) in input.lines().enumerate() {
            let number = index + 1;

            if let Some(character) = text.chars().find(|c| c.is_control() && *c != '\t') {
                return Err(ScanError::UnexpectedControlCharacter {
                    character,
                    line: number,
                });
            }

            lines.push(SourceLine { number, text: text.to_string() });
        }

        Ok(lines)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scans_lines_with_one_based_numbers() {
        let lines = LineScanner::scan("λx.x\n(y)").unwrap();
        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0].number, 1);
        assert_eq!(lines[1].number, 2);
        assert_eq!(lines[0].text, "λx.x");
    }

    #[test]
    fn handles_crlf() {
        let lines = LineScanner::scan("a\r\nb").unwrap();
        assert_eq!(lines[0].text, "a");
        assert_eq!(lines[1].text, "b");
    }

    #[test]
    fn keeps_blank_lines_so_numbering_stays_faithful() {
        let lines = LineScanner::scan("a\n\nb").unwrap();
        assert_eq!(lines.len(), 3);
        assert_eq!(lines[2].number, 3);
    }

    #[test]
    fn empty_input_has_no_lines() {
        assert!(LineScanner::scan("").unwrap().is_empty());
    }

    #[test]
    fn rejects_control_characters() {
        assert!(LineScanner::scan("a\u{0007}b").is_err());
    }
}
