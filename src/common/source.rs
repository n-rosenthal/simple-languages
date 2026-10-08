//! Front-end: posições no fonte e os traits de cada fase.
//!
//! `Scanner` divide o texto em linhas, `Lexer` produz tokens e `Parser`
//! produz termos. Cada linguagem implementa os três.
//!
//! O scanner é igual para todas as linguagens ([`LineScanner`]).

use std::fmt;

/// Um intervalo `start..end` em `char`s no texto-fonte *inteiro* (não por
/// linha): ele continua valendo em programas de várias linhas. O
/// [`crate::common::diagnostic`] o converte em linha e coluna.
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
    /// A posição, em `char`s, do começo da linha no texto inteiro.
    pub offset: usize,
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
    UnexpectedControlCharacter { character: char, line: usize, span: Span },
}

impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedControlCharacter { character, line, .. } => write!(
                f,
                "unexpected control character {:?} on line {line}",
                character
            ),
        }
    }
}

impl std::error::Error for ScanError {}

impl crate::common::diagnostic::Diagnostic for ScanError {
    fn message(&self) -> String {
        match self {
            Self::UnexpectedControlCharacter { character, .. } => {
                format!("unexpected control character {character:?}")
            }
        }
    }

    fn position(&self) -> crate::common::diagnostic::Position {
        match self {
            Self::UnexpectedControlCharacter { span, .. } => {
                crate::common::diagnostic::Position::Span(*span)
            }
        }
    }
}

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
        let mut offset = 0;

        // `split_inclusive` guarda o terminador, o que permite contar os
        // `char`s de cada linha (inclusive `\r\n`) e dar a posição exata de
        // cada uma no texto inteiro.
        for (index, raw) in input.split_inclusive('\n').enumerate() {
            let number = index + 1;
            let text = raw.strip_suffix('\n').unwrap_or(raw);
            let text = text.strip_suffix('\r').unwrap_or(text);

            if let Some((column, character)) = text
                .chars()
                .enumerate()
                .find(|(_, c)| c.is_control() && *c != '\t')
            {
                return Err(ScanError::UnexpectedControlCharacter {
                    character,
                    line: number,
                    span: Span::new(offset + column, offset + column + 1),
                });
            }

            lines.push(SourceLine { number, text: text.to_string(), offset });
            offset += raw.chars().count();
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
