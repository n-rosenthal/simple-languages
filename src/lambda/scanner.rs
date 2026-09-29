use std::fmt;

use crate::common::{Scanner, SourceLine};

/// Errors produced by the scanner.
///
/// Splitting a string into lines cannot fail today, but the `Scanner`
/// trait returns a `Result`, and this leaves room for future checks
/// (e.g. rejecting control characters).
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

pub struct LambdaScanner;

impl LambdaScanner {
    pub fn new() -> Self {
        Self
    }
}

impl Default for LambdaScanner {
    fn default() -> Self {
        Self::new()
    }
}

impl Scanner for LambdaScanner {
    type Error = ScanError;

    fn scan(input: &str) -> Result<Vec<SourceLine>, Self::Error> {
        let mut lines = Vec::new();

        // `str::lines` splits on `\n` and strips a trailing `\r`,
        // so both Unix and Windows line endings work.
        for (index, text) in input.lines().enumerate() {
            let number = index + 1; // 1-based, as in the arith tests

            if let Some(character) = text
                .chars()
                .find(|c| c.is_control() && *c != '\t')
            {
                return Err(ScanError::UnexpectedControlCharacter {
                    character,
                    line: number,
                });
            }

            lines.push(SourceLine {
                number,
                text: text.to_string(),
            });
        }

        Ok(lines)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scans_lines_with_one_based_numbers() {
        let lines = LambdaScanner::scan("λx.x\n(y)").unwrap();
        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0].number, 1);
        assert_eq!(lines[1].number, 2);
        assert_eq!(lines[0].text, "λx.x");
    }

    #[test]
    fn handles_crlf() {
        let lines = LambdaScanner::scan("a\r\nb").unwrap();
        assert_eq!(lines[0].text, "a");
        assert_eq!(lines[1].text, "b");
    }

    #[test]
    fn keeps_blank_lines_so_numbering_stays_faithful() {
        let lines = LambdaScanner::scan("a\n\nb").unwrap();
        assert_eq!(lines.len(), 3);
        assert_eq!(lines[2].number, 3);
    }

    #[test]
    fn empty_input_has_no_lines() {
        assert!(LambdaScanner::scan("").unwrap().is_empty());
    }

    #[test]
    fn rejects_control_characters() {
        assert!(LambdaScanner::scan("a\u{0007}b").is_err());
    }
}   