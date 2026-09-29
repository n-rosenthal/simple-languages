//! `simple-languages/common/lexer.rs` defines a lexer for the lambda calculus language.
//! 
//! author:  n-rosenthal
//! date:    2026-09-28
//! version: 0.1.0
//! 
//! This module provides a lexer for the lambda calculus language, which is responsible for converting a string of source code into a sequence of tokens that can be further processed by a parser. The lexer recognizes the following tokens:
//! (1.) variables (identifiers): ASCII strings that start with a letter and can contain letters, digits, and underscores;
//! (2.) lambda abstraction: the keyword "λ";
//! (3.) dot: the character ".";
//! (4.) parentheses: the characters "(" and ")";
//! (5.) whitespace: spaces, tabs, and newlines, which are ignored by the lexer.
//! 
//! The lexer also returns errors when it encounters unexpected characters or missing tokens.
//!

use std::fmt;

use crate::common::{Lexer, SourceLine, Span};

use super::token::{LambdaToken, LambdaTokenType};
use super::scanner::LambdaScanner;

/// Errors produced by the lexer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    UnexpectedCharacter { character: char, span: Span },
    /// A multi-character operator that was started but not finished (`-` without `>`).
    UnexpectedEndOfOperator { span: Span },
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedCharacter { character, span } => write!(
                f,
                "unexpected character `{character}` at {}..{}",
                span.start, span.end
            ),
            Self::UnexpectedEndOfOperator { span } => write!(
                f,
                "unexpected end of operator at {}..{}",
                span.start, span.end
            ),
        }
    }
}

impl std::error::Error for LexError {}

pub struct LambdaLexer;

impl LambdaLexer {
    pub fn new() -> Self {
        Self
    }

    fn is_identifier_start(c: char) -> bool {
        c.is_ascii_alphabetic()
    }

    fn is_identifier_continue(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }

    fn tokenize_line(
        line: &SourceLine,
    ) -> Result<Vec<LambdaToken>, LexError> {
        let chars: Vec<char> = line.text.chars().collect();

        let mut tokens = Vec::new();
        let mut index = 0;

        while index < chars.len() {
            let character = chars[index];

            // Whitespace
            if character.is_whitespace() {
                index += 1;
                continue;
            }

            let start = index;

            // Identifier: letter (letter | digit | '_')*
            if Self::is_identifier_start(character) {
                index += 1;
                while index < chars.len()
                    && Self::is_identifier_continue(chars[index])
                {
                    index += 1;
                }

                let lexeme: String = chars[start..index].iter().collect();
                tokens.push(LambdaToken::new(
                    LambdaTokenType::Identifier,
                    lexeme,
                    Span::new(start, index),
                ));
                continue;
            }

            // Arrow: '->'  (a lone '-' is an unfinished operator)
            if character == '-' {
                if index + 1 < chars.len() && chars[index + 1] == '>' {
                    index += 2;
                    tokens.push(LambdaToken::new(
                        LambdaTokenType::Arrow,
                        "->",
                        Span::new(start, index),
                    ));
                    continue;
                }

                return Err(LexError::UnexpectedEndOfOperator {
                    span: Span::new(start, start + 1),
                });
            }

            // Single-character tokens
            let kind = match character {
                'λ' | '\\' => Some(LambdaTokenType::Lambda),
                '.' => Some(LambdaTokenType::Dot),
                ':' => Some(LambdaTokenType::Colon),
                '(' => Some(LambdaTokenType::LeftParen),
                ')' => Some(LambdaTokenType::RightParen),
                _ => None,
            };

            if let Some(kind) = kind {
                index += 1;
                tokens.push(LambdaToken::new(
                    kind,
                    character.to_string(),
                    Span::new(start, index),
                ));
                continue;
            }

            // Unknown character
            return Err(LexError::UnexpectedCharacter {
                character,
                span: Span::new(start, start + 1),
            });
        }

        Ok(tokens)
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

    fn analyze(input: &[SourceLine]) -> Result<Vec<Self::Token>, Self::Error> {
        let mut tokens = Vec::new();
        for line in input {
            tokens.extend(Self::tokenize_line(line)?);
        }
        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::Scanner;
    // assumes a LambdaScanner analogous to ArithScanner

    fn kinds(source: &str) -> Vec<LambdaTokenType> {
        let lines = LambdaScanner::scan(source).unwrap();
        LambdaLexer::analyze(&lines)
            .unwrap()
            .iter()
            .map(|t| t.kind)
            .collect()
    }

    #[test]
    fn lexes_untyped_abstraction_without_spaces() {
        use LambdaTokenType::*;
        assert_eq!(kinds("λx.x"), vec![Lambda, Identifier, Dot, Identifier]);
    }

    #[test]
    fn lexes_typed_abstraction() {
        use LambdaTokenType::*;
        assert_eq!(
            kinds("λx:Bool->Bool. (f x)"),
            vec![
                Lambda, Identifier, Colon, Identifier, Arrow, Identifier,
                Dot, LeftParen, Identifier, Identifier, RightParen,
            ]
        );
    }

    #[test]
    fn accepts_backslash_as_lambda() {
        assert_eq!(kinds("\\x.x")[0], LambdaTokenType::Lambda);
    }

    #[test]
    fn lone_minus_is_an_error() {
        let lines = LambdaScanner::scan("Bool - Bool").unwrap();
        assert!(matches!(
            LambdaLexer::analyze(&lines),
            Err(LexError::UnexpectedEndOfOperator { .. })
        ));
    }

    #[test]
    fn rejects_unknown_character() {
        let lines = LambdaScanner::scan("λx#.x").unwrap();
        assert!(matches!(
            LambdaLexer::analyze(&lines),
            Err(LexError::UnexpectedCharacter { character: '#', .. })
        ));
    }
}