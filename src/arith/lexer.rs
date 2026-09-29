use std::fmt;

use crate::common::{
    Lexer,
    SourceLine,
    Span,
};

use super::token::{
    ArithToken,
    ArithTokenType,
};

/// Erros produzidos pelo lexer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    UnexpectedCharacter {
        character: char,
        span: Span,
    },

    UnexpectedEndOfOperator {
        span: Span,
    },
}

impl fmt::Display for LexError {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        match self {
            Self::UnexpectedCharacter {
                character,
                span,
            } => {
                write!(
                    f,
                    "unexpected character `{character}` at \
                     {}..{}",
                    span.start,
                    span.end
                )
            }

            Self::UnexpectedEndOfOperator { span } => {
                write!(
                    f,
                    "unexpected end of operator at \
                     {}..{}",
                    span.start,
                    span.end
                )
            }
        }
    }
}

impl std::error::Error for LexError {}

pub struct ArithLexer;

impl ArithLexer {
    pub fn new() -> Self {
        Self
    }

    fn keyword_type(
        lexeme: &str,
    ) -> Option<ArithTokenType> {
        match lexeme {
            "true" | "false" => {
                Some(ArithTokenType::Boolean)
            }

            "if" => Some(ArithTokenType::If),
            "then" => Some(ArithTokenType::Then),
            "else" => Some(ArithTokenType::Else),

            _ => None,
        }
    }

    fn tokenize_line(
        line: &SourceLine,
    ) -> Result<Vec<ArithToken>, LexError> {
        let chars: Vec<char> =
            line.text.chars().collect();

        let mut tokens = Vec::new();
        let mut index = 0;

        while index < chars.len() {
            let character = chars[index];

            // -------------------------------------------------------------
            // Whitespace
            // -------------------------------------------------------------

            if character.is_whitespace() {
                index += 1;
                continue;
            }

            let start = index;

            // -------------------------------------------------------------
            // Integer
            // -------------------------------------------------------------

            if character.is_ascii_digit() {
                index += 1;

                while index < chars.len()
                    && chars[index].is_ascii_digit()
                {
                    index += 1;
                }

                let lexeme: String =
                    chars[start..index].iter().collect();

                tokens.push(ArithToken::new(
                    ArithTokenType::Integer,
                    lexeme,
                    Span::new(start, index),
                ));

                continue;
            }

            // -------------------------------------------------------------
            // Identifier / keyword
            // -------------------------------------------------------------

            if character.is_ascii_alphabetic() {
                index += 1;

                while index < chars.len()
                    && chars[index].is_ascii_alphanumeric()
                {
                    index += 1;
                }

                let lexeme: String =
                    chars[start..index].iter().collect();

                match Self::keyword_type(&lexeme) {
                    Some(kind) => {
                        tokens.push(ArithToken::new(
                            kind,
                            lexeme,
                            Span::new(start, index),
                        ));
                    }

                    None => {
                        return Err(
                            LexError::UnexpectedCharacter {
                                character,
                                span: Span::new(
                                    start,
                                    index,
                                ),
                            },
                        );
                    }
                }

                continue;
            }

            // -------------------------------------------------------------
            // Single-character tokens
            // -------------------------------------------------------------

            let kind = match character {
                '+' => Some(ArithTokenType::Plus),
                '-' => Some(ArithTokenType::Minus),
                '*' => Some(ArithTokenType::Star),
                '<' => Some(ArithTokenType::LessThan),
                '(' => Some(ArithTokenType::LeftParen),
                ')' => Some(ArithTokenType::RightParen),

                _ => None,
            };

            if let Some(kind) = kind {
                index += 1;

                let lexeme: String =
                    chars[start..index].iter().collect();

                tokens.push(ArithToken::new(
                    kind,
                    lexeme,
                    Span::new(start, index),
                ));

                continue;
            }

            // -------------------------------------------------------------
            // Multi-character operators
            // -------------------------------------------------------------

            if character == '=' {
                if index + 1 < chars.len()
                    && chars[index + 1] == '='
                {
                    index += 2;

                    tokens.push(ArithToken::new(
                        ArithTokenType::Equal,
                        "==",
                        Span::new(start, index),
                    ));

                    continue;
                }

                return Err(
                    LexError::UnexpectedEndOfOperator {
                        span: Span::new(start, start + 1),
                    },
                );
            }

            if character == '&' {
                if index + 1 < chars.len()
                    && chars[index + 1] == '&'
                {
                    index += 2;

                    tokens.push(ArithToken::new(
                        ArithTokenType::And,
                        "&&",
                        Span::new(start, index),
                    ));

                    continue;
                }

                return Err(
                    LexError::UnexpectedEndOfOperator {
                        span: Span::new(start, start + 1),
                    },
                );
            }

            if character == '|' {
                if index + 1 < chars.len()
                    && chars[index + 1] == '|'
                {
                    index += 2;

                    tokens.push(ArithToken::new(
                        ArithTokenType::Or,
                        "||",
                        Span::new(start, index),
                    ));

                    continue;
                }

                return Err(
                    LexError::UnexpectedEndOfOperator {
                        span: Span::new(start, start + 1),
                    },
                );
            }

            // -------------------------------------------------------------
            // Unknown character
            // -------------------------------------------------------------

            return Err(
                LexError::UnexpectedCharacter {
                    character,
                    span: Span::new(start, start + 1),
                },
            );
        }

        Ok(tokens)
    }
}

impl Default for ArithLexer {
    fn default() -> Self {
        Self::new()
    }
}

impl Lexer for ArithLexer {
    type Token = ArithToken;
    type Error = LexError;

    fn analyze(
        input: &[SourceLine],
    ) -> Result<Vec<Self::Token>, Self::Error> {
        let mut tokens = Vec::new();

        for line in input {
            tokens.extend(
                Self::tokenize_line(line)?
            );
        }

        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::Scanner;
    use crate::arith::ArithScanner;

    #[test]
    fn scans_lines() {
        let source =
            "1 + 2\n3 * 4";

        let lines =
            ArithScanner::scan(source)
                .unwrap();

        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0].number, 1);
        assert_eq!(lines[1].number, 2);
    }

    #[test]
    fn lexes_integer_expression() {
        let source =
            "1 + 2 * 3";

        let lines =
            ArithScanner::scan(source)
                .unwrap();

        let tokens =
            ArithLexer::analyze(&lines)
                .unwrap();

        assert_eq!(
            tokens
                .iter()
                .map(|token| token.kind)
                .collect::<Vec<_>>(),
            vec![
                ArithTokenType::Integer,
                ArithTokenType::Plus,
                ArithTokenType::Integer,
                ArithTokenType::Star,
                ArithTokenType::Integer,
            ]
        );
    }

    #[test]
    fn lexes_boolean_expression() {
        let source =
            "true && false";

        let lines =
            ArithScanner::scan(source)
                .unwrap();

        let tokens =
            ArithLexer::analyze(&lines)
                .unwrap();

        assert_eq!(
            tokens
                .iter()
                .map(|token| token.kind)
                .collect::<Vec<_>>(),
            vec![
                ArithTokenType::Boolean,
                ArithTokenType::And,
                ArithTokenType::Boolean,
            ]
        );
    }

    #[test]
    fn lexes_conditional() {
        let source =
            "if 1 < 2 then 3 else 4";

        let lines =
            ArithScanner::scan(source)
                .unwrap();

        let tokens =
            ArithLexer::analyze(&lines)
                .unwrap();

        assert_eq!(
            tokens
                .iter()
                .map(|token| token.kind)
                .collect::<Vec<_>>(),
            vec![
                ArithTokenType::If,
                ArithTokenType::Integer,
                ArithTokenType::LessThan,
                ArithTokenType::Integer,
                ArithTokenType::Then,
                ArithTokenType::Integer,
                ArithTokenType::Else,
                ArithTokenType::Integer,
            ]
        );
    }
}
