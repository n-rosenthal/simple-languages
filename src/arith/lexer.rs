//! Lexer de `arith`.
//!
//! Tokens: inteiros, `true`/`false`, `if`/`then`/`else`, `+ - * <`,
//! `==`, `&&`, `||` e parênteses. Qualquer outra palavra é um erro:
//! `arith` não tem variáveis.

use std::fmt;

use crate::common::{Lexer, SourceLine, Span};

use super::token::{ArithToken, ArithTokenType};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    UnexpectedCharacter { character: char, span: Span },
    /// Uma palavra que não é palavra-chave (`arith` não tem variáveis).
    UnknownWord { word: String, span: Span },
    /// Um operador de dois caracteres que não terminou (`=` sem `=`).
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
            Self::UnknownWord { word, span } => write!(
                f,
                "unknown word `{word}` at {}..{} (arith has no variables)",
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

pub struct ArithLexer;

impl ArithLexer {
    pub fn new() -> Self {
        Self
    }

    fn keyword_type(lexeme: &str) -> Option<ArithTokenType> {
        match lexeme {
            "true" | "false" => Some(ArithTokenType::Boolean),
            "if" => Some(ArithTokenType::If),
            "then" => Some(ArithTokenType::Then),
            "else" => Some(ArithTokenType::Else),
            _ => None,
        }
    }

    /// `==`, `&&` ou `||`: o mesmo caractere duas vezes.
    fn doubled(
        chars: &[char],
        index: usize,
        kind: ArithTokenType,
        lexeme: &'static str,
    ) -> Result<(ArithToken, usize), LexError> {
        if chars.get(index + 1) == Some(&chars[index]) {
            let token = ArithToken::new(kind, lexeme, Span::new(index, index + 2));
            Ok((token, index + 2))
        } else {
            Err(LexError::UnexpectedEndOfOperator { span: Span::new(index, index + 1) })
        }
    }

    fn tokenize_line(line: &SourceLine) -> Result<Vec<ArithToken>, LexError> {
        let chars: Vec<char> = line.text.chars().collect();
        let mut tokens = Vec::new();
        let mut index = 0;

        while index < chars.len() {
            let character = chars[index];
            let start = index;

            if character.is_whitespace() {
                index += 1;
                continue;
            }

            // Inteiro
            if character.is_ascii_digit() {
                while index < chars.len() && chars[index].is_ascii_digit() {
                    index += 1;
                }
                let lexeme: String = chars[start..index].iter().collect();
                tokens.push(ArithToken::new(
                    ArithTokenType::Integer,
                    lexeme,
                    Span::new(start, index),
                ));
                continue;
            }

            // Palavra-chave (qualquer outra palavra é um erro)
            if character.is_ascii_alphabetic() {
                while index < chars.len() && chars[index].is_ascii_alphanumeric() {
                    index += 1;
                }
                let lexeme: String = chars[start..index].iter().collect();

                match Self::keyword_type(&lexeme) {
                    Some(kind) => {
                        tokens.push(ArithToken::new(kind, lexeme, Span::new(start, index)))
                    }
                    None => {
                        return Err(LexError::UnknownWord {
                            word: lexeme,
                            span: Span::new(start, index),
                        })
                    }
                }
                continue;
            }

            // Operadores de dois caracteres
            let doubled = match character {
                '=' => Some((ArithTokenType::Equal, "==")),
                '&' => Some((ArithTokenType::And, "&&")),
                '|' => Some((ArithTokenType::Or, "||")),
                _ => None,
            };
            if let Some((kind, lexeme)) = doubled {
                let (token, next) = Self::doubled(&chars, index, kind, lexeme)?;
                tokens.push(token);
                index = next;
                continue;
            }

            // Operadores de um caractere
            let kind = match character {
                '+' => ArithTokenType::Plus,
                '-' => ArithTokenType::Minus,
                '*' => ArithTokenType::Star,
                '<' => ArithTokenType::LessThan,
                '(' => ArithTokenType::LeftParen,
                ')' => ArithTokenType::RightParen,
                _ => {
                    return Err(LexError::UnexpectedCharacter {
                        character,
                        span: Span::new(start, start + 1),
                    })
                }
            };
            index += 1;
            tokens.push(ArithToken::new(kind, character.to_string(), Span::new(start, index)));
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

    fn analyze(input: &[SourceLine]) -> Result<Vec<ArithToken>, LexError> {
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
    use crate::arith::ArithScanner;
    use crate::common::Scanner;
    use ArithTokenType::*;

    fn kinds(source: &str) -> Vec<ArithTokenType> {
        let lines = ArithScanner::scan(source).unwrap();
        ArithLexer::analyze(&lines).unwrap().iter().map(|t| t.kind).collect()
    }

    fn error(source: &str) -> LexError {
        let lines = ArithScanner::scan(source).unwrap();
        ArithLexer::analyze(&lines).unwrap_err()
    }

    #[test]
    fn scans_lines() {
        let lines = ArithScanner::scan("1 + 2\n3 * 4").unwrap();
        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0].number, 1);
        assert_eq!(lines[1].number, 2);
    }

    #[test]
    fn lexes_integer_expression() {
        assert_eq!(kinds("1 + 2 * 3"), vec![Integer, Plus, Integer, Star, Integer]);
    }

    #[test]
    fn lexes_boolean_expression() {
        assert_eq!(kinds("true && false"), vec![Boolean, And, Boolean]);
        assert_eq!(kinds("true || false"), vec![Boolean, Or, Boolean]);
    }

    #[test]
    fn lexes_conditional() {
        assert_eq!(
            kinds("if 1 < 2 then 3 else 4"),
            vec![If, Integer, LessThan, Integer, Then, Integer, Else, Integer]
        );
    }

    #[test]
    fn lexes_equality_and_parentheses() {
        assert_eq!(
            kinds("(1 - 2) == 3"),
            vec![LeftParen, Integer, Minus, Integer, RightParen, Equal, Integer]
        );
    }

    #[test]
    fn numbers_followed_by_letters_split() {
        // `12ab`: o inteiro 12 e depois a palavra desconhecida `ab`
        assert!(matches!(error("12ab"), LexError::UnknownWord { .. }));
    }

    #[test]
    fn unknown_words_are_errors() {
        assert_eq!(
            error("1 + foo"),
            LexError::UnknownWord { word: "foo".into(), span: Span::new(4, 7) }
        );
    }

    #[test]
    fn half_operators_are_errors() {
        for source in ["a = b", "1 = 2", "1 & 2", "1 | 2", "1 ="] {
            // `a` falha antes, as demais no operador
            let e = error(source);
            assert!(
                matches!(e, LexError::UnknownWord { .. } | LexError::UnexpectedEndOfOperator { .. }),
                "{source}: {e:?}"
            );
        }
        assert!(matches!(error("1 = 2"), LexError::UnexpectedEndOfOperator { .. }));
    }

    #[test]
    fn unknown_characters_are_errors() {
        assert!(matches!(
            error("1 # 2"),
            LexError::UnexpectedCharacter { character: '#', .. }
        ));
    }

    #[test]
    fn spans_and_lexemes() {
        let lines = ArithScanner::scan("12 == 3").unwrap();
        let tokens = ArithLexer::analyze(&lines).unwrap();
        assert_eq!(tokens[0].lexeme, "12");
        assert_eq!((tokens[1].span.start, tokens[1].span.end), (3, 5));
    }
}
