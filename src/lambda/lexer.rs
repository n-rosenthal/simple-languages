//! Lexer da linguagem `lambda` (cálculo λ simplesmente tipado).
//!
//! Tokens reconhecidos:
//!
//! 1. identificadores: uma letra ASCII seguida de letras, dígitos ou `_`
//!    (variáveis e nomes de tipos base);
//! 2. abstração: `λ` ou `\`;
//! 3. ponto `.`, dois-pontos `:` e seta `->`;
//! 4. parênteses `(` e `)`;
//! 5. espaço em branco, ignorado.
//!
//! As posições (`Span`) contam `char`s, não bytes: `λ` ocupa 2 bytes em
//! UTF-8, mas é uma posição.

use std::fmt;

use crate::common::{Lexer, SourceLine, Span};

use super::token::{LambdaToken, LambdaTokenType};

/// Erros produzidos pelo lexer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    UnexpectedCharacter { character: char, span: Span },
    /// Um operador de vários caracteres que começou e não terminou
    /// (`-` sem `>`).
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

    fn tokenize_line(line: &SourceLine) -> Result<Vec<LambdaToken>, LexError> {
        let chars: Vec<char> = line.text.chars().collect();

        let mut tokens = Vec::new();
        let mut index = 0;

        while index < chars.len() {
            let character = chars[index];

            // Espaço em branco
            if character.is_whitespace() {
                index += 1;
                continue;
            }

            let start = index;

            // Identificador: letra (letra | dígito | '_')*
            if Self::is_identifier_start(character) {
                index += 1;
                while index < chars.len() && Self::is_identifier_continue(chars[index]) {
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

            // Seta: '->' (um '-' sozinho é um operador incompleto)
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

            // Tokens de um caractere
            let kind = match character {
                'λ' | '\\' => Some(LambdaTokenType::Lambda),
                '.' => Some(LambdaTokenType::Dot),
                ':' => Some(LambdaTokenType::Colon),
                '(' => Some(LambdaTokenType::LParen),
                ')' => Some(LambdaTokenType::RParen),
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

            // Caractere desconhecido
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
    use crate::lambda::scanner::LambdaScanner;

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
                Lambda, Identifier, Colon, Identifier, Arrow, Identifier, Dot, LParen,
                Identifier, Identifier, RParen,
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

    #[test]
    fn empty_input_yields_no_tokens() {
        assert!(kinds("").is_empty());
        assert!(kinds("   \t ").is_empty());
    }

    #[test]
    fn spans_are_char_offsets() {
        let lines = LambdaScanner::scan("λx.x").unwrap();
        let tokens = LambdaLexer::analyze(&lines).unwrap();
        let spans: Vec<_> = tokens.iter().map(|t| (t.span.start, t.span.end)).collect();
        assert_eq!(spans, vec![(0, 1), (1, 2), (2, 3), (3, 4)]);
    }

    #[test]
    fn lexemes_are_preserved() {
        let lines = LambdaScanner::scan("λfoo_1:Nat->Nat. foo_1").unwrap();
        let tokens = LambdaLexer::analyze(&lines).unwrap();
        assert_eq!(tokens[1].lexeme, "foo_1");
        assert_eq!(tokens[4].lexeme, "->");
    }

    #[test]
    fn multiple_lines() {
        assert_eq!(kinds("λx.x\n(y)").len(), 4 + 3);
    }

    #[test]
    fn identifier_cannot_start_with_underscore() {
        let lines = LambdaScanner::scan("_x").unwrap();
        assert!(LambdaLexer::analyze(&lines).is_err());
    }
}
