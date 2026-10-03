//! Lexer da linguagem `stlc` (cálculo λ simplesmente tipado, com `Bool`).
//!
//! Tokens reconhecidos:
//!
//! 1. identificadores: uma letra ASCII seguida de letras, dígitos ou `_`
//!    (variáveis e nomes de tipos base), exceto as palavras-chave
//!    `true`, `false`, `if`, `then` e `else`;
//! 2. abstração: `λ` ou `\`;
//! 3. ponto `.`, dois-pontos `:` e seta `->`;
//! 4. parênteses `(` e `)`;
//! 5. espaço em branco, ignorado.
//!
//! As posições (`Span`) contam `char`s, não bytes: `λ` ocupa 2 bytes em
//! UTF-8, mas é uma posição.

use std::fmt;

use crate::common::{Lexer, SourceLine, Span};

use super::token::{StlcToken, StlcTokenType};

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

pub struct StlcLexer;

impl StlcLexer {
    pub fn new() -> Self {
        Self
    }

    fn keyword_type(lexeme: &str) -> Option<StlcTokenType> {
        match lexeme {
            "true" => Some(StlcTokenType::True),
            "false" => Some(StlcTokenType::False),
            "if" => Some(StlcTokenType::If),
            "then" => Some(StlcTokenType::Then),
            "else" => Some(StlcTokenType::Else),
            _ => None,
        }
    }

    fn is_identifier_start(c: char) -> bool {
        c.is_ascii_alphabetic()
    }

    fn is_identifier_continue(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }

    fn tokenize_line(line: &SourceLine) -> Result<Vec<StlcToken>, LexError> {
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
                let kind = Self::keyword_type(&lexeme).unwrap_or(StlcTokenType::Identifier);
                tokens.push(StlcToken::new(kind, lexeme, Span::new(start, index)));
                continue;
            }

            // Seta: '->' (um '-' sozinho é um operador incompleto)
            if character == '-' {
                if index + 1 < chars.len() && chars[index + 1] == '>' {
                    index += 2;
                    tokens.push(StlcToken::new(
                        StlcTokenType::Arrow,
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
                'λ' | '\\' => Some(StlcTokenType::Lambda),
                '.' => Some(StlcTokenType::Dot),
                ':' => Some(StlcTokenType::Colon),
                '(' => Some(StlcTokenType::LParen),
                ')' => Some(StlcTokenType::RParen),
                _ => None,
            };

            if let Some(kind) = kind {
                index += 1;
                tokens.push(StlcToken::new(
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

impl Default for StlcLexer {
    fn default() -> Self {
        Self::new()
    }
}

impl Lexer for StlcLexer {
    type Token = StlcToken;
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
    use crate::stlc::scanner::StlcScanner;

    fn kinds(source: &str) -> Vec<StlcTokenType> {
        let lines = StlcScanner::scan(source).unwrap();
        StlcLexer::analyze(&lines)
            .unwrap()
            .iter()
            .map(|t| t.kind)
            .collect()
    }

    #[test]
    fn lexes_untyped_abstraction_without_spaces() {
        use StlcTokenType::*;
        assert_eq!(kinds("λx.x"), vec![Lambda, Identifier, Dot, Identifier]);
    }

    #[test]
    fn lexes_typed_abstraction() {
        use StlcTokenType::*;
        assert_eq!(
            kinds("λx:Bool->Bool. (f x)"),
            vec![
                Lambda, Identifier, Colon, Identifier, Arrow, Identifier, Dot, LParen,
                Identifier, Identifier, RParen,
            ]
        );
    }

    #[test]
    fn keywords_are_not_identifiers() {
        use StlcTokenType::*;
        assert_eq!(
            kinds("if true then false else x"),
            vec![If, True, Then, False, Else, Identifier]
        );
    }

    #[test]
    fn keyword_prefixes_are_identifiers() {
        use StlcTokenType::*;
        assert_eq!(kinds("iffy trueish thenx"), vec![Identifier, Identifier, Identifier]);
    }

    #[test]
    fn accepts_backslash_as_lambda() {
        assert_eq!(kinds("\\x.x")[0], StlcTokenType::Lambda);
    }

    #[test]
    fn lone_minus_is_an_error() {
        let lines = StlcScanner::scan("Bool - Bool").unwrap();
        assert!(matches!(
            StlcLexer::analyze(&lines),
            Err(LexError::UnexpectedEndOfOperator { .. })
        ));
    }

    #[test]
    fn rejects_unknown_character() {
        let lines = StlcScanner::scan("λx#.x").unwrap();
        assert!(matches!(
            StlcLexer::analyze(&lines),
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
        let lines = StlcScanner::scan("λx.x").unwrap();
        let tokens = StlcLexer::analyze(&lines).unwrap();
        let spans: Vec<_> = tokens.iter().map(|t| (t.span.start, t.span.end)).collect();
        assert_eq!(spans, vec![(0, 1), (1, 2), (2, 3), (3, 4)]);
    }

    #[test]
    fn lexemes_are_preserved() {
        let lines = StlcScanner::scan("λfoo_1:Nat->Nat. foo_1").unwrap();
        let tokens = StlcLexer::analyze(&lines).unwrap();
        assert_eq!(tokens[1].lexeme, "foo_1");
        assert_eq!(tokens[4].lexeme, "->");
    }

    #[test]
    fn multiple_lines() {
        assert_eq!(kinds("λx.x\n(y)").len(), 4 + 3);
    }

    #[test]
    fn identifier_cannot_start_with_underscore() {
        let lines = StlcScanner::scan("_x").unwrap();
        assert!(StlcLexer::analyze(&lines).is_err());
    }
}
