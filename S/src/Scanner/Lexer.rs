//! S/src/Scanner/Lexer.rs
//!
//! Análise léxica: converte texto-fonte em uma sequência de tokens.
//!
//! author:  n-rosenthal
//! date:    2026-08-17
//! version: 0.1

use std::iter::Peekable;
use std::str::CharIndices;

use crate::Scanner::Types::SourceFile;

/// Um token da linguagem `S`, com sua posição de origem.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,   // 1-indexado
    pub column: usize, // 1-indexado
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    LiteralInteger(i64),
    LiteralBoolean(bool),

    Plus,   // Add
    Minus,  // Sub
    EqEq,   // Eq

    LParen,
    RParen,

    Eof,
}

/// Erros possíveis durante a análise léxica.
#[derive(Debug, PartialEq)]
pub enum LexError {
    UnexpectedChar { ch: char, line: usize, column: usize },
    UnterminatedNumber { line: usize, column: usize },
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexError::UnexpectedChar { ch, line, column } => {
                write!(f, "caractere inesperado '{ch}' em {line}:{column}")
            }
            LexError::UnterminatedNumber { line, column } => {
                write!(f, "número malformado em {line}:{column}")
            }
        }
    }
}

impl std::error::Error for LexError {}

/// Percorre o conteúdo de um `SourceFile` produzindo tokens.
pub struct Lexer<'a> {
    source: &'a str,
    chars: Peekable<CharIndices<'a>>,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source_file: &'a SourceFile) -> Self {
        Lexer {
            source: &source_file.content,
            chars: source_file.content.char_indices().peekable(),
            line: 1,
            column: 1,
        }
    }

    /// Consome todo o input e retorna a lista de tokens (terminada em
    /// `TokenKind::Eof`), ou o primeiro erro léxico encontrado.
    pub fn tokenize(mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();

        loop {
            self.skip_whitespace();

            let (line, column) = (self.line, self.column);

            let Some(&(_, ch)) = self.chars.peek() else {
                tokens.push(Token { kind: TokenKind::Eof, line, column });
                break;
            };

            let kind = match ch {
                '+' => { self.advance(); TokenKind::Plus }
                '-' => { self.advance(); TokenKind::Minus }
                '(' => { self.advance(); TokenKind::LParen }
                ')' => { self.advance(); TokenKind::RParen }
                '=' => {
                    self.advance();
                    match self.chars.peek() {
                        Some(&(_, '=')) => { self.advance(); TokenKind::EqEq }
                        _ => return Err(LexError::UnexpectedChar { ch: '=', line, column }),
                    }
                }
                c if c.is_ascii_digit() => self.lex_integer(line, column)?,
                c if c.is_alphabetic() || c == '_' => self.lex_keyword(line, column)?,
                c => return Err(LexError::UnexpectedChar { ch: c, line, column }),
            };

            tokens.push(Token { kind, line, column });
        }

        Ok(tokens)
    }

    fn advance(&mut self) -> Option<char> {
        let (_, ch) = self.chars.next()?;

        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }

        Some(ch)
    }

    fn skip_whitespace(&mut self) {
        while let Some(&(_, ch)) = self.chars.peek() {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn lex_integer(&mut self, line: usize, column: usize) -> Result<TokenKind, LexError> {
        let mut digits = String::new();

        while let Some(&(_, ch)) = self.chars.peek() {
            if ch.is_ascii_digit() {
                digits.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        digits
            .parse::<i64>()
            .map(TokenKind::LiteralInteger)
            .map_err(|_| LexError::UnterminatedNumber { line, column })
    }

    fn lex_keyword(&mut self, _line: usize, _column: usize) -> Result<TokenKind, LexError> {
        let mut word = String::new();

        while let Some(&(_, ch)) = self.chars.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                word.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        match word.as_str() {
            "true"  => Ok(TokenKind::LiteralBoolean(true)),
            "false" => Ok(TokenKind::LiteralBoolean(false)),
            other => Err(LexError::UnexpectedChar {
                ch: other.chars().next().unwrap_or('?'),
                line: self.line,
                column: self.column,
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Scanner::Types::SourceFile;
    use std::path::PathBuf;

    fn source_from(content: &str) -> SourceFile {
        SourceFile {
            path: PathBuf::from("<memória>"),
            content: content.to_string(),
            lines: content.lines().map(str::to_string).collect(),
        }
    }

    #[test]
    fn tokeniza_soma_de_inteiros() {
        let src = source_from("1 + 2");
        let tokens = Lexer::new(&src).tokenize().expect("deveria tokenizar");

        let kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::LiteralInteger(1),
                TokenKind::Plus,
                TokenKind::LiteralInteger(2),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn tokeniza_comparacao_de_booleanos() {
        let src = source_from("true == false");
        let tokens = Lexer::new(&src).tokenize().expect("deveria tokenizar");

        let kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::LiteralBoolean(true),
                TokenKind::EqEq,
                TokenKind::LiteralBoolean(false),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn caractere_desconhecido_gera_erro() {
        let src = source_from("1 % 2");
        let result = Lexer::new(&src).tokenize();
        assert!(matches!(result, Err(LexError::UnexpectedChar { ch: '%', .. })));
    }

    #[test]
    fn expressao_com_parenteses() {
        let src = source_from("(1 - 2)");
        let tokens = Lexer::new(&src).tokenize().expect("deveria tokenizar");

        let kinds: Vec<TokenKind> = tokens.into_iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::LParen,
                TokenKind::LiteralInteger(1),
                TokenKind::Minus,
                TokenKind::LiteralInteger(2),
                TokenKind::RParen,
                TokenKind::Eof,
            ]
        );
    }
}
