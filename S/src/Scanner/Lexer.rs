//! S/src/Scanner/Lexer.rs
//!
//! Análise léxica: converte texto-fonte em uma sequência de tokens.
//!
//! author:  n-rosenthal
//! date:    2026-08-17
//! version: 0.5 (Iterator + Semicolon, removed unused source field)

use std::iter::Peekable;
use std::str::CharIndices;

use crate::Scanner::Types::{SourceFile, Span};

/// Um token da linguagem `S`, com sua posição de origem.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

// ---------------------------------------------------------------------
// TOKENS para a linguagem S
// ---------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // literais e identificadores
    LiteralInteger(i64),
    LiteralBoolean(bool),
    UnitLiteral,
    Identifier(String),

    // operadores binários
    AdditionOperator,
    SubtractionOperator,
    EqualityOperator,
    InequalityOperator,
    LTOperator,
    LEQOperator,
    GTOperator,
    GEQOperator,

    // operadores unários
    NotOperator,

    // memória
    RefKeyword,  // "ref"   (aloca)
    Bang,        // "!"     (dereferencia)
    ColonEquals, // ":="    (atribui)

    // condicional: if t then t else t
    If,
    Then,
    Else,

    // binding: let name : type = t in t
    Let,
    In,
    Colon,  // ":"
    Assign, // "=" (diferente de "==")

    // delimitadores e sequência
    LParen,
    RParen,
    LSquareBracket,
    RSquareBracket,
    LBrace,
    RBrace,
    Semicolon, // ";"

    Eof,
}

/// Erros possíveis durante a análise léxica.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum LexError {
    #[error("caractere inesperado '{ch}' em {line}:{column}")]
    UnexpectedChar { ch: char, line: usize, column: usize },
    #[error("número malformado em {line}:{column}")]
    UnterminatedNumber { line: usize, column: usize },
}

/// Percorre o conteúdo de um `SourceFile` produzindo tokens.
pub struct Lexer<'a> {
    chars: Peekable<CharIndices<'a>>,
    line: usize,
    column: usize,
    eof_emitted: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(source_file: &'a SourceFile) -> Self {
        Lexer {
            chars: source_file.content.char_indices().peekable(),
            line: 1,
            column: 1,
            eof_emitted: false,
        }
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

    /// Lê uma palavra alfanumérica e a classifica como palavra-chave
    /// conhecida ou, caso contrário, como `Identifier`.
    fn lex_word(&mut self) -> TokenKind {
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
            "true" => TokenKind::LiteralBoolean(true),
            "false" => TokenKind::LiteralBoolean(false),
            "unit" => TokenKind::UnitLiteral,
            "if" => TokenKind::If,
            "then" => TokenKind::Then,
            "else" => TokenKind::Else,
            "not" => TokenKind::NotOperator,
            "let" => TokenKind::Let,
            "in" => TokenKind::In,
            "ref" => TokenKind::RefKeyword,
            _ => TokenKind::Identifier(word),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        let (line, column) = (self.line, self.column);

        let Some(&(_, ch)) = self.chars.peek() else {
            if self.eof_emitted {
                return None;
            }
            self.eof_emitted = true;
            return Some(Ok(Token {
                kind: TokenKind::Eof,
                span: Span::new(line, column),
            }));
        };

        let kind = match ch {
            '+' => { self.advance(); TokenKind::AdditionOperator }
            '-' => { self.advance(); TokenKind::SubtractionOperator }
            '(' => { self.advance(); TokenKind::LParen }
            ')' => { self.advance(); TokenKind::RParen }
            '[' => { self.advance(); TokenKind::LSquareBracket }
            ']' => { self.advance(); TokenKind::RSquareBracket }
            '{' => { self.advance(); TokenKind::LBrace }
            '}' => { self.advance(); TokenKind::RBrace }
            ';' => { self.advance(); TokenKind::Semicolon }
            '!' => { self.advance(); TokenKind::Bang }
            ':' => {
                self.advance();
                match self.chars.peek() {
                    Some(&(_, '=')) => { self.advance(); TokenKind::ColonEquals }
                    _ => TokenKind::Colon,
                }
            }
            '=' => {
                self.advance();
                match self.chars.peek() {
                    Some(&(_, '=')) => { self.advance(); TokenKind::EqualityOperator }
                    _ => TokenKind::Assign,
                }
            }
            '<' => {
                self.advance();
                match self.chars.peek() {
                    Some(&(_, '>')) => { self.advance(); TokenKind::InequalityOperator }
                    Some(&(_, '=')) => { self.advance(); TokenKind::LEQOperator }
                    _ => TokenKind::LTOperator,
                }
            }
            '>' => {
                self.advance();
                match self.chars.peek() {
                    Some(&(_, '=')) => { self.advance(); TokenKind::GEQOperator }
                    _ => TokenKind::GTOperator,
                }
            }
            c if c.is_ascii_digit() => match self.lex_integer(line, column) {
                Ok(k) => k,
                Err(e) => return Some(Err(e)),
            },
            c if c.is_alphabetic() || c == '_' => self.lex_word(),
            c => return Some(Err(LexError::UnexpectedChar { ch: c, line, column })),
        };

        Some(Ok(Token {
            kind,
            span: Span::new(line, column),
        }))
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

    fn kinds_of(content: &str) -> Vec<TokenKind> {
        let src = source_from(content);
        Lexer::new(&src)
            .map(|res| res.expect("deveria tokenizar"))
            .map(|t| t.kind)
            .filter(|k| !matches!(k, TokenKind::Eof))
            .collect()
    }

    #[test]
    fn tokeniza_soma_de_inteiros() {
        assert_eq!(
            kinds_of("1 + 2"),
            vec![
                TokenKind::LiteralInteger(1),
                TokenKind::AdditionOperator,
                TokenKind::LiteralInteger(2),
            ]
        );
    }

    #[test]
    fn tokeniza_let_binding() {
        assert_eq!(
            kinds_of("let x: Integer = 1 in x + 1"),
            vec![
                TokenKind::Let,
                TokenKind::Identifier("x".to_string()),
                TokenKind::Colon,
                TokenKind::Identifier("Integer".to_string()),
                TokenKind::Assign,
                TokenKind::LiteralInteger(1),
                TokenKind::In,
                TokenKind::Identifier("x".to_string()),
                TokenKind::AdditionOperator,
                TokenKind::LiteralInteger(1),
            ]
        );
    }

    #[test]
    fn distingue_igual_simples_de_igual_duplo() {
        assert_eq!(
            kinds_of("x = 1"),
            vec![
                TokenKind::Identifier("x".to_string()),
                TokenKind::Assign,
                TokenKind::LiteralInteger(1),
            ]
        );
        assert_eq!(
            kinds_of("x == 1"),
            vec![
                TokenKind::Identifier("x".to_string()),
                TokenKind::EqualityOperator,
                TokenKind::LiteralInteger(1),
            ]
        );
    }

    #[test]
    fn tokeniza_sequencia() {
        assert_eq!(
            kinds_of("1 ; 2"),
            vec![
                TokenKind::LiteralInteger(1),
                TokenKind::Semicolon,
                TokenKind::LiteralInteger(2),
            ]
        );
    }
}
