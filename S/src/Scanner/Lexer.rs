//! S/src/Scanner/Lexer.rs
//!
//! Análise léxica: converte texto-fonte em uma sequência de tokens.
//!
//! author:  n-rosenthal
//! date:    2026-08-17
//! version: 0.4

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

    // delimitadores
    LParen,
    RParen,
    LSquareBracket,
    RSquareBracket,
    LBrace,
    RBrace,

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
                '+' => { self.advance(); TokenKind::AdditionOperator }
                '-' => { self.advance(); TokenKind::SubtractionOperator }
                '(' => { self.advance(); TokenKind::LParen }
                ')' => { self.advance(); TokenKind::RParen }
                '[' => { self.advance(); TokenKind::LSquareBracket }
                ']' => { self.advance(); TokenKind::RSquareBracket }
                '{' => { self.advance(); TokenKind::LBrace }
                '}' => { self.advance(); TokenKind::RBrace }
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
                c if c.is_ascii_digit() => self.lex_integer(line, column)?,
                c if c.is_alphabetic() || c == '_' => self.lex_word(),
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

    /// Lê uma palavra alfanumérica e a classifica como palavra-chave
    /// conhecida ou, caso contrário, como `Identifier` (nome de
    /// variável ou tipo).
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
            .tokenize()
            .expect("deveria tokenizar")
            .into_iter()
            .map(|t| t.kind)
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
                TokenKind::Eof,
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
                TokenKind::Eof,
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
                TokenKind::Eof,
            ]
        );
        assert_eq!(
            kinds_of("x == 1"),
            vec![
                TokenKind::Identifier("x".to_string()),
                TokenKind::EqualityOperator,
                TokenKind::LiteralInteger(1),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn tokeniza_ref_deref_assign() {
        assert_eq!(
            kinds_of("ref 1"),
            vec![TokenKind::RefKeyword, TokenKind::LiteralInteger(1), TokenKind::Eof]
        );
        assert_eq!(
            kinds_of("!r"),
            vec![TokenKind::Bang, TokenKind::Identifier("r".to_string()), TokenKind::Eof]
        );
        assert_eq!(
            kinds_of("r := 5"),
            vec![
                TokenKind::Identifier("r".to_string()),
                TokenKind::ColonEquals,
                TokenKind::LiteralInteger(5),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn colon_simples_nao_conflita_com_colon_equals() {
        // ':' seguido de identificador (anotação de tipo em `let`)
        // não deve ser confundido com ':='.
        assert_eq!(
            kinds_of("x: Integer"),
            vec![
                TokenKind::Identifier("x".to_string()),
                TokenKind::Colon,
                TokenKind::Identifier("Integer".to_string()),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn tokeniza_unit() {
        assert_eq!(kinds_of("unit"), vec![TokenKind::UnitLiteral, TokenKind::Eof]);
    }

    #[test]
    fn caractere_desconhecido_gera_erro() {
        let result = Lexer::new(&source_from("1 % 2")).tokenize();
        assert!(matches!(result, Err(LexError::UnexpectedChar { ch: '%', .. })));
    }
}
