//! Um lexer dirigido por tabela, comum a todas as linguagens.
//!
//! Cada linguagem declara uma [`LexSpec`] (palavras-chave, símbolos, se há
//! inteiros e o que fazer com as outras palavras) e o resto, que era repetido
//! em cada lexer escrito à mão, fica aqui: espaços, posições, o casamento do
//! símbolo mais longo, operadores incompletos e erros.
//!
//! Regras fixas, iguais para todas as linguagens por enquanto:
//!
//! - uma *palavra* começa com uma letra ASCII e continua com letras, dígitos
//!   ou `_`;
//! - um *inteiro* é uma sequência de dígitos ASCII (se a linguagem os tiver);
//! - os símbolos casam pelo mais longo (`->` vence `-`);
//! - um símbolo que só aparece como começo de outro (`=` em `==`, `-` em `->`)
//!   é um operador incompleto: [`LexError::UnexpectedEndOfOperator`];
//! - as posições ([`Span`]) contam `char`s, não bytes.

use std::fmt;

use crate::common::{SourceLine, Span};

/// Um token: o tipo `K` (uma enum por linguagem), o texto e a posição.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<K> {
    pub kind: K,
    pub lexeme: String,
    pub span: Span,
}

impl<K> Token<K> {
    pub fn new(kind: K, lexeme: impl Into<String>, span: Span) -> Self {
        Self { kind, lexeme: lexeme.into(), span }
    }
}

/// O que fazer com uma palavra que não é palavra-chave.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Words<K> {
    /// É um identificador (uma variável, um nome de tipo).
    Identifier(K),
    /// É um erro; o texto é uma dica para a mensagem (`"arith has no variables"`).
    Reject(&'static str),
}

/// A descrição léxica de uma linguagem.
#[derive(Debug, Clone, Copy)]
pub struct LexSpec<K: 'static> {
    /// Palavras reservadas: `("if", If)`.
    pub keywords: &'static [(&'static str, K)],
    /// Símbolos: `("->", Arrow)`. Podem ter mais de um caractere, e mais de
    /// um símbolo pode ter o mesmo tipo (`λ` e `\`).
    pub symbols: &'static [(&'static str, K)],
    /// O tipo dos literais inteiros, se a linguagem os tiver.
    pub integer: Option<K>,
    /// O que fazer com as demais palavras.
    pub words: Words<K>,
    /// O começo de um comentário de linha (`"--"`), se houver.
    pub line_comment: Option<&'static str>,
}

/// Erros do lexer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    UnexpectedCharacter { character: char, span: Span },
    /// Uma palavra que não é palavra-chave, numa linguagem sem identificadores.
    UnknownWord { word: String, span: Span, hint: &'static str },
    /// Um operador de vários caracteres que não terminou (`=` sem `=`).
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
            Self::UnknownWord { word, span, hint } if hint.is_empty() => {
                write!(f, "unknown word `{word}` at {}..{}", span.start, span.end)
            }
            Self::UnknownWord { word, span, hint } => write!(
                f,
                "unknown word `{word}` at {}..{} ({hint})",
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

/// `chars[index..]` começa com `text`?
fn starts_with(chars: &[char], index: usize, text: &str) -> bool {
    text.chars()
        .enumerate()
        .all(|(offset, c)| chars.get(index + offset) == Some(&c))
}

fn is_word_start(c: char) -> bool {
    c.is_ascii_alphabetic()
}

fn is_word_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

fn lex_line<K: Copy>(
    spec: &LexSpec<K>,
    line: &SourceLine,
    out: &mut Vec<Token<K>>,
) -> Result<(), LexError> {
    let chars: Vec<char> = line.text.chars().collect();
    let mut index = 0;

    while index < chars.len() {
        let c = chars[index];
        let start = index;

        if c.is_whitespace() {
            index += 1;
            continue;
        }

        if let Some(comment) = spec.line_comment {
            if starts_with(&chars, index, comment) {
                break; // o resto da linha é comentário
            }
        }

        // Inteiro
        if let (Some(kind), true) = (spec.integer, c.is_ascii_digit()) {
            while index < chars.len() && chars[index].is_ascii_digit() {
                index += 1;
            }
            let lexeme: String = chars[start..index].iter().collect();
            out.push(Token::new(kind, lexeme, Span::new(start, index)));
            continue;
        }

        // Palavra: palavra-chave ou identificador
        if is_word_start(c) {
            while index < chars.len() && is_word_continue(chars[index]) {
                index += 1;
            }
            let lexeme: String = chars[start..index].iter().collect();
            let span = Span::new(start, index);

            let keyword = spec
                .keywords
                .iter()
                .find(|(word, _)| *word == lexeme)
                .map(|(_, kind)| *kind);

            match (keyword, spec.words) {
                (Some(kind), _) | (None, Words::Identifier(kind)) => {
                    out.push(Token::new(kind, lexeme, span));
                }
                (None, Words::Reject(hint)) => {
                    return Err(LexError::UnknownWord { word: lexeme, span, hint });
                }
            }
            continue;
        }

        // Símbolo: o mais longo que casa
        let longest = spec
            .symbols
            .iter()
            .filter(|(symbol, _)| starts_with(&chars, index, symbol))
            .max_by_key(|(symbol, _)| symbol.chars().count());

        if let Some((symbol, kind)) = longest {
            index += symbol.chars().count();
            out.push(Token::new(*kind, *symbol, Span::new(start, index)));
            continue;
        }

        // Nenhum símbolo casou inteiro: se algum começa assim, o operador ficou
        // incompleto (`=` sem o segundo `=`); senão, o caractere não é da linguagem.
        let span = Span::new(start, start + 1);
        if spec.symbols.iter().any(|(symbol, _)| symbol.chars().next() == Some(c)) {
            return Err(LexError::UnexpectedEndOfOperator { span });
        }
        return Err(LexError::UnexpectedCharacter { character: c, span });
    }

    Ok(())
}

/// Divide as linhas em tokens segundo `spec`.
pub fn lex<K: Copy>(spec: &LexSpec<K>, input: &[SourceLine]) -> Result<Vec<Token<K>>, LexError> {
    let mut tokens = Vec::new();
    for line in input {
        lex_line(spec, line, &mut tokens)?;
    }
    Ok(tokens)
}
