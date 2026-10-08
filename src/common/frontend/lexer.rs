//! Um lexer dirigido por tabela, comum a todas as linguagens.
//!
//! Cada linguagem declara uma [`LexSpec`] (palavras-chave, símbolos, se há
//! inteiros e o que fazer com as outras palavras) e o resto, que era repetido
//! em cada lexer escrito à mão, fica aqui: espaços, posições, o casamento do
//! símbolo mais longo, operadores incompletos e erros.
//!
//! Como o lexer lê:
//!
//! - uma *palavra* começa onde `word_start` diz (padrão: letra ASCII) e
//!   continua enquanto `word_continue` disser (padrão: letra, dígito ou `_`);
//! - um *inteiro* é uma sequência de dígitos ASCII (se a linguagem os tiver);
//! - os símbolos casam pelo mais longo (`->` vence `-`);
//! - um símbolo que só aparece como começo de outro (`=` em `==`, `-` em `->`)
//!   é um operador incompleto: [`LexError::UnexpectedEndOfOperator`];
//! - comentários de linha (`--`) e de bloco (`(* ... *)`, que podem atravessar
//!   linhas, sem aninhamento) são ignorados;
//! - as posições ([`Span`]) contam `char`s, não bytes.

use std::fmt;

use crate::common::diagnostic::{Diagnostic, Position};
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

/// Começa uma palavra: letra ASCII.
pub fn ascii_word_start(c: char) -> bool {
    c.is_ascii_alphabetic()
}

/// Continua uma palavra: letra ASCII, dígito ou `_`.
pub fn ascii_word_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

/// A descrição léxica de uma linguagem.
///
/// Uma tabela curta parte de [`LexSpec::EMPTY`]:
///
/// ```ignore
/// const SPEC: LexSpec<Kind> = LexSpec {
///     keywords: &[("if", Kind::If)],
///     symbols: &[("->", Kind::Arrow)],
///     words: Words::Identifier(Kind::Ident),
///     ..LexSpec::EMPTY
/// };
/// ```
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
    /// Os delimitadores de um comentário de bloco (`("(*", "*)")`), se houver.
    pub block_comment: Option<(&'static str, &'static str)>,
    /// Quais caracteres podem começar uma palavra.
    pub word_start: fn(char) -> bool,
    /// Quais caracteres podem continuar uma palavra.
    pub word_continue: fn(char) -> bool,
}

impl<K: 'static> LexSpec<K> {
    /// Uma tabela vazia: sem palavras-chave, símbolos, inteiros nem comentários,
    /// e toda palavra é um erro.
    pub const EMPTY: Self = Self {
        keywords: &[],
        symbols: &[],
        integer: None,
        words: Words::Reject(""),
        line_comment: None,
        block_comment: None,
        word_start: ascii_word_start,
        word_continue: ascii_word_continue,
    };
}

/// Erros do lexer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    UnexpectedCharacter { character: char, span: Span },
    /// Uma palavra que não é palavra-chave, numa linguagem sem identificadores.
    UnknownWord { word: String, span: Span, hint: &'static str },
    /// Um operador de vários caracteres que não terminou (`=` sem `=`).
    UnexpectedEndOfOperator { span: Span },
    /// Um comentário de bloco que chegou ao fim da entrada sem fechar.
    UnterminatedComment { line: usize, span: Span },
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
            Self::UnterminatedComment { line, span } => write!(
                f,
                "unterminated comment starting on line {line} at {}..{}",
                span.start, span.end
            ),
        }
    }
}

impl std::error::Error for LexError {}

impl Diagnostic for LexError {
    fn message(&self) -> String {
        match self {
            Self::UnexpectedCharacter { character, .. } => {
                format!("unexpected character `{character}`")
            }
            Self::UnknownWord { word, hint, .. } if hint.is_empty() => {
                format!("unknown word `{word}`")
            }
            Self::UnknownWord { word, hint, .. } => format!("unknown word `{word}` ({hint})"),
            Self::UnexpectedEndOfOperator { .. } => "unexpected end of operator".to_string(),
            Self::UnterminatedComment { .. } => "unterminated comment".to_string(),
        }
    }

    fn position(&self) -> Position {
        match self {
            Self::UnexpectedCharacter { span, .. }
            | Self::UnknownWord { span, .. }
            | Self::UnexpectedEndOfOperator { span }
            | Self::UnterminatedComment { span, .. } => Position::Span(*span),
        }
    }

    /// Um comentário de bloco aberto pode ser fechado por mais texto.
    fn is_incomplete(&self) -> bool {
        matches!(self, Self::UnterminatedComment { .. })
    }
}

/// `chars[index..]` começa com `text`?
fn starts_with(chars: &[char], index: usize, text: &str) -> bool {
    text.chars()
        .enumerate()
        .all(|(offset, c)| chars.get(index + offset) == Some(&c))
}

/// A posição de `text` em `chars[from..]`, se aparecer.
fn find(chars: &[char], from: usize, text: &str) -> Option<usize> {
    (from..=chars.len()).find(|&index| starts_with(chars, index, text))
}

/// Um comentário de bloco aberto: a linha e a posição do `(*`.
type OpenComment = Option<(usize, Span)>;

fn lex_line<K: Copy>(
    spec: &LexSpec<K>,
    line: &SourceLine,
    out: &mut Vec<Token<K>>,
    open: &mut OpenComment,
) -> Result<(), LexError> {
    let chars: Vec<char> = line.text.chars().collect();
    // posições globais: o começo da linha no texto inteiro mais a coluna
    let span = |from: usize, to: usize| Span::new(line.offset + from, line.offset + to);
    let mut index = 0;

    // um comentário de bloco que vem de uma linha anterior
    if open.is_some() {
        let Some((_, close)) = spec.block_comment else {
            unreachable!("só há comentário aberto se a linguagem tem comentários de bloco")
        };
        match find(&chars, 0, close) {
            Some(position) => {
                index = position + close.chars().count();
                *open = None;
            }
            None => return Ok(()), // a linha toda é comentário
        }
    }

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

        if let Some((begin, close)) = spec.block_comment {
            if starts_with(&chars, index, begin) {
                let after = index + begin.chars().count();
                match find(&chars, after, close) {
                    Some(position) => {
                        index = position + close.chars().count();
                        continue;
                    }
                    None => {
                        *open = Some((line.number, span(start, after)));
                        return Ok(());
                    }
                }
            }
        }

        // Inteiro
        if let (Some(kind), true) = (spec.integer, c.is_ascii_digit()) {
            while index < chars.len() && chars[index].is_ascii_digit() {
                index += 1;
            }
            let lexeme: String = chars[start..index].iter().collect();
            out.push(Token::new(kind, lexeme, span(start, index)));
            continue;
        }

        // Palavra: palavra-chave ou identificador
        if (spec.word_start)(c) {
            while index < chars.len() && (spec.word_continue)(chars[index]) {
                index += 1;
            }
            let lexeme: String = chars[start..index].iter().collect();
            let word_span = span(start, index);

            let keyword = spec
                .keywords
                .iter()
                .find(|(word, _)| *word == lexeme)
                .map(|(_, kind)| *kind);

            match (keyword, spec.words) {
                (Some(kind), _) | (None, Words::Identifier(kind)) => {
                    out.push(Token::new(kind, lexeme, word_span));
                }
                (None, Words::Reject(hint)) => {
                    return Err(LexError::UnknownWord { word: lexeme, span: word_span, hint });
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
            out.push(Token::new(*kind, *symbol, span(start, index)));
            continue;
        }

        // Nenhum símbolo casou inteiro: se algum começa assim, o operador ficou
        // incompleto (`=` sem o segundo `=`); senão, o caractere não é da linguagem.
        let here = span(start, start + 1);
        if spec.symbols.iter().any(|(symbol, _)| symbol.chars().next() == Some(c)) {
            return Err(LexError::UnexpectedEndOfOperator { span: here });
        }
        return Err(LexError::UnexpectedCharacter { character: c, span: here });
    }

    Ok(())
}

/// Divide as linhas em tokens segundo `spec`.
pub fn lex<K: Copy>(spec: &LexSpec<K>, input: &[SourceLine]) -> Result<Vec<Token<K>>, LexError> {
    let mut tokens = Vec::new();
    let mut open = None;

    for line in input {
        lex_line(spec, line, &mut tokens, &mut open)?;
    }

    match open {
        Some((line, span)) => Err(LexError::UnterminatedComment { line, span }),
        None => Ok(tokens),
    }
}
