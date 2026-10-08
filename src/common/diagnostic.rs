//! Erros com posição: onde, no fonte, algo deu errado.
//!
//! Os erros de sintaxe (scanner, lexer e parser) implementam [`Diagnostic`]:
//! uma mensagem, uma [`Position`] e se a entrada acabou cedo demais. Com o
//! texto-fonte, [`render`] mostra o trecho com um `^` sob o erro:
//!
//! ```text
//! unexpected character `#`
//!  --> 1:3
//!   |
//! 1 | λx#.x
//!   |   ^
//! ```
//!
//! As posições ([`Span`]) são offsets em `char`s no texto inteiro, então
//! valem também em programas de várias linhas. Erros de tipo e de avaliação
//! ainda não têm posição: os termos não guardam de onde vieram.

use crate::common::Span;

/// Onde um erro aconteceu.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Position {
    /// Num trecho do fonte.
    Span(Span),
    /// No fim da entrada (faltou alguma coisa).
    EndOfInput,
    /// Sem posição conhecida.
    Unknown,
}

pub trait Diagnostic {
    /// A mensagem, sem a posição (que o renderizador acrescenta).
    fn message(&self) -> String;

    fn position(&self) -> Position {
        Position::Unknown
    }

    /// A entrada acabou antes de a construção terminar: mais texto poderia
    /// completá-la. O REPL usa isto para pedir outra linha em vez de falhar.
    fn is_incomplete(&self) -> bool {
        false
    }
}

/// Uma linha e uma coluna, ambas a partir de 1.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}

/// As linhas de um texto, com a posição de cada uma em `char`s.
pub struct SourceMap<'a> {
    lines: Vec<(usize, &'a str)>,
    length: usize,
}

impl<'a> SourceMap<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut lines = Vec::new();
        let mut offset = 0;

        for raw in source.split_inclusive('\n') {
            let text = raw.strip_suffix('\n').unwrap_or(raw);
            let text = text.strip_suffix('\r').unwrap_or(text);
            lines.push((offset, text));
            offset += raw.chars().count();
        }

        Self { lines, length: offset }
    }

    /// O tamanho do texto, em `char`s.
    pub fn len(&self) -> usize {
        self.length
    }

    pub fn is_empty(&self) -> bool {
        self.lines.is_empty()
    }

    pub fn line_count(&self) -> usize {
        self.lines.len()
    }

    /// A linha (a partir de 1) que contém a posição `offset`; uma posição
    /// depois do fim é a última linha.
    pub fn line_of(&self, offset: usize) -> usize {
        match self.lines.iter().rposition(|(start, _)| *start <= offset) {
            Some(index) => index + 1,
            None => 1,
        }
    }

    pub fn line_column(&self, offset: usize) -> LineColumn {
        let line = self.line_of(offset);
        let start = self.lines.get(line - 1).map_or(0, |(start, _)| *start);
        LineColumn { line, column: offset.saturating_sub(start) + 1 }
    }

    /// O texto da linha `line` (a partir de 1), sem o terminador.
    pub fn line_text(&self, line: usize) -> &'a str {
        self.lines.get(line - 1).map_or("", |(_, text)| text)
    }
}

/// O trecho do fonte com um `^` sob o erro, depois da mensagem.
///
/// `shift` é a posição, no `source`, do texto que foi de fato analisado: o
/// interpretador analisa só um pedaço (o termo de `nome = termo`, uma
/// instrução de um programa) e as posições do erro são relativas a ele.
pub fn render(source: &str, shift: usize, diagnostic: &dyn Diagnostic) -> String {
    let message = diagnostic.message();
    let map = SourceMap::new(source);

    if map.is_empty() {
        return message;
    }

    let (line, column, width) = match diagnostic.position() {
        Position::Unknown => return message,

        // depois do último caractere da última linha
        Position::EndOfInput => {
            let line = map.line_count();
            (line, map.line_text(line).chars().count() + 1, 1)
        }

        Position::Span(span) => {
            let start = span.start + shift;
            let end = span.end.max(span.start + 1) + shift;
            let LineColumn { line, column } = map.line_column(start);

            // a seta não passa do fim da linha (nem de um comentário de várias linhas)
            let room = map.line_text(line).chars().count() + 2 - column;
            (line, column, (end - start).min(room).max(1))
        }
    };

    let text = map.line_text(line);
    let line_length = text.chars().count();

    // tabulações ficam tabulações, para a seta alinhar com o texto acima
    let padding: String = text
        .chars()
        .take(column - 1)
        .map(|c| if c == '\t' { '\t' } else { ' ' })
        .chain(std::iter::repeat(' ').take((column - 1).saturating_sub(line_length)))
        .collect();

    let blank = " ".repeat(line.to_string().len());
    format!(
        "{message}\n{blank}--> {line}:{column}\n{blank} |\n{line} | {text}\n{blank} | {padding}{carets}",
        carets = "^".repeat(width),
    )
}
