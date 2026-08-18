//! S/src/Scanner/Types.rs
//!
//! Definições dos tipos necessários para implementação
//! do scanner para linguagem S.
//!
//! author:  n-rosenthal
//! date:    2026-08-17
//! version: 0.1

use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use crate::Scanner::Errors::ScannerError;

/// Representa um arquivo-fonte de `S` já lido em memória.
#[derive(Debug, Clone)]
pub struct SourceFile {
    /// Caminho original do arquivo.
    pub path: PathBuf,

    /// Conteúdo bruto do arquivo, sem modificações.
    pub content: String,

    /// Conteúdo quebrado em linhas (sem o `\n` final de cada uma).
    pub lines: Vec<String>,
}

/// Tipos de `S`, conforme a gramática: `type := Integer | Boolean`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Integer,
    Boolean,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Type::Integer => "Integer",
            Type::Boolean => "Boolean",
        };
        write!(f, "{name}")
    }
}

impl SourceFile {
    /// Lê o arquivo em `path`, quebra o conteúdo em linhas e retorna
    /// um `SourceFile` pronto para as próximas etapas (lexer/parser).
    ///
    /// # Erros
    /// Retorna `ScannerError::Io` se o arquivo não puder ser lido, ou
    /// `ScannerError::EmptyFile` se o conteúdo lido for vazio.
    pub fn read<P: AsRef<Path>>(path: P) -> Result<SourceFile, ScannerError> {
        let path = path.as_ref().to_path_buf();

        let content = fs::read_to_string(&path)?;

        if content.trim().is_empty() {
            return Err(ScannerError::EmptyFile(path));
        }

        let lines: Vec<String> = content.lines().map(str::to_string).collect();

        Ok(SourceFile {
            path,
            content,
            lines,
        })
    }

    /// Número total de linhas do arquivo-fonte.
    pub fn line_count(&self) -> usize {
        self.lines.len()
    }

    /// Retorna a linha de índice `n` (0-indexado), se existir.
    pub fn line(&self, n: usize) -> Option<&str> {
        self.lines.get(n).map(String::as_str)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    /// Escreve `content` em um arquivo temporário único e devolve seu
    /// caminho. O arquivo é fechado (via `drop`) antes de retornar,
    /// garantindo que `SourceFile::read` consiga reabri-lo sem
    /// conflito de handle, inclusive no Windows.
    fn write_temp_file(content: &str) -> PathBuf {
        let path = std::env::temp_dir().join(format!(
            "s-scanner-test-{}-{}.s",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));

        let mut file = fs::File::create(&path).expect("não foi possível criar arquivo temporário");
        write!(file, "{content}").expect("não foi possível escrever no arquivo temporário");
        drop(file);

        path
    }

    #[test]
    fn le_arquivo_e_quebra_em_linhas() {
        let path = write_temp_file("1 + 2\ntrue == false\n");

        let src = SourceFile::read(&path).expect("leitura deveria funcionar");

        assert_eq!(src.line_count(), 2);
        assert_eq!(src.line(0), Some("1 + 2"));
        assert_eq!(src.line(1), Some("true == false"));
        assert_eq!(src.line(2), None);

        let _ = fs::remove_file(&path);
    }

    #[test]
    fn arquivo_inexistente_retorna_erro_io() {
        let result = SourceFile::read("/caminho/que/nao/existe.s");
        assert!(matches!(result, Err(ScannerError::Io(_))));
    }

    #[test]
    fn arquivo_vazio_retorna_erro_dedicado() {
        let path = write_temp_file("");
        let result = SourceFile::read(&path);
        assert!(matches!(result, Err(ScannerError::EmptyFile(_))));
        let _ = fs::remove_file(&path);
    }
}

/// Operadores binários de `S`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Eq,
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let symbol = match self {
            BinaryOperator::Add => "+",
            BinaryOperator::Sub => "-",
            BinaryOperator::Eq  => "==",
        };
        write!(f, "{symbol}")
    }
}

/// Um termo da linguagem `S`, conforme a gramática:
///
/// ```text
/// term := LiteralInteger   n
///      |  LiteralBoolean   b
///      |  BinaryOperation  BinaryOperator, term, term
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    LiteralInteger(i64),
    LiteralBoolean(bool),
    BinaryOperation {
        operator: BinaryOperator,
        left: Box<Term>,
        right: Box<Term>,
    },
}

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::LiteralInteger(n) => write!(f, "{n}"),
            Term::LiteralBoolean(b) => write!(f, "{b}"),
            Term::BinaryOperation { operator, left, right } => {
                write!(f, "({left} {operator} {right})")
            }
        }
    }
}
