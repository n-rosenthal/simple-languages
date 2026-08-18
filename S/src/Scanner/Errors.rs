//! S/src/Scanner/Errors.rs
//!
//! Definição dos erros possíveis de ocorrência durante a execução
//! do módulo Scanner.
//!
//! author:  n-rosenthal
//! date:    2026-08-17
//! version: 0.1

use std::io;
use std::path::PathBuf;

/// Erros possíveis na leitura de um arquivo-fonte.
#[derive(Debug)]
pub enum ScannerError {
    /// Falha de I/O ao abrir ou ler o arquivo (caminho inválido, sem
    /// permissão, etc.).
    Io(io::Error),

    /// O arquivo existe e foi lido, mas está vazio.
    EmptyFile(PathBuf),
}

impl std::fmt::Display for ScannerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ScannerError::Io(err) => write!(f, "erro de I/O: {err}"),
            ScannerError::EmptyFile(path) => {
                write!(f, "arquivo vazio: {}", path.display())
            }
        }
    }
}

impl std::error::Error for ScannerError {}

impl From<io::Error> for ScannerError {
    fn from(err: io::Error) -> Self {
        ScannerError::Io(err)
    }
}
