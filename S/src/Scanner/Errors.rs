//! S/src/Scanner/Errors.rs
//!
//! Definição dos erros possíveis de ocorrência durante a execução
//! do módulo Scanner.
//!
//! author:  n-rosenthal
//! date:    2026-08-17
//! version: 0.2 (with thiserror)

use std::io;
use std::path::PathBuf;
use thiserror::Error;

/// Erros possíveis na leitura de um arquivo-fonte.
#[derive(Error, Debug)]
pub enum ScannerError {
    #[error("erro de I/O: {0}")]
    Io(#[from] io::Error),

    #[error("arquivo vazio: {}", .0.display())]
    EmptyFile(PathBuf),
}
