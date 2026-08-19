//! S/src/Scanner/Types.rs
//!
//! Definições dos tipos necessários para implementação
//! do scanner para linguagem S.
//!
//! author:  n-rosenthal
//! date:    2026-08-17
//! version: 0.4

use std::fs;
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

/// Tipos de `S`.
///
/// ```text
/// type := Integer | Boolean | Unit | Ref type
/// ```
///
/// `Reference` não deriva `Copy` (carrega um `Box<Type>` recursivo),
/// então em todo o type-checker usamos `.clone()` explicitamente
/// sempre que precisamos de uma cópia independente.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Integer,
    Boolean,
    Unit,
    Reference(Box<Type>),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Integer => write!(f, "Integer"),
            Type::Boolean => write!(f, "Boolean"),
            Type::Unit => write!(f, "Unit"),
            Type::Reference(inner) => write!(f, "Ref({inner})"),
        }
    }
}

/// Operadores binários de `S`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let symbol = match self {
            BinaryOperator::Add => "+",
            BinaryOperator::Sub => "-",
            BinaryOperator::Eq => "==",
            BinaryOperator::Neq => "<>",
            BinaryOperator::Lt => "<",
            BinaryOperator::Leq => "<=",
            BinaryOperator::Gt => ">",
            BinaryOperator::Geq => ">=",
        };
        write!(f, "{symbol}")
    }
}

/// Operadores unários de `S`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Not,
}

impl std::fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let symbol = match self {
            UnaryOperator::Not => "not",
        };
        write!(f, "{symbol}")
    }
}

/// Um termo da linguagem `S`, conforme a gramática:
///
/// ```text
/// term := LiteralInteger   n
///      |  LiteralBoolean   b
///      |  Unit
///      |  Variable         name
///      |  BinaryOperation  BinaryOperator, term, term
///      |  UnaryOperation   UnaryOperator, term
///      |  Conditional      term, term, term
///      |  Let               name, type, term, term
///      |  Ref               term            (* aloca memória *)
///      |  Deref             term            (* !t, lê memória *)
///      |  Assign            term, term      (* t := t, escreve memória *)
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    LiteralInteger(i64),
    LiteralBoolean(bool),
    Unit,
    Variable(String),
    BinaryOperation {
        operator: BinaryOperator,
        left: Box<Term>,
        right: Box<Term>,
    },
    UnaryOperation {
        operator: UnaryOperator,
        operand: Box<Term>,
    },
    Conditional {
        condition: Box<Term>,
        then_branch: Box<Term>,
        else_branch: Box<Term>,
    },
    Let {
        name: String,
        declared_type: Type,
        value: Box<Term>,
        body: Box<Term>,
    },
    Ref(Box<Term>),
    Deref(Box<Term>),
    Assign {
        target: Box<Term>,
        value: Box<Term>,
    },
}

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::LiteralInteger(n) => write!(f, "{n}"),
            Term::LiteralBoolean(b) => write!(f, "{b}"),
            Term::Unit => write!(f, "unit"),
            Term::Variable(name) => write!(f, "{name}"),
            Term::BinaryOperation { operator, left, right } => {
                write!(f, "({left} {operator} {right})")
            }
            Term::UnaryOperation { operator, operand } => {
                write!(f, "({operator} {operand})")
            }
            Term::Conditional { condition, then_branch, else_branch } => {
                write!(f, "(if {condition} then {then_branch} else {else_branch})")
            }
            Term::Let { name, declared_type, value, body } => {
                write!(f, "(let {name}: {declared_type} = {value} in {body})")
            }
            Term::Ref(inner) => write!(f, "(ref {inner})"),
            Term::Deref(inner) => write!(f, "(!{inner})"),
            Term::Assign { target, value } => write!(f, "({target} := {value})"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    fn write_temp_file(content: &str) -> PathBuf {
        let path = std::env::temp_dir().join(format!(
            "s-scanner-test-{}-{}.s",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));

        let mut file =
            fs::File::create(&path).expect("não foi possível criar arquivo temporário");
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

    #[test]
    fn display_de_type_reference_e_recursivo() {
        let ty = Type::Reference(Box::new(Type::Reference(Box::new(Type::Integer))));
        assert_eq!(ty.to_string(), "Ref(Ref(Integer))");
    }
}
