//! Compilação de `lambda` (call-by-value) para a linguagem de máquina
//! compartilhada: as variáveis viram índices de de Bruijn e a aplicação,
//! `função; argumento; apply`.
//!
//! Termos abertos são rejeitados na compilação.

use std::fmt;

use crate::common::machine_language::{code, Compile, Instr, Program, Value as MachineValue};

use super::terms::Term;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompileError {
    UnboundVariable { name: String },
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnboundVariable { name } => write!(f, "unbound variable `{name}`"),
        }
    }
}

impl std::error::Error for CompileError {}

pub struct LambdaCompiler;

/// `names` guarda os binders em escopo, do mais externo ao mais interno:
/// o índice de de Bruijn é a distância até o último.
fn go(term: &Term, names: &mut Vec<String>, out: &mut Vec<Instr>) -> Result<(), CompileError> {
    match term {
        Term::Var(name) => {
            let position = names
                .iter()
                .rposition(|bound| bound == name)
                .ok_or_else(|| CompileError::UnboundVariable { name: name.clone() })?;
            out.push(Instr::Access(names.len() - 1 - position));
        }
        Term::Lambda { param, body } => {
            let mut inner = Vec::new();
            names.push(param.clone());
            let result = go(body, names, &mut inner);
            names.pop();
            result?;
            out.push(Instr::Closure(code(inner)));
        }
        Term::App { func, arg } => {
            go(func, names, out)?;
            go(arg, names, out)?;
            out.push(Instr::Apply);
        }
    }
    Ok(())
}

impl Compile for LambdaCompiler {
    type Source = Term;
    type Value = Term;
    type Error = CompileError;

    fn compile(term: &Term) -> Result<Program, CompileError> {
        let mut out = Vec::new();
        go(term, &mut Vec::new(), &mut out)?;
        Ok(Program::new(out))
    }

    /// Uma closure corresponde a uma abstração (não há igualdade
    /// extensional entre funções).
    fn corresponds(machine: &MachineValue, source: &Term) -> bool {
        matches!((machine, source), (MachineValue::Closure(_), Term::Lambda { .. }))
    }
}
