//! Compilação de `nameless_terms` (call-by-value) para a linguagem de máquina
//! compartilhada.
//!
//! Os termos já utilizam índices de De Bruijn:
//! - variável: `Access(index)`;
//! - abstração: `Closure(código_do_corpo)`;
//! - aplicação: `função; argumento; Apply`.
//!
//! Termos abertos são rejeitados na compilação.

use std::fmt;

use crate::common::machine_language::{
    code, Compile, Instr, Program, Value as MachineValue,
};

use super::terms::Term;

/// Erros encontrados durante a compilação.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompileError {
    /// O índice de De Bruijn não está ligado por uma abstração
    /// no contexto atual.
    UnboundVariable {
        index: usize,
        depth: usize,
    },
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnboundVariable { index, depth } => write!(
                f,
                "unbound De Bruijn index `{index}` at context depth {depth}"
            ),
        }
    }
}

impl std::error::Error for CompileError {}

pub struct NamelessTermsCompiler;

/// Compila um termo, verificando se todos os índices de De Bruijn
/// são válidos no contexto atual.
///
/// `depth` representa o número de abstrações que estão em escopo.
fn go(
    term: &Term,
    depth: usize,
    out: &mut Vec<Instr>,
) -> Result<(), CompileError> {
    match term {
        Term::Var(index) => {
            if *index >= depth {
                return Err(CompileError::UnboundVariable {
                    index: *index,
                    depth,
                });
            }

            out.push(Instr::Access(*index));
        }

        Term::Abs(body) => {
            // O corpo da abstração é compilado como código de uma closure.
            let mut inner = Vec::new();

            go(
                body,
                depth.checked_add(1).ok_or(
                    CompileError::UnboundVariable {
                        index: usize::MAX,
                        depth,
                    },
                )?,
                &mut inner,
            )?;

            out.push(Instr::Closure(code(inner)));
        }

        Term::App(func, arg) => {
            // A máquina utiliza a ordem call-by-value:
            // primeiro a função, depois o argumento e, por fim, Apply.
            go(func, depth, out)?;
            go(arg, depth, out)?;
            out.push(Instr::Apply);
        }
    }

    Ok(())
}

impl Compile for NamelessTermsCompiler {
    type Source = Term;
    type Value = Term;
    type Error = CompileError;

    fn compile(term: &Term) -> Result<Program, CompileError> {
        let mut out = Vec::new();

        go(term, 0, &mut out)?;

        Ok(Program::new(out))
    }

    /// Uma closure corresponde a uma abstração.
    ///
    /// A correspondência é estrutural apenas no nível do construtor:
    /// não afirma igualdade extensional entre funções.
    fn corresponds(machine: &MachineValue, source: &Term) -> bool {
        matches!(
            (machine, source),
            (MachineValue::Closure(_), Term::Abs(_))
        )
    }
}