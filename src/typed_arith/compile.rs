//! Compilação de `typed-arith` para a linguagem de máquina compartilhada.
//!
//! Os numerais são inteiros da máquina e os booleanos, booleanos:
//!
//! ```text
//!   succ t    ⟹  t; 1; add
//!   iszero t  ⟹  t; 0; eq
//!   pred t    ⟹  (closure: #0 == 0 ? 0 : #0 - 1); t; apply
//! ```
//!
//! O `pred` usa uma closure para usar o operando duas vezes sem avaliá-lo
//! duas vezes (a máquina não tem `dup`).

use std::fmt;

use crate::common::machine_language::{code, Compile, Instr, Prim, Program, Value as MachineValue};

use super::terms::Term;

/// Nunca falha: a linguagem não tem variáveis. O tipo existe para o
/// contrato de `Compile`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompileError {}

impl fmt::Display for CompileError {
    fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {}
    }
}

impl std::error::Error for CompileError {}

pub struct TypedArithCompiler;

fn go(term: &Term, out: &mut Vec<Instr>) {
    match term {
        Term::True => out.push(Instr::Bool(true)),
        Term::False => out.push(Instr::Bool(false)),
        Term::Zero => out.push(Instr::Int(0)),
        Term::If { condition, then_branch, else_branch } => {
            go(condition, out);
            let (mut then_code, mut else_code) = (Vec::new(), Vec::new());
            go(then_branch, &mut then_code);
            go(else_branch, &mut else_code);
            out.push(Instr::Branch(code(then_code), code(else_code)));
        }
        Term::Succ(t) => {
            go(t, out);
            out.push(Instr::Int(1));
            out.push(Instr::Prim(Prim::Add));
        }
        Term::IsZero(t) => {
            go(t, out);
            out.push(Instr::Int(0));
            out.push(Instr::Prim(Prim::Eq));
        }
        Term::Pred(t) => {
            let body = vec![
                Instr::Access(0),
                Instr::Int(0),
                Instr::Prim(Prim::Eq),
                Instr::Branch(
                    code(vec![Instr::Int(0)]),
                    code(vec![Instr::Access(0), Instr::Int(1), Instr::Prim(Prim::Sub)]),
                ),
            ];
            out.push(Instr::Closure(code(body)));
            go(t, out);
            out.push(Instr::Apply);
        }
    }
}

impl Compile for TypedArithCompiler {
    type Source = Term;
    type Value = Term;
    type Error = CompileError;

    fn compile(term: &Term) -> Result<Program, CompileError> {
        let mut out = Vec::new();
        go(term, &mut out);
        Ok(Program::new(out))
    }

    fn corresponds(machine: &MachineValue, source: &Term) -> bool {
        match (machine, source) {
            (MachineValue::Bool(b), Term::True) => *b,
            (MachineValue::Bool(b), Term::False) => !*b,
            (MachineValue::Int(n), term) => {
                term.numeral_value().map_or(false, |m| usize::try_from(*n) == Ok(m))
            }
            _ => false,
        }
    }
}
