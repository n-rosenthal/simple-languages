//! Compilação de `lambda` para a linguagem de máquina compartilhada.
//!
//! Os tipos são apagados e as variáveis viram índices de de Bruijn:
//! `Context::lookup_index` dá exatamente o índice que `Access` espera.

use std::fmt;

use crate::common::machine_language::{code, Compile, Instr, Program, Value as MachineValue};
use crate::common::Context;

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

fn go(term: &Term, ctx: &Context<()>, out: &mut Vec<Instr>) -> Result<(), CompileError> {
    match term {
        Term::Var(name) => {
            let index = ctx
                .lookup_index(name)
                .ok_or_else(|| CompileError::UnboundVariable { name: name.clone() })?;
            out.push(Instr::Access(index));
        }

        // o tipo do parâmetro é apagado
        Term::Lambda { param, body, .. } => {
            let mut inner = Vec::new();
            go(body, &ctx.extend(param.clone(), ()), &mut inner)?;
            out.push(Instr::Closure(code(inner)));
        }

        // convenção do Vm: função, argumento, apply
        Term::App { func, arg } => {
            go(func, ctx, out)?;
            go(arg, ctx, out)?;
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
        go(term, &Context::empty(), &mut out)?;
        Ok(Program::new(out))
    }

    /// Sem constantes, os únicos valores são funções: a máquina devolve
    /// uma closure quando a linguagem devolve uma abstração.
    fn corresponds(machine: &MachineValue, source: &Term) -> bool {
        matches!((machine, source), (MachineValue::Closure(_), Term::Lambda { .. }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::machine_language::Vm;
    use crate::common::semantics::Machine;
    use crate::lambda::testing::parse;

    fn compile(source: &str) -> Result<Program, CompileError> {
        LambdaCompiler::compile(&parse(source))
    }

    #[test]
    fn application_of_identities() {
        let id = || Instr::Closure(code(vec![Instr::Access(0)]));

        assert_eq!(
            compile("(λx:A->A. x) (λy:A. y)").unwrap(),
            Program::new(vec![id(), id(), Instr::Apply])
        );
    }

    #[test]
    fn variables_become_de_bruijn_indices() {
        let inner = |index| {
            Program::new(vec![Instr::Closure(code(vec![Instr::Closure(code(vec![
                Instr::Access(index),
            ]))]))])
        };

        assert_eq!(compile("λx:A. λy:A. x").unwrap(), inner(1));
        assert_eq!(compile("λx:A. λy:A. y").unwrap(), inner(0));
        assert_eq!(compile("λx:A. λx:A. x").unwrap(), inner(0)); // sombreamento
    }

    #[test]
    fn free_variables_fail_to_compile() {
        assert_eq!(
            compile("λx:A. y").unwrap_err(),
            CompileError::UnboundVariable { name: "y".into() }
        );
    }

    #[test]
    fn types_are_erased() {
        assert_eq!(
            compile("λx:A. x").unwrap(),
            compile("λx:(A->B)->C. x").unwrap()
        );
    }

    #[test]
    fn compiled_programs_run_on_the_vm() {
        let program = compile("(λf:A->A. λx:A. f x) (λy:A. y)").unwrap();
        let execution = Vm::execute(&program);

        assert!(execution.trace.is_final());
        assert!(matches!(
            execution.value,
            Some(crate::common::machine_language::Value::Closure(_))
        ));
    }
}
