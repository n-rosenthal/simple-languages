//! Compilação de `stlc` para a linguagem de máquina compartilhada.
//!
//! Os tipos são apagados e as variáveis viram índices de de Bruijn:
//! `Context::lookup_index` dá exatamente o índice que `Access` espera.
//! `true` e `false` são constantes, e o `if` vira `cond; branch`: só o ramo
//! escolhido executa, e ele roda no mesmo ambiente (um `Branch` não
//! estende o ambiente, então os índices não mudam).
//!
//! Termos abertos são rejeitados na compilação, mesmo que a variável livre
//! esteja num ramo que nunca executaria; a semântica natural só falha se o
//! ramo for avaliado. A lei de correção, portanto, vale para termos
//! fechados.

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

pub struct StlcCompiler;

fn go(term: &Term, ctx: &Context<()>, out: &mut Vec<Instr>) -> Result<(), CompileError> {
    match term {
        Term::Var(name) => {
            let index = ctx
                .lookup_index(name)
                .ok_or_else(|| CompileError::UnboundVariable { name: name.clone() })?;
            out.push(Instr::Access(index));
        }

        Term::True => out.push(Instr::Bool(true)),
        Term::False => out.push(Instr::Bool(false)),

        Term::If { condition, then_branch, else_branch } => {
            go(condition, ctx, out)?;

            let (mut then_code, mut else_code) = (Vec::new(), Vec::new());
            go(then_branch, ctx, &mut then_code)?;
            go(else_branch, ctx, &mut else_code)?;

            out.push(Instr::Branch(code(then_code), code(else_code)));
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

impl Compile for StlcCompiler {
    type Source = Term;
    type Value = Term;
    type Error = CompileError;

    fn compile(term: &Term) -> Result<Program, CompileError> {
        let mut out = Vec::new();
        go(term, &Context::empty(), &mut out)?;
        Ok(Program::new(out))
    }

    /// Booleanos correspondem a booleanos; uma closure, a uma abstração
    /// (não há igualdade extensional entre funções).
    fn corresponds(machine: &MachineValue, source: &Term) -> bool {
        match (machine, source) {
            (MachineValue::Closure(_), Term::Lambda { .. }) => true,
            (MachineValue::Bool(b), Term::True) => *b,
            (MachineValue::Bool(b), Term::False) => !*b,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::machine_language::Vm;
    use crate::common::semantics::Machine;
    use crate::stlc::testing::parse;

    fn compile(source: &str) -> Result<Program, CompileError> {
        StlcCompiler::compile(&parse(source))
    }

    fn run(source: &str) -> Option<MachineValue> {
        Vm::execute(&compile(source).unwrap()).value
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
    fn booleans_are_constants() {
        assert_eq!(compile("true").unwrap(), Program::new(vec![Instr::Bool(true)]));
        assert_eq!(compile("false").unwrap(), Program::new(vec![Instr::Bool(false)]));
    }

    #[test]
    fn conditionals_compile_to_branches() {
        assert_eq!(
            compile("if true then false else true").unwrap(),
            Program::new(vec![
                Instr::Bool(true),
                Instr::Branch(code(vec![Instr::Bool(false)]), code(vec![Instr::Bool(true)])),
            ])
        );
    }

    #[test]
    fn branches_run_in_the_same_environment() {
        // λb. λx. if b then x else x: `x` é o índice 0 e `b` é o índice 1 nos dois ramos
        let program = compile("λb:Bool. λx:A. if b then x else b").unwrap();
        let branch = Instr::Branch(
            code(vec![Instr::Access(0)]),
            code(vec![Instr::Access(1)]),
        );
        let expected = Program::new(vec![Instr::Closure(code(vec![Instr::Closure(code(vec![
            Instr::Access(1),
            branch,
        ]))]))]);
        assert_eq!(program, expected);
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
        assert!(matches!(
            run("(λf:A->A. λx:A. f x) (λy:A. y)"),
            Some(MachineValue::Closure(_))
        ));
        assert_eq!(
            run("(λb:Bool. if b then false else true) true"),
            Some(MachineValue::Bool(false))
        );
    }

    #[test]
    fn the_untaken_branch_never_runs() {
        assert_eq!(run("if true then true else (true true)"), Some(MachineValue::Bool(true)));
    }

    #[test]
    fn applying_a_boolean_gets_stuck() {
        assert_eq!(run("true false"), None);
    }
}
