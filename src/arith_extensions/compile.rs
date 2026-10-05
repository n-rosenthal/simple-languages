//! Compilação de `arith-extensions` para a linguagem de máquina compartilhada.
//!
//! Os operadores são estritos e avaliam a esquerda antes da direita, como
//! na semântica estrutural: `a op b` vira `a; b; prim`. O `if` vira
//! `cond; branch`, e só o ramo escolhido executa.

use std::convert::Infallible;

use crate::common::machine_language::{code, Compile, Instr, Prim, Program, Value as MachineValue};

use super::terms::{BinaryOp, Term};
use super::values::Value;

pub struct ArithCompiler;

fn prim(op: BinaryOp) -> Prim {
    match op {
        BinaryOp::Add => Prim::Add,
        BinaryOp::Sub => Prim::Sub,
        BinaryOp::Mul => Prim::Mul,
        BinaryOp::Div => Prim::Div,
        BinaryOp::Mod => Prim::Mod,
        BinaryOp::LessThan => Prim::Lt,
        BinaryOp::Equal => Prim::Eq,
        BinaryOp::And => Prim::And,
        BinaryOp::Or => Prim::Or,
    }
}

fn go(term: &Term, out: &mut Vec<Instr>) {
    match term {
        Term::Integer(n) => out.push(Instr::Int(*n)),
        Term::Boolean(b) => out.push(Instr::Bool(*b)),

        Term::Zero => out.push(Instr::Int(0)),
        Term::Succ(t) => {
            go(t, out);
            out.push(Instr::Prim(Prim::Succ));
        }
        Term::Pred(t) => {
            go(t, out);
            out.push(Instr::Prim(Prim::Pred));
        }
        Term::IsZero(t) => {
            go(t, out);
            out.push(Instr::Prim(Prim::IsZero));
        }
        
        Term::Binary { op, lhs, rhs } => {
            go(lhs, out);
            go(rhs, out);
            out.push(Instr::Prim(prim(*op)));
        }

        Term::If { condition, then_branch, else_branch } => {
            go(condition, out);

            let (mut then_code, mut else_code) = (Vec::new(), Vec::new());
            go(then_branch, &mut then_code);
            go(else_branch, &mut else_code);

            out.push(Instr::Branch(code(then_code), code(else_code)));
        }
    }
}

impl Compile for ArithCompiler {
    type Source = Term;
    type Value = Value;
    type Error = Infallible;

    fn compile(term: &Term) -> Result<Program, Infallible> {
        let mut out = Vec::new();
        go(term, &mut out);
        Ok(Program::new(out))
    }

    fn corresponds(machine: &MachineValue, source: &Value) -> bool {
        match (machine, source) {
            (MachineValue::Int(a), Value::Integer(b)) => a == b,
            (MachineValue::Int(a), Value::Natural(b)) => {
                *a >= 0 && *a as u64 == *b
            }
            (MachineValue::Bool(a), Value::Boolean(b)) => a == b,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::machine_language::Vm;
    use crate::common::semantics::Machine;

    fn int(n: i64) -> Term {
        Term::integer(n)
    }

    #[test]
    fn binary_operators_compile_to_postfix() {
        let term = Term::binary(BinaryOp::Add, int(1), Term::binary(BinaryOp::Mul, int(2), int(3)));
        let program = ArithCompiler::compile(&term).unwrap();

        assert_eq!(
            program,
            Program::new(vec![
                Instr::Int(1),
                Instr::Int(2),
                Instr::Int(3),
                Instr::Prim(Prim::Mul),
                Instr::Prim(Prim::Add),
            ])
        );
    }

    #[test]
    fn conditionals_compile_to_branches() {
        let term = Term::if_then_else(Term::boolean(true), int(1), int(2));
        let program = ArithCompiler::compile(&term).unwrap();

        assert_eq!(
            program,
            Program::new(vec![
                Instr::Bool(true),
                Instr::Branch(code(vec![Instr::Int(1)]), code(vec![Instr::Int(2)])),
            ])
        );
    }

    #[test]
    fn compiled_programs_run_on_the_vm() {
        let term = Term::binary(BinaryOp::Sub, int(10), int(4));
        let execution = Vm::execute(&ArithCompiler::compile(&term).unwrap());

        assert_eq!(execution.value, Some(MachineValue::Int(6)));
    }

    #[test]
    fn the_untaken_branch_never_runs() {
        // o ramo `else` travaria a máquina, mas nunca executa
        let term = Term::if_then_else(
            Term::boolean(true),
            int(1),
            Term::binary(BinaryOp::Add, Term::boolean(true), int(1)),
        );
        let execution = Vm::execute(&ArithCompiler::compile(&term).unwrap());

        assert_eq!(execution.value, Some(MachineValue::Int(1)));
    }
}
