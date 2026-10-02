//! Máquinas abstratas.
//!
//! Uma máquina é um [`Step`] cujo estado é uma *configuração* (código,
//! pilha, ambiente, memória, ...) em vez de um termo. [`Machine`] só
//! acrescenta a ponte entre os dois mundos:
//!
//! - [`Machine::load`]: termo → configuração inicial (a "compilação");
//! - [`Machine::unload`]: configuração final → valor.
//!
//! Com isso é possível verificar a correção da máquina contra a semântica
//! natural (ver `laws::machine_agrees_with_big_step`).
//!
//! Máquinas com referências (cap. 13) guardam uma [`crate::common::Store`]
//! dentro do próprio estado.

use super::step::{run_with_fuel, Step, Trace, DEFAULT_FUEL};

/// O resultado de executar um termo em uma máquina.
pub struct Execution<M: Machine> {
    pub trace: Trace<M>,
    /// `Some` só se a execução terminou em um estado final do qual
    /// `unload` extrai um valor.
    pub value: Option<M::Value>,
}

pub trait Machine: Step {
    type Term;
    type Value;

    /// A configuração inicial para `term`.
    fn load(term: &Self::Term) -> Self::State;

    /// O valor de uma configuração final. `None` para uma configuração
    /// que não é final, ou final mas malformada (pilha com sobras, por
    /// exemplo, que normalmente indica um erro na máquina).
    fn unload(state: &Self::State) -> Option<Self::Value>;

    /// Carrega `term`, executa e extrai o valor.
    fn execute(term: &Self::Term) -> Execution<Self>
    where
        Self: Sized,
    {
        Self::execute_with_fuel(term, DEFAULT_FUEL)
    }

    fn execute_with_fuel(term: &Self::Term, fuel: usize) -> Execution<Self>
    where
        Self: Sized,
    {
        let trace = run_with_fuel::<Self>(Self::load(term), fuel);

        let value = if trace.is_final() {
            Self::unload(&trace.final_state)
        } else {
            None
        };

        Execution { trace, value }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::semantics::toy::*;

    #[test]
    fn load_compiles_to_postfix_code() {
        let state = ToyMachine::load(&add(num(1), num(2)));

        assert_eq!(
            state.code,
            vec![Instr::Push(Value::Num(1)), Instr::Push(Value::Num(2)), Instr::Add]
        );
        assert!(state.stack.is_empty());
    }

    #[test]
    fn executes_to_a_value() {
        let run = ToyMachine::execute(&add(num(1), num(2)));

        assert_eq!(run.value, Some(Value::Num(3)));
        assert!(run.trace.is_final());
        assert_eq!(
            run.trace.rules(),
            vec![MachineRule::Push, MachineRule::Push, MachineRule::Add]
        );
    }

    #[test]
    fn a_literal_is_one_push() {
        let run = ToyMachine::execute(&num(7));
        assert_eq!(run.value, Some(Value::Num(7)));
        assert_eq!(run.trace.len(), 1);
    }

    #[test]
    fn ill_typed_code_gets_stuck() {
        let run = ToyMachine::execute(&add(num(1), boolean(true)));

        assert!(run.trace.is_stuck());
        assert_eq!(run.value, None);
    }

    #[test]
    fn unload_rejects_non_final_configurations() {
        let initial = ToyMachine::load(&add(num(1), num(2)));
        assert_eq!(ToyMachine::unload(&initial), None);
    }

    #[test]
    fn the_trace_renders_configurations() {
        let run = ToyMachine::execute(&num(1));
        assert_eq!(run.trace.to_text(), "⟨[push 1], []⟩\n→ ⟨[], [1]⟩  [M-Push]\n");
    }
}
