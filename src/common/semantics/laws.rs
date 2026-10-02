//! Propriedades que relacionam semânticas diferentes.
//!
//! Cada função devolve `true` quando a propriedade vale para o termo
//! dado. Testes por linguagem (ou testes de propriedades com `proptest`)
//! as chamam com termos de exemplo; [`crate::common::language::law_violations`]
//! as roda todas de uma vez.

use super::big_step::BigStep;
use super::machine::Machine;
use super::step::{run, Step, Trace};
use super::typing::Typing;

/// Segurança, na forma big-step: se `t` é bem tipado, a avaliação
/// produz um valor (não falha por operandos incompatíveis).
///
/// Vale vacuamente (`true`) para termos mal tipados.
///
/// Observação: em big-step, uma falha de avaliação pode ser travamento
/// ou outro erro do avaliador (estouro, por exemplo). A propriedade só é
/// significativa se `B::Error` representar apenas travamento.
pub fn well_typed_evaluates<T, B>(term: &T::Term) -> bool
where
    T: Typing,
    B: BigStep<Term = T::Term>,
{
    !T::is_well_typed(term) || B::evaluate(term).is_ok()
}

/// Estados finais não dão passos. Sem isto, `is_final` e `step`
/// discordariam sobre o que é um valor.
pub fn final_states_do_not_step<S: Step>(state: &S::State) -> bool {
    !S::is_final(state) || S::step(state).is_none()
}

/// Um trace é uma cadeia consistente: cada passo parte de onde o
/// anterior chegou, e o último chega ao estado final registrado.
pub fn trace_is_connected<S: Step>(trace: &Trace<S>) -> bool
where
    S::State: PartialEq,
{
    let mut expected = &trace.start;
    for transition in &trace.steps {
        if &transition.from != expected {
            return false;
        }
        expected = &transition.to;
    }
    expected == &trace.final_state
}

/// Small-step e big-step concordam: se `t ⇓ v`, a execução small-step
/// termina em `v`; se `t` não tem derivação, ela não termina em valor.
pub fn small_step_agrees_with_big_step<S, B>(term: &S::State) -> bool
where
    S: Step,
    B: BigStep<Term = S::State>,
    B::Value: Into<S::State>,
    S::State: PartialEq,
{
    let trace = run::<S>(term.clone());

    match B::value_of(term) {
        Ok(value) => {
            let expected: S::State = value.into();
            trace.is_final() && trace.final_state == expected
        }
        Err(_) => !trace.is_final(),
    }
}

/// A máquina concorda com a semântica natural: é a *correção* da
/// compilação. Se `t ⇓ v`, a máquina termina com `v`; senão, não produz
/// valor algum.
pub fn machine_agrees_with_big_step<M, B>(term: &M::Term) -> bool
where
    M: Machine,
    B: BigStep<Term = M::Term, Value = M::Value>,
    M::Value: PartialEq,
{
    let execution = M::execute(term);

    match B::value_of(term) {
        Ok(expected) => execution.value.as_ref() == Some(&expected),
        Err(_) => execution.value.is_none(),
    }
}

/// Progresso: um termo bem tipado nunca fica travado (pode terminar ou
/// divergir). Vale vacuamente para termos mal tipados.
pub fn well_typed_never_gets_stuck<T, S>(term: &T::Term) -> bool
where
    T: Typing,
    S: Step<State = T::Term>,
{
    !T::is_well_typed(term) || !run::<S>(term.clone()).is_stuck()
}

/// Preservação: se `t : T` e `t → t'`, então `t' : T`.
pub fn preservation<T, S>(term: &T::Term) -> bool
where
    T: Typing,
    S: Step<State = T::Term>,
    T::Type: PartialEq,
{
    let Ok(before) = T::type_of(term) else {
        return true;
    };

    match S::step(term) {
        None => true,
        Some(transition) => T::type_of(&transition.to).map_or(false, |after| after == before),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::semantics::toy::*;

    fn samples() -> Vec<Term> {
        vec![
            num(1),
            boolean(false),
            add(num(1), num(2)),
            add(add(num(1), num(2)), num(3)),
            add(num(1), add(num(2), add(num(3), num(4)))),
            add(num(1), boolean(true)),
            add(boolean(true), add(num(1), num(2))),
            add(add(num(1), num(2)), boolean(true)),
        ]
    }

    #[test]
    fn toy_language_is_safe() {
        for t in &samples() {
            assert!(well_typed_evaluates::<ToyTyping, ToyBigStep>(t), "failed on {t}");
        }
    }

    #[test]
    fn progress_and_preservation_on_the_toy_language() {
        for t in &samples() {
            assert!(well_typed_never_gets_stuck::<ToyTyping, ToySmallStep>(t), "{t}");
            assert!(preservation::<ToyTyping, ToySmallStep>(t), "{t}");
        }
    }

    #[test]
    fn final_states_do_not_step_in_both_systems() {
        for t in samples() {
            assert!(final_states_do_not_step::<ToySmallStep>(&t), "small-step on {t}");
            assert!(
                final_states_do_not_step::<ToyMachine>(&ToyMachine::load(&t)),
                "machine on {t}"
            );
        }
    }

    #[test]
    fn traces_are_connected() {
        for t in samples() {
            assert!(trace_is_connected(&run::<ToySmallStep>(t.clone())), "small-step on {t}");
            assert!(
                trace_is_connected(&run::<ToyMachine>(ToyMachine::load(&t))),
                "machine on {t}"
            );
        }
    }

    #[test]
    fn small_step_agrees_with_big_step_on_the_toy_language() {
        for t in samples() {
            assert!(small_step_agrees_with_big_step::<ToySmallStep, ToyBigStep>(&t), "{t}");
        }
    }

    #[test]
    fn the_machine_is_correct_with_respect_to_big_step() {
        for t in samples() {
            assert!(machine_agrees_with_big_step::<ToyMachine, ToyBigStep>(&t), "{t}");
        }
    }
}
