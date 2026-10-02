//! Semântica estrutural de `lambda`: call-by-value, esquerda para a
//! direita (TAPL, cap. 5).
//!
//! ```text
//!   t1 → t1'                 ⟹  t1 t2 → t1' t2             (E-App1)
//!   v1 valor, t2 → t2'       ⟹  v1 t2 → v1 t2'             (E-App2)
//!   v2 valor                 ⟹  (λx:T. t) v2 → [x ↦ v2] t  (E-AppAbs)
//! ```
//!
//! Os valores são as abstrações. Uma variável livre é uma forma normal
//! que não é valor: um termo travado.

use crate::common::semantics::{Step, Transition};

use super::terms::Term;

crate::rules! {
    pub enum StepRule {
        App1 => "E-App1",
        App2 => "E-App2",
        AppAbs => "E-AppAbs",
    }
}

pub struct LambdaSmallStep;

impl Step for LambdaSmallStep {
    type State = Term;
    type Rule = StepRule;

    fn is_final(term: &Term) -> bool {
        term.is_value()
    }

    fn step(term: &Term) -> Option<Transition<StepRule, Term>> {
        let Term::App { func, arg } = term else {
            return None; // abstração (valor) ou variável livre (travado)
        };

        // E-App1: reduz a função primeiro
        if !Self::is_final(func) {
            let inner = Self::step(func)?; // função travada: o termo todo trava
            let next = Term::app(inner.to, (**arg).clone());
            return Some(Transition::new(StepRule::App1, term.clone(), next));
        }

        // E-App2: a função já é valor, reduz o argumento
        if !Self::is_final(arg) {
            let inner = Self::step(arg)?;
            let next = Term::app((**func).clone(), inner.to);
            return Some(Transition::new(StepRule::App2, term.clone(), next));
        }

        // E-AppAbs: (λx:T. t) v → [x ↦ v] t
        match &**func {
            Term::Lambda { param, body, .. } => Some(Transition::new(
                StepRule::AppAbs,
                term.clone(),
                body.substitute(param, arg),
            )),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::semantics::{run, run_with_fuel};
    use crate::lambda::testing::parse;

    #[test]
    fn beta_reduction() {
        let t = parse("(λx:A->A. x) (λy:A. y)");
        let step = LambdaSmallStep::step(&t).unwrap();

        assert_eq!(step.rule, StepRule::AppAbs);
        assert_eq!(step.to, parse("λy:A. y"));
    }

    #[test]
    fn abstractions_are_final() {
        let trace = run::<LambdaSmallStep>(parse("λx:A. x"));
        assert!(trace.is_final() && trace.is_empty());
    }

    #[test]
    fn the_argument_is_reduced_before_the_call() {
        // CBV: o argumento (λg. g) (λy. y) é reduzido antes do β externo
        let t = parse("(λf:A->A. λx:A. f x) ((λg:A->A. g) (λy:A. y))");
        let trace = run::<LambdaSmallStep>(t);

        assert!(trace.is_final());
        assert_eq!(trace.rules(), vec![StepRule::App2, StepRule::AppAbs]);
        assert_eq!(trace.final_state, parse("λx:A. (λy:A. y) x"));
    }

    #[test]
    fn the_function_is_reduced_first() {
        let t = parse("((λf:(A->A)->A->A. f) (λg:A->A. g)) (λz:A. z)");
        let trace = run::<LambdaSmallStep>(t);

        assert_eq!(trace.rules(), vec![StepRule::App1, StepRule::AppAbs]);
        assert_eq!(trace.final_state, parse("λz:A. z"));
    }

    #[test]
    fn free_variables_are_stuck() {
        for source in ["x", "x (λy:A. y)", "(λy:A. y) x"] {
            let trace = run::<LambdaSmallStep>(parse(source));
            assert!(trace.is_stuck(), "{source}");
        }
    }

    #[test]
    fn omega_diverges() {
        let omega = parse("(λx:A. x x) (λx:A. x x)");
        let trace = run_with_fuel::<LambdaSmallStep>(omega.clone(), 100);

        assert!(trace.is_out_of_fuel());
        assert_eq!(trace.len(), 100);
        assert_eq!(trace.final_state, omega); // reduz a si mesmo
    }

    #[test]
    fn substitution_does_not_capture_free_variables() {
        // (λx:A. λy:A. x) y: `y` é livre e não é valor, então a execução trava
        // antes da substituição.
        assert!(LambdaSmallStep::step(&parse("(λx:A. λy:A. x) y")).is_none());

        // A captura só aparece com um argumento que é valor e tem variável
        // livre: uma abstração aberta.
        let t = Term::app(parse("λx:A. λy:A. x"), parse("λz:A. y"));
        let step = LambdaSmallStep::step(&t).unwrap();
        assert_eq!(step.to, parse("λy1:A. λz:A. y"));
    }
}
