//! Semântica estrutural de `stlc`: call-by-value, esquerda para a direita
//! (TAPL, caps. 5 e 9).
//!
//! ```text
//!   t1 → t1'                 ⟹  t1 t2 → t1' t2              (E-App1)
//!   v1 valor, t2 → t2'       ⟹  v1 t2 → v1 t2'              (E-App2)
//!   v2 valor                 ⟹  (λx:T. t) v2 → [x ↦ v2] t   (E-AppAbs)
//!
//!   if true then t2 else t3  →  t2                           (E-IfTrue)
//!   if false then t2 else t3 →  t3                           (E-IfFalse)
//!   t1 → t1'  ⟹  if t1 then t2 else t3 → if t1' then t2 else t3   (E-If)
//! ```
//!
//! Os valores são `true`, `false` e as abstrações. Uma variável livre, ou um
//! `if` cuja condição é uma abstração, é uma forma normal que não é valor:
//! um termo travado.

use crate::common::semantics::{Step, Transition};

use super::terms::Term;

crate::rules! {
    pub enum StepRule {
        App1 => "E-App1",
        App2 => "E-App2",
        AppAbs => "E-AppAbs",
        If => "E-If",
        IfTrue => "E-IfTrue",
        IfFalse => "E-IfFalse",
    }
}

pub struct StlcSmallStep;

impl StlcSmallStep {
    /// A regra mais externa e o termo seguinte, sem montar a `Transition`
    /// (que clonaria o termo em cada nível da recursão de congruência:
    /// O(n²) por passo numa cadeia `f x y z ...`).
    fn reduce(term: &Term) -> Option<(StepRule, Term)> {
        match term {
            // valores e variáveis livres: nenhuma regra
            Term::Var(_) | Term::True | Term::False | Term::Lambda { .. } => None,

            Term::If { condition, then_branch, else_branch } => match **condition {
                // E-IfTrue / E-IfFalse
                Term::True => Some((StepRule::IfTrue, (**then_branch).clone())),
                Term::False => Some((StepRule::IfFalse, (**else_branch).clone())),
                // condição que é função: travado
                Term::Lambda { .. } => None,
                // E-If: reduz a condição
                _ => {
                    let (_, inner) = Self::reduce(condition)?;
                    let next = Term::if_then_else(
                        inner,
                        (**then_branch).clone(),
                        (**else_branch).clone(),
                    );
                    Some((StepRule::If, next))
                }
            },

            Term::App { func, arg } => {
                // E-App1: reduz a função primeiro
                if !func.is_value() {
                    let (_, inner) = Self::reduce(func)?; // função travada: o termo todo trava
                    return Some((StepRule::App1, Term::app(inner, (**arg).clone())));
                }

                // E-App2: a função já é valor, reduz o argumento
                if !arg.is_value() {
                    let (_, inner) = Self::reduce(arg)?;
                    return Some((StepRule::App2, Term::app((**func).clone(), inner)));
                }

                // E-AppAbs: (λx:T. t) v → [x ↦ v] t
                match &**func {
                    Term::Lambda { param, body, .. } => {
                        Some((StepRule::AppAbs, body.substitute(param, arg)))
                    }
                    _ => None, // `true v`: travado
                }
            }
        }
    }
}

impl Step for StlcSmallStep {
    type State = Term;
    type Rule = StepRule;

    fn is_final(term: &Term) -> bool {
        term.is_value()
    }

    fn step(term: &Term) -> Option<Transition<StepRule, Term>> {
        let (rule, next) = Self::reduce(term)?;
        Some(Transition::new(rule, term.clone(), next))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::semantics::{run, run_with_fuel};
    use crate::stlc::testing::parse;

    #[test]
    fn beta_reduction() {
        let t = parse("(λx:A->A. x) (λy:A. y)");
        let step = StlcSmallStep::step(&t).unwrap();

        assert_eq!(step.rule, StepRule::AppAbs);
        assert_eq!(step.to, parse("λy:A. y"));
    }

    #[test]
    fn values_are_final() {
        for source in ["λx:A. x", "true", "false"] {
            let trace = run::<StlcSmallStep>(parse(source));
            assert!(trace.is_final() && trace.is_empty(), "{source}");
        }
    }

    #[test]
    fn conditionals_on_literals() {
        let t = parse("if true then false else true");
        let step = StlcSmallStep::step(&t).unwrap();
        assert_eq!((step.rule, step.to), (StepRule::IfTrue, parse("false")));

        let t = parse("if false then false else true");
        let step = StlcSmallStep::step(&t).unwrap();
        assert_eq!((step.rule, step.to), (StepRule::IfFalse, parse("true")));
    }

    #[test]
    fn the_condition_is_reduced_first() {
        // if (not true) then a else b  →  if false then a else b  →  b
        let t = parse("if (λb:Bool. if b then false else true) true then x else y");
        let trace = run::<StlcSmallStep>(t);

        // o trace guarda a regra mais externa de cada passo: os dois primeiros
        // passos reduzem a condição (E-If), e o terceiro escolhe o ramo
        assert_eq!(
            trace.rules(),
            vec![StepRule::If, StepRule::If, StepRule::IfFalse]
        );
        assert_eq!(trace.final_state, parse("y"));
    }

    #[test]
    fn only_the_chosen_branch_is_reduced() {
        // o ramo `else` é um termo travado, mas nunca é tocado
        let trace = run::<StlcSmallStep>(parse("if true then false else (true true)"));
        assert!(trace.is_final());
        assert_eq!(trace.final_state, parse("false"));
    }

    #[test]
    fn the_argument_is_reduced_before_the_call() {
        // CBV: o argumento (λg. g) (λy. y) é reduzido antes do β externo
        let t = parse("(λf:A->A. λx:A. f x) ((λg:A->A. g) (λy:A. y))");
        let trace = run::<StlcSmallStep>(t);

        assert!(trace.is_final());
        assert_eq!(trace.rules(), vec![StepRule::App2, StepRule::AppAbs]);
        assert_eq!(trace.final_state, parse("λx:A. (λy:A. y) x"));
    }

    #[test]
    fn call_by_value_evaluates_a_conditional_argument_first() {
        // (λb:Bool. b) (if true then false else true)
        let trace = run::<StlcSmallStep>(parse("(λb:Bool. b) (if true then false else true)"));

        assert_eq!(trace.rules(), vec![StepRule::App2, StepRule::AppAbs]);
        assert_eq!(trace.final_state, parse("false"));
    }

    #[test]
    fn the_function_is_reduced_first() {
        let t = parse("((λf:(A->A)->A->A. f) (λg:A->A. g)) (λz:A. z)");
        let trace = run::<StlcSmallStep>(t);

        assert_eq!(trace.rules(), vec![StepRule::App1, StepRule::AppAbs]);
        assert_eq!(trace.final_state, parse("λz:A. z"));
    }

    #[test]
    fn stuck_terms() {
        for source in [
            "x",
            "x (λy:A. y)",
            "(λy:A. y) x",
            "true false",                  // aplicar um booleano
            "if (λx:Bool. x) then true else false", // condição que é função
            "if x then true else false",   // condição que é variável livre
        ] {
            let trace = run::<StlcSmallStep>(parse(source));
            assert!(trace.is_stuck(), "{source}");
        }
    }

    #[test]
    fn stuck_terms_are_not_values() {
        let t = parse("true false");
        assert!(!StlcSmallStep::is_final(&t));
        assert!(StlcSmallStep::step(&t).is_none());
    }

    #[test]
    fn omega_diverges() {
        let omega = parse("(λx:A. x x) (λx:A. x x)");
        let trace = run_with_fuel::<StlcSmallStep>(omega.clone(), 100);

        assert!(trace.is_out_of_fuel());
        assert_eq!(trace.len(), 100);
        assert_eq!(trace.final_state, omega); // reduz a si mesmo
    }

    #[test]
    fn substitution_does_not_capture_free_variables() {
        // (λx:A. λy:A. x) y: `y` é livre e não é valor, então a execução trava
        // antes da substituição.
        assert!(StlcSmallStep::step(&parse("(λx:A. λy:A. x) y")).is_none());

        // A captura só aparece com um argumento que é valor e tem variável
        // livre: uma abstração aberta.
        let t = Term::app(parse("λx:A. λy:A. x"), parse("λz:A. y"));
        let step = StlcSmallStep::step(&t).unwrap();
        assert_eq!(step.to, parse("λy1:A. λz:A. y"));
    }
}
