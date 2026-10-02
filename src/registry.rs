//! As linguagens disponíveis para o CLI e para os testes de integração.
//! Para registrar uma nova linguagem, acrescente uma linha em [`all`].

use crate::common::driver::{Dispatch, Runner};
use crate::lambda::Lambda;

pub fn all() -> Vec<Box<dyn Runner>> {
    vec![Box::new(Dispatch::<Lambda>::new())]
}

pub fn find(name: &str) -> Option<Box<dyn Runner>> {
    all()
        .into_iter()
        .find(|runner| runner.name().eq_ignore_ascii_case(name))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::driver::Command;

    fn run(command: Command, source: &str) -> Result<String, String> {
        find("lambda").expect("lambda is registered").run(command, source)
    }

    #[test]
    fn finds_languages_case_insensitively() {
        assert!(find("Lambda").is_some());
        assert!(find("nope").is_none());
    }

    #[test]
    fn parse_prints_the_term() {
        assert_eq!(run(Command::Parse, "(λx:A. x)  y").unwrap(), "(λx:A. x) y\n");
    }

    #[test]
    fn type_prints_the_type_and_the_derivation() {
        assert_eq!(
            run(Command::Type, "λx:A. x").unwrap(),
            "type: A->A\n⊢ λx:A. x : A->A  [T-Abs]\n  x:A ⊢ x : A  [T-Var]\n"
        );
    }

    #[test]
    fn small_prints_the_trace() {
        assert_eq!(
            run(Command::Small, "(λx:A->A. x) (λy:A. y)").unwrap(),
            "(λx:A->A. x) (λy:A. y)\n→ λy:A. y  [E-AppAbs]\n"
        );
    }

    #[test]
    fn big_starts_with_the_value() {
        let out = run(Command::Big, "(λx:A->A. x) (λy:A. y)").unwrap();
        assert!(out.starts_with("value: λy:A. y\n"));
    }

    #[test]
    fn machine_runs_the_compiled_program() {
        let out = run(Command::Machine, "(λx:A->A. x) (λy:A. y)").unwrap();
        assert!(out.contains("steps: 5, outcome: final\n"));
        assert!(out.ends_with("value: <closure>\n"));
    }

    #[test]
    fn latex_contains_the_derivations() {
        let out = run(Command::Latex, "λx:A. x").unwrap();
        assert!(out.contains(r"\inferrule*") && out.contains("% small-step"));
    }

    #[test]
    fn syntax_errors_are_reported() {
        let error = run(Command::Parse, "λx. x").unwrap_err();
        assert!(error.starts_with("syntax error:"));
    }

    #[test]
    fn full_keeps_going_when_a_stage_fails() {
        // `x` é livre: não tipa, não avalia, não compila, e a execução trava.
        let out = run(Command::Full, "x").unwrap();

        assert!(out.contains("type error: unbound variable `x`"));
        assert!(out.contains("evaluation error: unbound variable `x`"));
        assert!(out.contains("compile error: unbound variable `x`"));
        assert!(out.contains("(stuck)"));
    }

    #[test]
    fn laws_hold_for_a_well_typed_term() {
        assert_eq!(run(Command::Laws, "λx:A. x").unwrap(), "all laws hold\n");
    }
}