//! Sistemas de transição: o julgamento `s → s'`.
//!
//! [`Step`] descreve *um* passo. Para a semântica estrutural (small-step)
//! o estado é o próprio termo; para uma máquina abstrata (ver
//! [`super::machine`]) é uma configuração. O driver [`run`] itera os
//! passos e diz como a execução terminou:
//!
//! - [`Outcome::Final`]: chegou a um estado final (um valor, ou uma
//!   máquina que parou);
//! - [`Outcome::Stuck`]: não é final e nenhuma regra se aplica. Um termo
//!   travado é uma forma normal que não é valor (TAPL, cap. 3), um
//!   resultado *normal*, não um erro;
//! - [`Outcome::OutOfFuel`]: o limite de passos acabou (provável
//!   divergência).
//!
//! O contraste com [`super::big_step`] e [`super::typing`] é de
//! propósito: lá, "nenhuma regra se aplica" significa que não há
//! derivação, e o resultado é um erro.

use std::fmt;

use crate::common::ToLatex;

use super::derivation::Reduces;
use super::Rule;

/// Limite de passos de [`run`]. Use [`run_with_fuel`] para outro valor.
///
/// É pequeno de propósito: a máquina clona a configuração a cada passo, e
/// um termo divergente (ω) deve falhar rápido num REPL ou numa página web.
pub const DEFAULT_FUEL: usize = 10_000;

// =============================================================================
// Transition
// =============================================================================

/// Um passo `from → to`, justificado por `rule`.
///
/// `rule` é a regra *mais externa*; as regras de congruência das
/// premissas (E-Bin1, E-If, ...) não ficam registradas aqui.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Transition<R, S> {
    pub rule: R,
    pub from: S,
    pub to: S,
}

impl<R, S> Transition<R, S> {
    pub fn new(rule: R, from: S, to: S) -> Self {
        Self { rule, from, to }
    }
}

impl<R, S: Clone> Transition<R, S> {
    /// O julgamento `from → to`, para uso em derivações.
    pub fn judgment(&self) -> Reduces<S> {
        Reduces { from: self.from.clone(), to: self.to.clone() }
    }
}

// =============================================================================
// Step
// =============================================================================

pub trait Step {
    type State: Clone;
    type Rule: Rule;

    /// O passo a partir de `state`, ou `None` se nenhuma regra se aplica.
    /// É uma função (não uma relação): a semântica é determinística.
    fn step(state: &Self::State) -> Option<Transition<Self::Rule, Self::State>>;

    /// Estado final: um valor (small-step) ou uma máquina parada.
    /// Estados finais não dão passos (ver `laws::final_states_do_not_step`).
    fn is_final(state: &Self::State) -> bool;
}

// =============================================================================
// Outcome e Trace
// =============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Outcome {
    Final,
    Stuck,
    OutOfFuel,
}

impl fmt::Display for Outcome {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Outcome::Final => "final",
            Outcome::Stuck => "stuck",
            Outcome::OutOfFuel => "out of fuel",
        })
    }
}

/// O histórico de uma execução.
///
/// Os impls de `Debug` e `Clone` são manuais: o `derive` exigiria que o
/// tipo marcador `S` (por exemplo `ArithSmallStep`) também os tivesse.
pub struct Trace<S: Step> {
    pub start: S::State,
    pub steps: Vec<Transition<S::Rule, S::State>>,
    pub final_state: S::State,
    pub outcome: Outcome,
}

impl<S: Step> Trace<S> {
    pub fn is_final(&self) -> bool {
        self.outcome == Outcome::Final
    }

    pub fn is_stuck(&self) -> bool {
        self.outcome == Outcome::Stuck
    }

    pub fn is_out_of_fuel(&self) -> bool {
        self.outcome == Outcome::OutOfFuel
    }

    /// Número de passos dados.
    pub fn len(&self) -> usize {
        self.steps.len()
    }

    pub fn is_empty(&self) -> bool {
        self.steps.is_empty()
    }

    /// As regras aplicadas, na ordem.
    pub fn rules(&self) -> Vec<S::Rule> {
        self.steps.iter().map(|t| t.rule).collect()
    }
}

impl<S: Step> Clone for Trace<S> {
    fn clone(&self) -> Self {
        Self {
            start: self.start.clone(),
            steps: self.steps.clone(),
            final_state: self.final_state.clone(),
            outcome: self.outcome,
        }
    }
}

impl<S: Step> fmt::Debug for Trace<S>
where
    S::State: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Trace")
            .field("start", &self.start)
            .field("steps", &self.steps)
            .field("final_state", &self.final_state)
            .field("outcome", &self.outcome)
            .finish()
    }
}

impl<S: Step> Trace<S>
where
    S::State: fmt::Display,
{
    /// Uma linha por passo, com a regra entre colchetes.
    pub fn to_text(&self) -> String {
        self.to_text_limited(usize::MAX)
    }

    /// Como [`Trace::to_text`], mas mostra no máximo `max_steps` passos
    /// (traces de termos divergentes têm milhares de linhas).
    pub fn to_text_limited(&self, max_steps: usize) -> String {
        let mut out = format!("{}\n", self.start);
        let shown = self.steps.len().min(max_steps);

        for t in &self.steps[..shown] {
            out.push_str(&format!("→ {}  [{}]\n", t.to, t.rule));
        }
        if self.steps.len() > shown {
            out.push_str(&format!(
                "… {} more steps omitted; last state:\n→ {}\n",
                self.steps.len() - shown,
                self.final_state
            ));
        }
        if self.outcome != Outcome::Final {
            out.push_str(&format!("({})\n", self.outcome));
        }
        out
    }
}

impl<S: Step> Trace<S>
where
    S::State: ToLatex,
{
    /// Uma cadeia `t0 →[r1] t1 →[r2] t2`, alinhada (requer `amsmath`).
    pub fn to_latex(&self) -> String {
        if self.steps.is_empty() {
            return self.start.to_latex();
        }

        let rows: Vec<String> = self
            .steps
            .iter()
            .enumerate()
            .map(|(i, t)| {
                let arrow = format!(
                    r"&\xrightarrow{{{}}} {}",
                    t.rule.to_latex(),
                    t.to.to_latex()
                );
                if i == 0 {
                    format!("{} {}", self.start.to_latex(), arrow)
                } else {
                    arrow
                }
            })
            .collect();

        format!("\\begin{{aligned}}{}\\end{{aligned}}", rows.join(" \\\\\n"))
    }
}

// =============================================================================
// Drivers
// =============================================================================

/// Itera `step` até um estado final, um estado travado, ou `fuel` passos.
///
/// A ordem das checagens importa: um estado final nunca é "sem fuel", e um
/// estado travado é reportado como travado mesmo se o fuel acabou.
pub fn run_with_fuel<S: Step>(start: S::State, fuel: usize) -> Trace<S> {
    let mut current = start.clone();
    let mut steps = Vec::new();

    let outcome = loop {
        if S::is_final(&current) {
            break Outcome::Final;
        }

        match S::step(&current) {
            None => break Outcome::Stuck,
            Some(_) if steps.len() >= fuel => break Outcome::OutOfFuel,
            Some(transition) => {
                current = transition.to.clone();
                steps.push(transition);
            }
        }
    };

    Trace { start, steps, final_state: current, outcome }
}

/// [`run_with_fuel`] com [`DEFAULT_FUEL`].
pub fn run<S: Step>(start: S::State) -> Trace<S> {
    run_with_fuel::<S>(start, DEFAULT_FUEL)
}

/// Os passos de forma preguiçosa (para um REPL "passo a passo").
/// Termina em um estado final ou travado; não há limite de fuel.
pub struct Steps<S: Step> {
    current: S::State,
}

pub fn steps<S: Step>(start: S::State) -> Steps<S> {
    Steps { current: start }
}

impl<S: Step> Iterator for Steps<S> {
    type Item = Transition<S::Rule, S::State>;

    fn next(&mut self) -> Option<Self::Item> {
        if S::is_final(&self.current) {
            return None;
        }
        let transition = S::step(&self.current)?;
        self.current = transition.to.clone();
        Some(transition)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::semantics::toy::*;

    crate::rules! {
        enum LoopRule { Tick => "E-Tick" }
    }

    /// Um sistema que nunca termina, para testar o fuel.
    struct Forever;

    impl Step for Forever {
        type State = u64;
        type Rule = LoopRule;

        fn step(n: &u64) -> Option<Transition<LoopRule, u64>> {
            Some(Transition::new(LoopRule::Tick, *n, n + 1))
        }

        fn is_final(_: &u64) -> bool {
            false
        }
    }

    #[test]
    fn single_step() {
        let t = add(num(1), num(2));
        let step = ToySmallStep::step(&t).unwrap();

        assert_eq!(step.rule, StepRule::AddCompute);
        assert_eq!(step.from, t);
        assert_eq!(step.to, num(3));
    }

    #[test]
    fn nested_addition_reduces_left_to_right() {
        // (1+2)+(3+4) → 3+(3+4) → 3+7 → 10
        let t = add(add(num(1), num(2)), add(num(3), num(4)));
        let trace = run::<ToySmallStep>(t);

        assert!(trace.is_final());
        assert_eq!(trace.final_state, num(10));
        assert_eq!(
            trace.rules(),
            vec![StepRule::AddLeft, StepRule::AddRight, StepRule::AddCompute]
        );
    }

    #[test]
    fn a_final_state_takes_no_steps() {
        let trace = run::<ToySmallStep>(num(5));
        assert!(trace.is_final() && trace.is_empty());
        assert_eq!(trace.final_state, num(5));
    }

    #[test]
    fn stuck_is_a_normal_outcome() {
        let t = add(boolean(true), num(1));
        let trace = run::<ToySmallStep>(t.clone());

        assert!(trace.is_stuck());
        assert!(trace.is_empty());
        assert_eq!(trace.final_state, t);
    }

    #[test]
    fn gets_stuck_after_making_progress() {
        // (1+2)+true → 3+true (travado)
        let trace = run::<ToySmallStep>(add(add(num(1), num(2)), boolean(true)));

        assert!(trace.is_stuck());
        assert_eq!(trace.len(), 1);
        assert_eq!(trace.final_state, add(num(3), boolean(true)));
    }

    #[test]
    fn fuel_stops_a_divergent_system() {
        let trace = run_with_fuel::<Forever>(0, 10);

        assert!(trace.is_out_of_fuel());
        assert_eq!(trace.len(), 10);
        assert_eq!(trace.final_state, 10);
    }

    #[test]
    fn exactly_enough_fuel_still_finishes() {
        let t = add(num(1), num(2)); // precisa de 1 passo
        assert!(run_with_fuel::<ToySmallStep>(t.clone(), 1).is_final());
        assert!(run_with_fuel::<ToySmallStep>(t, 0).is_out_of_fuel());
    }

    #[test]
    fn stuck_wins_over_out_of_fuel() {
        let t = add(boolean(true), num(1));
        assert!(run_with_fuel::<ToySmallStep>(t, 0).is_stuck());
    }

    #[test]
    fn the_lazy_iterator_matches_run() {
        let t = add(add(num(1), num(2)), add(num(3), num(4)));
        assert_eq!(steps::<ToySmallStep>(t).count(), 3);
    }

    #[test]
    fn text_rendering() {
        let trace = run::<ToySmallStep>(add(num(1), num(2)));
        assert_eq!(trace.to_text(), "(1 + 2)\n→ 3  [E-AddConst]\n");

        let stuck = run::<ToySmallStep>(add(boolean(true), num(1)));
        assert!(stuck.to_text().ends_with("(stuck)\n"));
    }

    #[test]
    fn latex_rendering() {
        let trace = run::<ToySmallStep>(add(num(1), num(2)));
        assert_eq!(
            trace.to_latex(),
            r"\begin{aligned}(1 + 2) &\xrightarrow{\textsc{E-AddConst}} 3\end{aligned}"
        );
    }

    #[test]
    fn long_traces_can_be_truncated() {
        let trace = run_with_fuel::<Forever>(0, 50);
        let text = trace.to_text_limited(3);

        assert!(text.starts_with("0\n→ 1  [E-Tick]\n→ 2  [E-Tick]\n→ 3  [E-Tick]\n"));
        assert!(text.contains("… 47 more steps omitted"));
        assert!(text.ends_with("→ 50\n(out of fuel)\n"));
    }

    #[test]
    fn a_transition_is_a_reduction_judgment() {
        let step = ToySmallStep::step(&add(num(1), num(2))).unwrap();
        assert_eq!(step.judgment().to_string(), "(1 + 2) → 3");
    }
}
