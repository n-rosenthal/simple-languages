// common/small_step.rs
//! Infraestrutura genérica para semântica operacional small-step
//! (passo a passo): uma relação de redução `term → term'`, aplicada
//! repetidamente até atingir um termo em forma normal (ou travar).
//!
//! Paralelo direto de `derivation.rs` (big-step), mas onde big-step
//! produz uma árvore de premissas, small-step produz uma sequência —
//! o traço de reescrita.

use super::latex::ToLatex;

/// Um único passo de redução: `term → next`, justificado pela regra
/// `rule`. Diferente de `Judgment` (big-step, que relaciona termo a
/// *valor*), aqui os dois lados são *termos* — o resultado de um
/// passo pode não ser um valor final, só um termo "um pouco mais
/// reduzido".
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Step<R, T> {
    pub rule: R,
    pub from: T,
    pub to: T,
}

impl<R, T> Step<R, T> {
    pub fn new(rule: R, from: T, to: T) -> Self {
        Self { rule, from, to }
    }
}

impl<R: ToLatex, T: ToLatex> ToLatex for Step<R, T> {
    fn to_latex(&self) -> String {
        format!(
            "{} \\longrightarrow {}\\quad[{}]",
            self.from.to_latex(),
            self.to.to_latex(),
            self.rule.to_latex()
        )
    }
}

/// Um traço de redução completo: a sequência de passos até a forma
/// normal (ou até travar/errar). `final_term` é o último termo do
/// traço — se `is_stuck` for falso, é uma forma normal "de verdade"
/// (um valor); se verdadeiro, a redução parou porque nenhuma regra
/// se aplicava a um termo que não é um valor (termo mal formado
/// semanticamente, ex.: `true + 1` em `arith`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Trace<R, T> {
    pub steps: Vec<Step<R, T>>,
    pub final_term: T,
    pub is_stuck: bool,
}

impl<R: ToLatex, T: ToLatex> Trace<R, T> {
    /// Renderiza o traço inteiro como uma sequência de linhas
    /// `\longrightarrow`, uma por passo — útil para mostrar a
    /// redução completa de um programa pequeno em HTML.
    pub fn to_latex_sequence(&self) -> String {
        self.steps
            .iter()
            .map(|s| s.to_latex())
            .collect::<Vec<_>>()
            .join(" \\\\\n")
    }
}

/// Avaliador small-step: dado um termo, ou produz o próximo passo
/// (regra aplicada + termo resultante), ou reporta que o termo já é
/// um valor final (`step` retorna `None`).
///
/// Diferente do `Evaluator` (big-step), aqui a peça central é
/// `step` — um único passo — não `evaluate` inteiro. `evaluate_trace`
/// vem de graça (método default) a partir de `step` + `is_value`,
/// repetindo até não haver mais o que reduzir.
pub trait SmallStepEvaluator {
    type Term: Clone + PartialEq;
    type Rule;
    type Error;

    /// Verdadeiro se `term` já é uma forma normal (valor) — não há
    /// mais nenhum passo a dar. Precisa ser definido separadamente de
    /// `step`, porque um termo pode não ser valor E não ter passo
    /// aplicável (termo travado/stuck) — os dois casos são
    /// distinguíveis só comparando os dois.
    fn is_value(term: &Self::Term) -> bool;

    /// Tenta dar um passo a partir de `term`. Retorna `Ok(None)` se
    /// `term` já é um valor (nada a fazer). Retorna `Err` só para
    /// erros de fato (não para "termo travado" — isso é detectado por
    /// `evaluate_trace` comparando `is_value` contra a ausência de
    /// passo aplicável).
    fn step(term: &Self::Term) -> Result<Option<Step<Self::Rule, Self::Term>>, Self::Error>;

    /// Aplica `step` repetidamente até atingir um valor ou travar.
    /// Método default: qualquer linguagem que implemente `step` e
    /// `is_value` ganha isso sem escrever nada a mais.
    fn evaluate_trace(term: &Self::Term) -> Result<Trace<Self::Rule, Self::Term>, Self::Error>
    where
        Self::Term: std::fmt::Debug,
    {
        let mut steps = Vec::new();
        let mut current = term.clone();

        loop {
            match Self::step(&current)? {
                Some(step) => {
                    current = step.to.clone();
                    steps.push(step);
                }
                None => {
                    let is_stuck = !Self::is_value(&current);
                    return Ok(Trace { steps, final_term: current, is_stuck });
                }
            }
        }
    }
}