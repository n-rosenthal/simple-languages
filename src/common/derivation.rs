// common/derivation.rs
//! Estruturas genéricas para representar uma derivação big-step
//! (semântica natural): um julgamento relaciona um termo a um valor,
//! e uma derivação é a aplicação de uma regra a um julgamento,
//! justificada por zero ou mais sub-derivações (premissas).
//!
//! Deliberadamente parametrizado sobre `R` (rótulo de regra), `T`
//! (termo) e `V` (valor) — nenhum desses tipos é conhecido aqui,
//! porque este módulo deve servir qualquer linguagem futura que
//! implemente `Evaluator`, não só `arith`.

use super::latex::ToLatex;

/// Um julgamento big-step: `term ⇓ value` ("term avalia para value").
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Judgment<T, V> {
    pub term: T,
    pub value: V,
}

impl<T, V> Judgment<T, V> {
    pub fn new(term: T, value: V) -> Self {
        Self { term, value }
    }
}

/// 
impl<T: ToLatex, V: ToLatex> ToLatex for Judgment<T, V> {
    fn to_latex(&self) -> String {
        format!("{} \\Downarrow {}", self.term.to_latex(), self.value.to_latex())
    }
}

/// Um nó de uma árvore de derivação: a aplicação concreta de uma
/// regra (`rule`) para provar `conclusion`, com `premises` sendo as
/// sub-derivações (já provadas) exigidas por essa regra.
///
/// Isso é uma *árvore*, não uma lista — o que corresponde de fato à
/// estrutura de uma prova em semântica operacional big-step. Uma
/// lista achatada (como o `Vec<EvaluationRule>` anterior) perde a
/// informação de "quais premissas pertencem a qual regra".
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Derivation<R, T, V> {
    pub rule: R,
    pub conclusion: Judgment<T, V>,
    pub premises: Vec<Derivation<R, T, V>>,
}

impl<R, T, V> Derivation<R, T, V> {
    /// Uma derivação sem premissas (regra "de base", como `E-INT`).
    pub fn leaf(rule: R, conclusion: Judgment<T, V>) -> Self {
        Self { rule, conclusion, premises: Vec::new() }
    }

    /// Uma derivação com premissas (regra composta, como `E-ADD`).
    pub fn node(rule: R, conclusion: Judgment<T, V>, premises: Vec<Self>) -> Self {
        Self { rule, conclusion, premises }
    }

    /// Achata a árvore numa sequência de rótulos de regra, na ordem
    /// pós-ordem (premissas antes da própria regra)
    pub fn postorder_rules(&self) -> Vec<R>
    where
        R: Clone,
    {
        let mut acc = Vec::new();
        self.collect_postorder(&mut acc);
        acc
    }

    fn collect_postorder(&self, acc: &mut Vec<R>)
    where
        R: Clone,
    {
        for premise in &self.premises {
            premise.collect_postorder(acc);
        }
        acc.push(self.rule.clone());
    }
}

impl<R: ToLatex, T: ToLatex, V: ToLatex> Derivation<R, T, V> {
    /// Visão de *um único passo*: mostra as premissas como
    /// julgamentos já provados (não recursa nas sub-derivações
    /// delas) sobre a conclusão desta regra — exatamente "os
    /// pré-requisitos necessários, e o termo antes/depois da
    /// aplicação da regra", como uma única linha de inferência:
    ///
    /// ```text
    /// premissa_1  premissa_2
    /// ----------------------  [REGRA]
    ///      conclusão
    /// ```
    pub fn to_latex_step(&self) -> String {
        let premises = self
            .premises
            .iter()
            .map(|p| p.conclusion.to_latex())
            .collect::<Vec<_>>()
            .join(" \\qquad ");

        format!(
            "\\dfrac{{{}}}{{{}}}\\ [{}]",
            premises,
            self.conclusion.to_latex(),
            self.rule.to_latex()
        )
    }

    /// Visão da *árvore inteira*: recursa nas sub-derivações,
    /// produzindo a prova completa (útil para depuração ou para
    /// mostrar a derivação inteira de um programa pequeno, não só o
    /// último passo).
    pub fn to_latex_tree(&self) -> String {
        let premises = self
            .premises
            .iter()
            .map(|p| p.to_latex_tree())
            .collect::<Vec<_>>()
            .join(" \\qquad ");

        format!(
            "\\dfrac{{{}}}{{{}}}\\ [{}]",
            premises,
            self.conclusion.to_latex(),
            self.rule.to_latex()
        )
    }
}
