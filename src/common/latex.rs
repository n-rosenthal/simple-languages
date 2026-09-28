// common/latex.rs
//! Trait para tipos que sabem se renderizar como LaTeX — usado tanto
//! pelos termos/valores de cada linguagem quanto pelo sistema de
//! derivação genérico (`derivation.rs`), que é agnóstico a qual
//! linguagem está sendo avaliada.

pub trait ToLatex {
    /// Renderiza `self` como uma expressão LaTeX (sem delimitadores
    /// de modo matemático — quem for embutir em HTML decide entre
    /// `\(...\)`, `\[...\]`, `$$...$$` etc., conforme o motor de
    /// renderização usado, ex.: MathJax ou KaTeX).
    fn to_latex(&self) -> String;

    /// Conveniência: `to_latex()` já envolto em `\[ ... \]`, pronto
    /// para embutir direto num bloco de display matemático.
    fn to_latex_display(&self) -> String {
        format!("\\[{}\\]", self.to_latex())
    }
}