//! Geração de LaTeX.
//!
//! O trait [`ToLatex`] e funções auxiliares para regras de inferência.
//! As árvores de derivação usam o pacote `mathpartir`
//! (`\usepackage{mathpartir}`), cujo `\inferrule*` aninha naturalmente.

/// Tipos que sabem se escrever em LaTeX (modo matemático).
pub trait ToLatex {
    fn to_latex(&self) -> String;
}

impl<T: ToLatex + ?Sized> ToLatex for &T {
    fn to_latex(&self) -> String {
        (**self).to_latex()
    }
}

impl<T: ToLatex + ?Sized> ToLatex for Box<T> {
    fn to_latex(&self) -> String {
        (**self).to_latex()
    }
}

/// Escapa os caracteres especiais de LaTeX que podem aparecer em nomes.
pub fn escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        if matches!(c, '_' | '&' | '%' | '$' | '#' | '{' | '}') {
            out.push('\\');
        }
        out.push(c);
    }
    out
}

/// Um identificador em modo matemático: `x` fica `x`; `foo_1` fica
/// `\mathit{foo\_1}` (senão o LaTeX o leria como `f·o·o` em itálico).
pub fn ident(name: &str) -> String {
    if name.chars().count() == 1 {
        escape(name)
    } else {
        format!(r"\mathit{{{}}}", escape(name))
    }
}

/// Uma regra de inferência: `premissas / conclusão`, com o nome à direita.
/// As premissas são separadas por `\\` (convenção do `mathpartir`);
/// sem premissas, produz um axioma.
pub fn inference(premises: &[String], conclusion: &str, label: &str) -> String {
    format!(
        r"\inferrule*[right={}]{{{}}}{{{}}}",
        label,
        premises.join(r" \\ "),
        conclusion
    )
}

// -----------------------------------------------------------------------------
// Sabor "web" (KaTeX)
// -----------------------------------------------------------------------------
//
// O KaTeX não tem `mathpartir` (`\inferrule*`) nem `\textsc`. Para o navegador,
// uma regra é um `\dfrac` (as derivações aninham por recursão) e o nome da
// regra vai à direita da barra, em `\text`.

/// O nome de uma regra, à direita da barra de inferência.
pub fn web_label(name: &str) -> String {
    format!(r"\text{{\small {name}}}")
}

/// Uma regra de inferência para o KaTeX: `premissas / conclusão`, com o rótulo
/// à direita. As premissas ficam lado a lado; sem premissas é um axioma.
pub fn web_inference(premises: &[String], conclusion: &str, label: &str) -> String {
    let numerator = if premises.is_empty() {
        r"\vphantom{X}".to_string()
    } else {
        premises.join(r" \qquad ")
    };
    format!(r"\dfrac{{{numerator}}}{{{conclusion}}}\ {label}")
}

/// Documento mínimo compilável com `pdflatex`, útil para conferir a saída.
pub fn standalone_document(body: &str) -> String {
    format!(
        "\\documentclass[border=8pt]{{standalone}}\n\
         \\usepackage{{amsmath,amssymb,mathpartir}}\n\
         \\begin{{document}}\n$\\displaystyle\n{body}\n$\n\\end{{document}}\n"
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn escapes_special_characters() {
        assert_eq!(escape("a_b&c"), r"a\_b\&c");
    }

    #[test]
    fn single_letters_stay_plain() {
        assert_eq!(ident("x"), "x");
        assert_eq!(ident("foo_1"), r"\mathit{foo\_1}");
    }

    #[test]
    fn axiom_has_empty_premises() {
        assert_eq!(
            inference(&[], "a", r"\textsc{R}"),
            r"\inferrule*[right=\textsc{R}]{}{a}"
        );
    }

    #[test]
    fn premises_are_separated_by_double_backslash() {
        let p = vec!["p".to_string(), "q".to_string()];
        assert_eq!(inference(&p, "c", "L"), r"\inferrule*[right=L]{p \\ q}{c}");
    }

    #[test]
    fn standalone_wraps_the_body() {
        let doc = standalone_document("BODY");
        assert!(doc.contains("mathpartir") && doc.contains("BODY"));
    }
}
