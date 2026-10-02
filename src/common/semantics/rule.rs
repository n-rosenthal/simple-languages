//! Regras de inferência.
//!
//! Uma regra é um /nome/ (`E-IfTrue`, `T-Abs`, ...) que aparece em
//! derivações, em mensagens e em LaTeX. O conjunto de regras de cada
//! julgamento (avaliação, tipagem, ...) é uma enum por linguagem.
//!
//! Este módulo define o trait [`Rule`] e a macro [`rules!`], que gera a
//! enum, `Display` e `ToLatex` a partir de uma única tabela, de modo que
//! cada nome de regra seja escrito uma só vez.

use std::fmt;

use crate::common::ToLatex;

/// Uma regra de inferência identificada por nome.
///
/// `Copy` porque, em TAPL, regras são apenas nomes; se alguma linguagem
/// precisar de regras com dados associados, relaxe para `Clone`.
pub trait Rule: Copy + Eq + fmt::Debug + fmt::Display + ToLatex {
    /// Nome da regra na notação do livro, por exemplo `"E-IfTrue"`.
    fn name(&self) -> &'static str;
}

/// Gera uma enum de regras com `Rule`, `Display` e `ToLatex`.
///
/// ```ignore
/// rules! {
///     /// Regras de tipagem de `lambda`.
///     pub enum TypingRule {
///         /// x : T ∈ Γ
///         Var => "T-Var",
///         Abs => "T-Abs",
///         App => "T-App",
///     }
/// }
/// ```
///
/// A enum derivada é `Debug, Clone, Copy, PartialEq, Eq, Hash`.
/// `Display` imprime o nome; `ToLatex` produz `\textsc{nome}`.
#[macro_export]
macro_rules! rules {
    (
        $(#[$meta:meta])*
        $vis:vis enum $name:ident {
            $(
                $(#[$vmeta:meta])*
                $variant:ident => $label:literal
            ),+ $(,)?
        }
    ) => {
        $(#[$meta])*
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        $vis enum $name {
            $(
                $(#[$vmeta])*
                $variant,
            )+
        }

        impl $crate::common::semantics::Rule for $name {
            fn name(&self) -> &'static str {
                match self {
                    $( Self::$variant => $label, )+
                }
            }
        }

        impl ::std::fmt::Display for $name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                f.write_str($crate::common::semantics::Rule::name(self))
            }
        }

        impl $crate::common::ToLatex for $name {
            fn to_latex(&self) -> ::std::string::String {
                format!(r"\textsc{{{}}}", $crate::common::semantics::Rule::name(self))
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    crate::rules! {
        /// Conjunto de regras de brinquedo para testar a macro.
        enum DemoRule {
            /// primeira regra
            IfTrue => "E-IfTrue",
            Abs => "T-Abs",
        }
    }

    #[test]
    fn name_is_the_label() {
        assert_eq!(DemoRule::IfTrue.name(), "E-IfTrue");
        assert_eq!(DemoRule::Abs.name(), "T-Abs");
    }

    #[test]
    fn display_prints_the_name() {
        assert_eq!(DemoRule::IfTrue.to_string(), "E-IfTrue");
    }

    #[test]
    fn latex_uses_small_caps() {
        assert_eq!(DemoRule::Abs.to_latex(), r"\textsc{T-Abs}");
    }

    #[test]
    fn rules_are_copy_and_comparable() {
        let a = DemoRule::IfTrue;
        let b = a; // Copy
        assert_eq!(a, b);
        assert_ne!(DemoRule::IfTrue, DemoRule::Abs);
    }

    #[test]
    fn works_as_a_generic_rule() {
        fn label<R: Rule>(r: R) -> String {
            format!("[{}]", r)
        }
        assert_eq!(label(DemoRule::Abs), "[T-Abs]");
    }
}
