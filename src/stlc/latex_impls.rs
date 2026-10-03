use crate::common::latex::ident;
use crate::common::ToLatex;

use super::terms::Term;
use super::types::Type;

impl ToLatex for Type {
    fn to_latex(&self) -> String {
        match self {
            Type::Bool => r"\mathsf{Bool}".to_string(),
            Type::Base(name) => format!(r"\mathsf{{{name}}}"),
            Type::Arrow(from, to) => match **from {
                Type::Arrow(..) => format!(r"({}) \to {}", from.to_latex(), to.to_latex()),
                _ => format!(r"{} \to {}", from.to_latex(), to.to_latex()),
            },
        }
    }
}

impl ToLatex for Term {
    fn to_latex(&self) -> String {
        match self {
            Term::Var(x) => ident(x),
            Term::True => r"\mathsf{true}".to_string(),
            Term::False => r"\mathsf{false}".to_string(),

            Term::If { condition, then_branch, else_branch } => format!(
                r"\mathsf{{if}}\ {}\ \mathsf{{then}}\ {}\ \mathsf{{else}}\ {}",
                condition.to_latex(),
                then_branch.to_latex(),
                else_branch.to_latex(),
            ),

            Term::Lambda { param, ty, body } => format!(
                r"\lambda {}{{:}}{}.\, {}",
                ident(param),
                ty.to_latex(),
                body.to_latex()
            ),

            Term::App { func, arg } => {
                let func_tex = match **func {
                    Term::Lambda { .. } | Term::If { .. } => format!("({})", func.to_latex()),
                    _ => func.to_latex(),
                };
                let arg_tex = match **arg {
                    Term::Var(_) | Term::True | Term::False => arg.to_latex(),
                    _ => format!("({})", arg.to_latex()),
                };
                format!(r"{func_tex}\ {arg_tex}")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::stlc::testing::parse;

    #[test]
    fn booleans_and_conditionals() {
        assert_eq!(parse("true").to_latex(), r"\mathsf{true}");
        assert_eq!(
            parse("if a then true else false").to_latex(),
            r"\mathsf{if}\ a\ \mathsf{then}\ \mathsf{true}\ \mathsf{else}\ \mathsf{false}"
        );
    }

    #[test]
    fn types() {
        assert_eq!(parse("λx:Bool->A. x").to_latex(), r"\lambda x{:}\mathsf{Bool} \to \mathsf{A}.\, x");
    }

    #[test]
    fn multi_letter_variables_are_italic_words() {
        assert_eq!(parse("foo_1").to_latex(), r"\mathit{foo\_1}");
    }
}
