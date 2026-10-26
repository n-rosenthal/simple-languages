//! Representação LaTeX de termos lambda sem nomes explícitos.

use crate::common::ToLatex;

use super::terms::Term;

impl ToLatex for Term {
    fn to_latex(&self) -> String {
        match self {
            Term::Var(index) => index.to_string(),

            Term::Abs(body) => {
                format!(r"\lambda.\, {}", body.to_latex())
            }

            Term::App(func, arg) => {
                let func_tex = match func.as_ref() {
                    Term::Abs(_) => {
                        format!("({})", func.to_latex())
                    }
                    _ => func.to_latex(),
                };

                let arg_tex = match arg.as_ref() {
                    Term::Var(_) => arg.to_latex(),
                    Term::Abs(_) | Term::App(_, _) => {
                        format!("({})", arg.to_latex())
                    }
                };

                format!(r"{func_tex}\, {arg_tex}")
            }
        }
    }
}