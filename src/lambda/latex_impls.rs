use crate::common::latex::ident;
use crate::common::ToLatex;

use super::terms::Term;

impl ToLatex for Term {
    fn to_latex(&self) -> String {
        match self {
            Term::Var(x) => ident(x),
            Term::Lambda { param, body } => {
                format!(r"\lambda {}.\, {}", ident(param), body.to_latex())
            }
            Term::App { func, arg } => {
                let func_tex = match **func {
                    Term::Lambda { .. } => format!("({})", func.to_latex()),
                    _ => func.to_latex(),
                };
                let arg_tex = match **arg {
                    Term::Var(_) => arg.to_latex(),
                    _ => format!("({})", arg.to_latex()),
                };
                format!(r"{func_tex}\ {arg_tex}")
            }
        }
    }
}
