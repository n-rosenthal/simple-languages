use crate::common::latex::ident;
use crate::common::ToLatex;

use super::terms::Term;
use super::types::Type;

impl ToLatex for Type {
    fn to_latex(&self) -> String {
        match self {
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

            Term::Lambda { param, ty, body } => format!(
                r"\lambda {}{{:}}{}.\, {}",
                ident(param),
                ty.to_latex(),
                body.to_latex()
            ),

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
