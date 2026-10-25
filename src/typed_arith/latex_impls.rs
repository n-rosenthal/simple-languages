use crate::common::ToLatex;

use super::terms::Term;
use super::types::Type;

impl ToLatex for Type {
    fn to_latex(&self) -> String {
        match self {
            Type::Bool => r"\mathsf{Bool}".to_string(),
            Type::Nat => r"\mathsf{Nat}".to_string(),
        }
    }
}

fn operand(term: &Term) -> String {
    if matches!(term, Term::True | Term::False) || term.is_numeric_value() {
        term.to_latex()
    } else {
        format!("({})", term.to_latex())
    }
}

impl ToLatex for Term {
    fn to_latex(&self) -> String {
        if let Some(n) = self.numeral_value() {
            return n.to_string();
        }

        match self {
            Term::True => r"\mathsf{true}".to_string(),
            Term::False => r"\mathsf{false}".to_string(),
            Term::Zero => "0".to_string(),
            Term::If { condition, then_branch, else_branch } => format!(
                r"\mathsf{{if}}\ {}\ \mathsf{{then}}\ {}\ \mathsf{{else}}\ {}",
                condition.to_latex(),
                then_branch.to_latex(),
                else_branch.to_latex(),
            ),
            Term::Succ(t) => format!(r"\mathsf{{succ}}\ {}", operand(t)),
            Term::Pred(t) => format!(r"\mathsf{{pred}}\ {}", operand(t)),
            Term::IsZero(t) => format!(r"\mathsf{{iszero}}\ {}", operand(t)),
        }
    }
}
