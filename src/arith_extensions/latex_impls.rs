///! Implementações de `ToLatex` para `arith-extensions`.

use crate::common::ToLatex;

use super::terms::{BinaryOp, Term};
use super::types::Type;
use super::values::Value;

impl ToLatex for Type {
    fn to_latex(&self) -> String {
        format!(r"\mathsf{{{self}}}")
    }
}

impl ToLatex for Value {
    fn to_latex(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => r"\text{true}".to_string(),
            Value::Boolean(false) => r"\text{false}".to_string(),
            Value::Natural(n) => format!("({} : \\mathsf{{Natural}})", n.to_string()),
            Value::Closure { .. } => "<closure>".to_string(),
        }
    }
}

impl ToLatex for BinaryOp {
    fn to_latex(&self) -> String {
        match self {
            BinaryOp::Add => "+".to_string(),
            BinaryOp::Sub => "-".to_string(),
            BinaryOp::Mul => r"\times".to_string(),
            BinaryOp::LessThan => "<".to_string(),
            BinaryOp::Equal => "=".to_string(),
            BinaryOp::And => r"\wedge".to_string(),
            BinaryOp::Or => r"\vee".to_string(),
            BinaryOp::Div => "/".to_string(),
            BinaryOp::Mod => r"\bmod".to_string(),
        }
    }
}

impl ToLatex for Term {
    fn to_latex(&self) -> String {
        match self {
            //  valores
            Term::Integer(n) => n.to_string(),
            Term::Boolean(true) => r"\text{true}".to_string(),
            Term::Boolean(false) => r"\text{false}".to_string(),
            Term::Zero => "0".to_string(),
            Term::Succ(t) => format!("\\text{{succ }}({})", t.to_latex()),
            Term::Pred(t) => format!("\\text{{pred }}({})", t.to_latex()),
            Term::IsZero(t) => format!("\\text{{iszero }}({})", t.to_latex()),

            // expressões
            Term::Binary { op, lhs, rhs } => format!(
                "({} \\mathbin{{{}}} {})",
                lhs.to_latex(),
                op.to_latex(),
                rhs.to_latex()
            ),

            //  condicional
            Term::If { condition, then_branch, else_branch } => format!(
                r"\text{{if }}{}\text{{ then }}{}\text{{ else }}{}",
                condition.to_latex(),
                then_branch.to_latex(),
                else_branch.to_latex(),
            ),
        }
    }
}
