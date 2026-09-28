use crate::common::ToLatex;

use super::terms::{BinaryOp, Term};
use super::values::Value;
use super::evaluator::EvaluationRule;

impl ToLatex for Value {
    fn to_latex(&self) -> String {
        match self {
            Value::Integer(n) => n.to_string(),
            Value::Boolean(true) => r"\text{true}".to_string(),
            Value::Boolean(false) => r"\text{false}".to_string(),
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
        }
    }
}

impl ToLatex for Term {
    fn to_latex(&self) -> String {
        match self {
            Term::Integer(n) => n.to_string(),
            Term::Boolean(true) => r"\text{true}".to_string(),
            Term::Boolean(false) => r"\text{false}".to_string(),
            Term::Binary { op, lhs, rhs } => {
                format!("({} \\mathbin{{{}}} {})", lhs.to_latex(), op.to_latex(), rhs.to_latex())
            }
            Term::If { condition, then_branch, else_branch } => format!(
                r"\text{{if }}{}\text{{ then }}{}\text{{ else }}{}",
                condition.to_latex(),
                then_branch.to_latex(),
                else_branch.to_latex(),
            ),
        }
    }
}

impl ToLatex for EvaluationRule {
    fn to_latex(&self) -> String {
        let name = match self {
            Self::Int => "E-Int",
            Self::Bool => "E-Bool",
            Self::Add => "E-Add",
            Self::Sub => "E-Sub",
            Self::Mul => "E-Mul",
            Self::LessThan => "E-Lt",
            Self::Equal => "E-Eq",
            Self::And => "E-And",
            Self::Or => "E-Or",
            Self::IfTrue => "E-IfTrue",
            Self::IfFalse => "E-IfFalse",
        };
        format!(r"\textsc{{{}}}", name)
    }
}

// small step
use super::small_step::SmallStepRule;

impl ToLatex for SmallStepRule {
    fn to_latex(&self) -> String {
        let name = match self {
            Self::BinaryLeft => "E-Bin1",
            Self::BinaryRight => "E-Bin2",
            Self::BinaryCompute => "E-BinConst",
            Self::IfCongruence => "E-If",
            Self::IfTrue => "E-IfTrue",
            Self::IfFalse => "E-IfFalse",
        };
        format!(r"\textsc{{{}}}", name)
    }
}