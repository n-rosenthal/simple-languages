use crate::common::ast::{AstNode, ToAst};

use super::terms::Term;

impl ToAst for Term {
    fn to_ast(&self) -> AstNode {
        match self {
            Term::Zero => AstNode::labeled("Natural", "0"),

            Term::Succ(t) => AstNode::labeled_with_children(
                "Succ",
                "succ",
                [t.to_ast()],
            ),

            Term::Pred(t) => AstNode::labeled_with_children(
                "Pred",
                "pred",
                [t.to_ast()],
            ),

            Term::IsZero(t) => AstNode::labeled_with_children(
                "IsZero",
                "iszero",
                [t.to_ast()],
            ),

            Term::Integer(n) => {
                AstNode::labeled("Integer", n.to_string())
            }

            Term::Boolean(b) => {
                AstNode::labeled("Boolean", b.to_string())
            }

            Term::Binary { op, lhs, rhs } => {
                AstNode::labeled_with_children(
                    "Binary",
                    op.symbol(),
                    [
                        lhs.to_ast(),
                        rhs.to_ast(),
                    ],
                )
            }

            Term::If {
                condition,
                then_branch,
                else_branch,
            } => {
                AstNode::with_children(
                    "If",
                    [
                        condition.to_ast(),
                        then_branch.to_ast(),
                        else_branch.to_ast(),
                    ],
                )
            }
        }
    }
}