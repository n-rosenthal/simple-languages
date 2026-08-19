//! S/src/Scanner/TypeChecker.rs
//!
//! Checagem estática de tipos sobre a AST de `S`.
//!
//! author:  n-rosenthal
//! date:    2026-08-17
//! version: 0.6 (with env support for REPL)

use std::collections::HashMap;

use crate::Scanner::Types::{BinaryOperator, Span, Term, Type, UnaryOperator};

/// Ambiente de tipos: associa nomes de variáveis ligadas (via `let`)
/// ao seu `Type`.
pub type TypeEnv = HashMap<String, Type>;

/// Lado de um operador (para mensagens de erro).
#[derive(Debug, PartialEq)]
pub enum Side {
    Left,
    Right,
}

impl std::fmt::Display for Side {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Side::Left => write!(f, "esquerdo"),
            Side::Right => write!(f, "direito"),
        }
    }
}

/// Erros possíveis durante a checagem de tipos.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum TypeError {
    #[error("operador '{operator}': lado {side} deveria ser {expected}, mas é {found} (em {span})")]
    Mismatch {
        operator: BinaryOperator,
        expected: Type,
        found: Type,
        side: Side,
        span: Span,
    },
    #[error("operador '{operator}': tipos incompatíveis {left} e {right} (em {span})")]
    EqualityMismatch {
        operator: BinaryOperator,
        left: Type,
        right: Type,
        span: Span,
    },
    #[error("operador '{operator}': operando deveria ser {expected}, mas é {found} (em {span})")]
    UnaryMismatch {
        operator: UnaryOperator,
        expected: Type,
        found: Type,
        span: Span,
    },
    #[error("condição do 'if' deveria ser Boolean, mas é {found} (em {span})")]
    ConditionMismatch { found: Type, span: Span },
    #[error("ramos do 'if' têm tipos diferentes: then={then_type}, else={else_type} (em {span})")]
    BranchMismatch { then_type: Type, else_type: Type, span: Span },
    #[error("variável '{name}' não está ligada neste escopo (em {span})")]
    UnboundVariable { name: String, span: Span },
    #[error("'let {name}: {declared}' recebeu valor de tipo {found}, incompatível com a anotação (em {span})")]
    LetAnnotationMismatch {
        name: String,
        declared: Type,
        found: Type,
        span: Span,
    },
    #[error("operador '!': operando deveria ser uma referência (Ref T), mas é {found} (em {span})")]
    DerefMismatch { found: Type, span: Span },
    #[error("operador ':=': lado esquerdo tem tipo {target} (deveria ser Ref T compatível com {value}) (em {span})")]
    AssignMismatch { target: Type, value: Type, span: Span },
    #[error("operador ';': lado esquerdo deveria ser Unit, mas é {found} (em {span})")]
    SequenceMismatch { found: Type, span: Span },
}

/// Infere o `Type` de um `Term` no escopo vazio (para uso em arquivos).
pub fn type_of(term: &Term) -> Result<Type, TypeError> {
    type_of_with_env(term, &TypeEnv::new())
}

/// Infere o `Type` de um `Term` sob um ambiente fornecido (para REPL persistente).
pub fn type_of_with_env(term: &Term, env: &TypeEnv) -> Result<Type, TypeError> {
    type_of_in(term, env)
}

fn type_of_in(term: &Term, env: &TypeEnv) -> Result<Type, TypeError> {
    match term {
        Term::LiteralInteger { .. } => Ok(Type::Integer),
        Term::LiteralBoolean { .. } => Ok(Type::Boolean),
        Term::Unit { .. } => Ok(Type::Unit),

        Term::Variable { name, span } => env
            .get(name)
            .cloned()
            .ok_or_else(|| TypeError::UnboundVariable { name: name.clone(), span: *span }),

        Term::UnaryOperation { operator, operand, span } => {
            let operand_ty = type_of_in(operand, env)?;
            match operator {
                UnaryOperator::Not => {
                    if operand_ty != Type::Boolean {
                        return Err(TypeError::UnaryMismatch {
                            operator: *operator,
                            expected: Type::Boolean,
                            found: operand_ty,
                            span: *span,
                        });
                    }
                    Ok(Type::Boolean)
                }
            }
        }

        Term::Conditional { condition, then_branch, else_branch, span } => {
            let cond_ty = type_of_in(condition, env)?;
            if cond_ty != Type::Boolean {
                return Err(TypeError::ConditionMismatch {
                    found: cond_ty,
                    span: *span,
                });
            }

            let then_ty = type_of_in(then_branch, env)?;
            let else_ty = type_of_in(else_branch, env)?;
            if then_ty != else_ty {
                return Err(TypeError::BranchMismatch {
                    then_type: then_ty,
                    else_type: else_ty,
                    span: *span,
                });
            }

            Ok(then_ty)
        }

        Term::BinaryOperation { operator, left, right, span } => {
            let left_ty = type_of_in(left, env)?;
            let right_ty = type_of_in(right, env)?;

            match operator {
                BinaryOperator::Add
                | BinaryOperator::Sub
                | BinaryOperator::Lt
                | BinaryOperator::Leq
                | BinaryOperator::Gt
                | BinaryOperator::Geq => {
                    if left_ty != Type::Integer {
                        return Err(TypeError::Mismatch {
                            operator: *operator,
                            expected: Type::Integer,
                            found: left_ty,
                            side: Side::Left,
                            span: *span,
                        });
                    }
                    if right_ty != Type::Integer {
                        return Err(TypeError::Mismatch {
                            operator: *operator,
                            expected: Type::Integer,
                            found: right_ty,
                            side: Side::Right,
                            span: *span,
                        });
                    }

                    match operator {
                        BinaryOperator::Add | BinaryOperator::Sub => Ok(Type::Integer),
                        _ => Ok(Type::Boolean),
                    }
                }

                BinaryOperator::Eq | BinaryOperator::Neq => {
                    if left_ty != right_ty {
                        return Err(TypeError::EqualityMismatch {
                            operator: *operator,
                            left: left_ty,
                            right: right_ty,
                            span: *span,
                        });
                    }
                    Ok(Type::Boolean)
                }
            }
        }

        Term::Let { name, declared_type, value, body, span } => {
            let value_ty = type_of_in(value, env)?;
            if value_ty != *declared_type {
                return Err(TypeError::LetAnnotationMismatch {
                    name: name.clone(),
                    declared: declared_type.clone(),
                    found: value_ty,
                    span: *span,
                });
            }

            let mut inner_env = env.clone();
            inner_env.insert(name.clone(), declared_type.clone());

            // O corpo pode ser Unit (se não houver 'in'), então seu tipo é Unit.
            // Avaliamos o corpo para propagar o ambiente, mas o tipo do let é o tipo do corpo.
            let body_ty = type_of_in(body, &inner_env)?;
            Ok(body_ty)
        }

        Term::Ref { inner, span: _span } => {
            let inner_ty = type_of_in(inner, env)?;
            Ok(Type::Reference(Box::new(inner_ty)))
        }

        Term::Deref { inner, span } => {
            let inner_ty = type_of_in(inner, env)?;
            match inner_ty {
                Type::Reference(referenced) => Ok(*referenced),
                other => Err(TypeError::DerefMismatch {
                    found: other,
                    span: *span,
                }),
            }
        }

        Term::Assign { target, value, span } => {
            let target_ty = type_of_in(target, env)?;
            let value_ty = type_of_in(value, env)?;

            match target_ty {
                Type::Reference(referenced) => {
                    if *referenced == value_ty {
                        Ok(Type::Unit)
                    } else {
                        Err(TypeError::AssignMismatch {
                            target: Type::Reference(referenced),
                            value: value_ty,
                            span: *span,
                        })
                    }
                }
                other => Err(TypeError::AssignMismatch {
                    target: other,
                    value: value_ty,
                    span: *span,
                }),
            }
        }

        Term::Sequence { left, right, span } => {
            let left_ty = type_of_in(left, env)?;
            if left_ty != Type::Unit {
                return Err(TypeError::SequenceMismatch {
                    found: left_ty,
                    span: *span,
                });
            }
            type_of_in(right, env)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Scanner::Types::Span;

    fn dummy_span() -> Span {
        Span::new(1, 1)
    }

    #[test]
    fn soma_com_booleano_e_erro() {
        let term = Term::BinaryOperation {
            operator: BinaryOperator::Add,
            left: Box::new(Term::LiteralInteger { value: 1, span: dummy_span() }),
            right: Box::new(Term::LiteralBoolean { value: true, span: dummy_span() }),
            span: dummy_span(),
        };
        assert!(matches!(type_of(&term), Err(TypeError::Mismatch { .. })));
    }

    #[test]
    fn let_simples_tem_tipo_do_corpo() {
        let term = Term::Let {
            name: "x".to_string(),
            declared_type: Type::Integer,
            value: Box::new(Term::LiteralInteger { value: 1, span: dummy_span() }),
            body: Box::new(Term::BinaryOperation {
                operator: BinaryOperator::Add,
                left: Box::new(Term::Variable { name: "x".to_string(), span: dummy_span() }),
                right: Box::new(Term::LiteralInteger { value: 1, span: dummy_span() }),
                span: dummy_span(),
            }),
            span: dummy_span(),
        };
        assert_eq!(type_of(&term), Ok(Type::Integer));
    }

    #[test]
    fn let_sem_in_tem_tipo_unit() {
        let term = Term::Let {
            name: "x".to_string(),
            declared_type: Type::Integer,
            value: Box::new(Term::LiteralInteger { value: 10, span: dummy_span() }),
            body: Box::new(Term::Unit { span: dummy_span() }),
            span: dummy_span(),
        };
        assert_eq!(type_of(&term), Ok(Type::Unit));
    }

    #[test]
    fn sequence_ok() {
        let term = Term::Sequence {
            left: Box::new(Term::Unit { span: dummy_span() }),
            right: Box::new(Term::LiteralInteger { value: 42, span: dummy_span() }),
            span: dummy_span(),
        };
        assert_eq!(type_of(&term), Ok(Type::Integer));
    }

    #[test]
    fn sequence_erro_se_left_nao_unit() {
        let term = Term::Sequence {
            left: Box::new(Term::LiteralInteger { value: 1, span: dummy_span() }),
            right: Box::new(Term::LiteralInteger { value: 2, span: dummy_span() }),
            span: dummy_span(),
        };
        assert!(matches!(type_of(&term), Err(TypeError::SequenceMismatch { .. })));
    }
}
