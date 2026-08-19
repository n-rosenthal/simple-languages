//! S/src/Scanner/TypeChecker.rs
//!
//! Checagem estática de tipos sobre a AST de `S`.
//!
//! author:  n-rosenthal
//! date:    2026-08-17
//! version: 0.4

use std::collections::HashMap;

use crate::Scanner::Types::{BinaryOperator, Term, Type, UnaryOperator};

/// Ambiente de tipos: associa nomes de variáveis ligadas (via `let`)
/// ao seu `Type`.
pub type TypeEnv = HashMap<String, Type>;

/// Erros possíveis durante a checagem de tipos.
#[derive(Debug, PartialEq)]
pub enum TypeError {
    Mismatch {
        operator: BinaryOperator,
        expected: Type,
        found: Type,
        side: Side,
    },
    EqualityMismatch {
        operator: BinaryOperator,
        left: Type,
        right: Type,
    },
    UnaryMismatch {
        operator: UnaryOperator,
        expected: Type,
        found: Type,
    },
    ConditionMismatch {
        found: Type,
    },
    BranchMismatch {
        then_type: Type,
        else_type: Type,
    },
    /// Uso de uma variável não ligada por nenhum `let` no escopo.
    UnboundVariable {
        name: String,
    },
    /// O tipo anotado em `let x: T = ...` não bate com o tipo real de `t1`.
    LetAnnotationMismatch {
        name: String,
        declared: Type,
        found: Type,
    },
    /// `!t`: `t` não tem tipo `Ref(_)`.
    DerefMismatch {
        found: Type,
    },
    /// `t1 := t2`: `t1` não é `Ref(T)`, ou `t2` não é do tipo `T`.
    AssignMismatch {
        target: Type,
        value: Type,
    },
}

#[derive(Debug, PartialEq)]
pub enum Side {
    Left,
    Right,
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::Mismatch { operator, expected, found, side } => {
                let side_name = match side {
                    Side::Left => "operando esquerdo",
                    Side::Right => "operando direito",
                };
                write!(f, "operador '{operator}': {side_name} deveria ser {expected}, mas é {found}")
            }
            TypeError::EqualityMismatch { operator, left, right } => {
                write!(f, "operador '{operator}': tipos incompatíveis {left} e {right}")
            }
            TypeError::UnaryMismatch { operator, expected, found } => {
                write!(f, "operador '{operator}': operando deveria ser {expected}, mas é {found}")
            }
            TypeError::ConditionMismatch { found } => {
                write!(f, "condição do 'if' deveria ser Boolean, mas é {found}")
            }
            TypeError::BranchMismatch { then_type, else_type } => {
                write!(f, "ramos do 'if' têm tipos diferentes: then={then_type}, else={else_type}")
            }
            TypeError::UnboundVariable { name } => {
                write!(f, "variável '{name}' não está ligada neste escopo")
            }
            TypeError::LetAnnotationMismatch { name, declared, found } => {
                write!(
                    f,
                    "'let {name}: {declared}' recebeu valor de tipo {found}, incompatível com a anotação"
                )
            }
            TypeError::DerefMismatch { found } => {
                write!(f, "operador '!': operando deveria ser uma referência (Ref T), mas é {found}")
            }
            TypeError::AssignMismatch { target, value } => {
                write!(
                    f,
                    "operador ':=': lado esquerdo tem tipo {target} (deveria ser Ref T compatível com {value})"
                )
            }
        }
    }
}

impl std::error::Error for TypeError {}

/// Infere o `Type` de um `Term` no escopo vazio (sem variáveis ligadas).
pub fn type_of(term: &Term) -> Result<Type, TypeError> {
    type_of_in(term, &TypeEnv::new())
}

/// Infere o `Type` de um `Term` sob o ambiente `env`.
fn type_of_in(term: &Term, env: &TypeEnv) -> Result<Type, TypeError> {
    match term {
        Term::LiteralInteger(_) => Ok(Type::Integer),
        Term::LiteralBoolean(_) => Ok(Type::Boolean),
        Term::Unit => Ok(Type::Unit),

        Term::Variable(name) => env
            .get(name)
            .cloned()
            .ok_or_else(|| TypeError::UnboundVariable { name: name.clone() }),

        Term::UnaryOperation { operator, operand } => {
            let operand_ty = type_of_in(operand, env)?;
            match operator {
                UnaryOperator::Not => {
                    if operand_ty != Type::Boolean {
                        return Err(TypeError::UnaryMismatch {
                            operator: *operator,
                            expected: Type::Boolean,
                            found: operand_ty,
                        });
                    }
                    Ok(Type::Boolean)
                }
            }
        }

        Term::Conditional { condition, then_branch, else_branch } => {
            let cond_ty = type_of_in(condition, env)?;
            if cond_ty != Type::Boolean {
                return Err(TypeError::ConditionMismatch { found: cond_ty });
            }

            let then_ty = type_of_in(then_branch, env)?;
            let else_ty = type_of_in(else_branch, env)?;
            if then_ty != else_ty {
                return Err(TypeError::BranchMismatch { then_type: then_ty, else_type: else_ty });
            }

            Ok(then_ty)
        }

        Term::BinaryOperation { operator, left, right } => {
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
                        });
                    }
                    if right_ty != Type::Integer {
                        return Err(TypeError::Mismatch {
                            operator: *operator,
                            expected: Type::Integer,
                            found: right_ty,
                            side: Side::Right,
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
                        });
                    }
                    Ok(Type::Boolean)
                }
            }
        }

        Term::Let { name, declared_type, value, body } => {
            let value_ty = type_of_in(value, env)?;

            if value_ty != *declared_type {
                return Err(TypeError::LetAnnotationMismatch {
                    name: name.clone(),
                    declared: declared_type.clone(),
                    found: value_ty,
                });
            }

            let mut inner_env = env.clone();
            inner_env.insert(name.clone(), declared_type.clone());

            type_of_in(body, &inner_env)
        }

        Term::Ref(inner) => {
            let inner_ty = type_of_in(inner, env)?;
            Ok(Type::Reference(Box::new(inner_ty)))
        }

        Term::Deref(inner) => {
            let inner_ty = type_of_in(inner, env)?;
            match inner_ty {
                Type::Reference(referenced) => Ok(*referenced),
                other => Err(TypeError::DerefMismatch { found: other }),
            }
        }

        Term::Assign { target, value } => {
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
                        })
                    }
                }
                other => Err(TypeError::AssignMismatch { target: other, value: value_ty }),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn soma_com_booleano_e_erro() {
        let term = Term::BinaryOperation {
            operator: BinaryOperator::Add,
            left: Box::new(Term::LiteralInteger(1)),
            right: Box::new(Term::LiteralBoolean(true)),
        };
        assert!(matches!(type_of(&term), Err(TypeError::Mismatch { .. })));
    }

    #[test]
    fn variavel_nao_ligada_e_erro() {
        let term = Term::Variable("x".to_string());
        assert_eq!(type_of(&term), Err(TypeError::UnboundVariable { name: "x".to_string() }));
    }

    #[test]
    fn let_simples_tem_tipo_do_corpo() {
        let term = Term::Let {
            name: "x".to_string(),
            declared_type: Type::Integer,
            value: Box::new(Term::LiteralInteger(1)),
            body: Box::new(Term::BinaryOperation {
                operator: BinaryOperator::Add,
                left: Box::new(Term::Variable("x".to_string())),
                right: Box::new(Term::LiteralInteger(1)),
            }),
        };
        assert_eq!(type_of(&term), Ok(Type::Integer));
    }

    #[test]
    fn let_com_anotacao_incompativel_e_erro() {
        let term = Term::Let {
            name: "x".to_string(),
            declared_type: Type::Boolean,
            value: Box::new(Term::LiteralInteger(1)),
            body: Box::new(Term::Variable("x".to_string())),
        };
        assert!(matches!(type_of(&term), Err(TypeError::LetAnnotationMismatch { .. })));
    }

    #[test]
    fn unit_tem_tipo_unit() {
        assert_eq!(type_of(&Term::Unit), Ok(Type::Unit));
    }

    #[test]
    fn ref_de_inteiro_tem_tipo_ref_integer() {
        let term = Term::Ref(Box::new(Term::LiteralInteger(1)));
        assert_eq!(type_of(&term), Ok(Type::Reference(Box::new(Type::Integer))));
    }

    #[test]
    fn deref_de_ref_integer_tem_tipo_integer() {
        let term = Term::Deref(Box::new(Term::Ref(Box::new(Term::LiteralInteger(1)))));
        assert_eq!(type_of(&term), Ok(Type::Integer));
    }

    #[test]
    fn deref_de_nao_referencia_e_erro() {
        let term = Term::Deref(Box::new(Term::LiteralInteger(1)));
        assert!(matches!(type_of(&term), Err(TypeError::DerefMismatch { .. })));
    }

    #[test]
    fn assign_bem_tipado_produz_unit() {
        // let r: Ref Integer = ref 1 in r := 2
        let term = Term::Let {
            name: "r".to_string(),
            declared_type: Type::Reference(Box::new(Type::Integer)),
            value: Box::new(Term::Ref(Box::new(Term::LiteralInteger(1)))),
            body: Box::new(Term::Assign {
                target: Box::new(Term::Variable("r".to_string())),
                value: Box::new(Term::LiteralInteger(2)),
            }),
        };
        assert_eq!(type_of(&term), Ok(Type::Unit));
    }

    #[test]
    fn assign_com_tipo_incompativel_e_erro() {
        // let r: Ref Integer = ref 1 in r := true
        let term = Term::Let {
            name: "r".to_string(),
            declared_type: Type::Reference(Box::new(Type::Integer)),
            value: Box::new(Term::Ref(Box::new(Term::LiteralInteger(1)))),
            body: Box::new(Term::Assign {
                target: Box::new(Term::Variable("r".to_string())),
                value: Box::new(Term::LiteralBoolean(true)),
            }),
        };
        assert!(matches!(type_of(&term), Err(TypeError::AssignMismatch { .. })));
    }

    #[test]
    fn assign_sobre_nao_referencia_e_erro() {
        // 1 := 2  =>  1 não é uma referência
        let term = Term::Assign {
            target: Box::new(Term::LiteralInteger(1)),
            value: Box::new(Term::LiteralInteger(2)),
        };
        assert!(matches!(type_of(&term), Err(TypeError::AssignMismatch { .. })));
    }
}
