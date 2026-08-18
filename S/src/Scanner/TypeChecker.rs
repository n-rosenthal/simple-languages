//! S/src/Scanner/TypeChecker.rs
//!
//! Checagem estática de tipos sobre a AST de `S`.
//!
//! author:  n-rosenthal
//! date:    2026-08-17
//! version: 0.1

use crate::Scanner::Types::{BinaryOperator, Term, Type};

/// Erros possíveis durante a checagem de tipos.
#[derive(Debug, PartialEq)]
pub enum TypeError {
    /// Um operador foi aplicado a operando(s) de tipo incompatível.
    Mismatch {
        operator: BinaryOperator,
        expected: Type,
        found: Type,
        side: Side,
    },
    /// `Eq` recebeu operandos de tipos diferentes entre si.
    EqTypeMismatch { left: Type, right: Type },
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
                write!(
                    f,
                    "operador '{operator}': {side_name} deveria ser {expected}, mas é {found}"
                )
            }
            TypeError::EqTypeMismatch { left, right } => {
                write!(f, "operador '==': tipos incompatíveis {left} e {right}")
            }
        }
    }
}

impl std::error::Error for TypeError {}

/// Infere o `Type` de um `Term`, ou retorna o primeiro `TypeError`
/// encontrado na subárvore.
pub fn type_of(term: &Term) -> Result<Type, TypeError> {
    match term {
        Term::LiteralInteger(_) => Ok(Type::Integer),
        Term::LiteralBoolean(_) => Ok(Type::Boolean),

        Term::BinaryOperation { operator, left, right } => {
            let left_ty = type_of(left)?;
            let right_ty = type_of(right)?;

            match operator {
                BinaryOperator::Add | BinaryOperator::Sub => {
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
                    Ok(Type::Integer)
                }

                BinaryOperator::Eq => {
                    if left_ty != right_ty {
                        return Err(TypeError::EqTypeMismatch {
                            left: left_ty,
                            right: right_ty,
                        });
                    }
                    Ok(Type::Boolean)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn inteiro_literal_tem_tipo_integer() {
        assert_eq!(type_of(&Term::LiteralInteger(42)), Ok(Type::Integer));
    }

    #[test]
    fn booleano_literal_tem_tipo_boolean() {
        assert_eq!(type_of(&Term::LiteralBoolean(true)), Ok(Type::Boolean));
    }

    #[test]
    fn soma_de_inteiros_tem_tipo_integer() {
        let term = Term::BinaryOperation {
            operator: BinaryOperator::Add,
            left: Box::new(Term::LiteralInteger(1)),
            right: Box::new(Term::LiteralInteger(2)),
        };
        assert_eq!(type_of(&term), Ok(Type::Integer));
    }

    #[test]
    fn soma_com_booleano_e_erro() {
        let term = Term::BinaryOperation {
            operator: BinaryOperator::Add,
            left: Box::new(Term::LiteralInteger(1)),
            right: Box::new(Term::LiteralBoolean(true)),
        };
        assert_eq!(
            type_of(&term),
            Err(TypeError::Mismatch {
                operator: BinaryOperator::Add,
                expected: Type::Integer,
                found: Type::Boolean,
                side: Side::Right,
            })
        );
    }

    #[test]
    fn igualdade_entre_inteiros_tem_tipo_boolean() {
        let term = Term::BinaryOperation {
            operator: BinaryOperator::Eq,
            left: Box::new(Term::LiteralInteger(1)),
            right: Box::new(Term::LiteralInteger(1)),
        };
        assert_eq!(type_of(&term), Ok(Type::Boolean));
    }

    #[test]
    fn igualdade_entre_booleanos_tem_tipo_boolean() {
        let term = Term::BinaryOperation {
            operator: BinaryOperator::Eq,
            left: Box::new(Term::LiteralBoolean(true)),
            right: Box::new(Term::LiteralBoolean(false)),
        };
        assert_eq!(type_of(&term), Ok(Type::Boolean));
    }

    #[test]
    fn igualdade_entre_tipos_diferentes_e_erro() {
        let term = Term::BinaryOperation {
            operator: BinaryOperator::Eq,
            left: Box::new(Term::LiteralInteger(1)),
            right: Box::new(Term::LiteralBoolean(true)),
        };
        assert_eq!(
            type_of(&term),
            Err(TypeError::EqTypeMismatch { left: Type::Integer, right: Type::Boolean })
        );
    }

    #[test]
    fn erro_de_tipo_em_subarvore_propaga() {
        // (1 + true) - 2  =>  erro já no lado esquerdo da subtração
        let term = Term::BinaryOperation {
            operator: BinaryOperator::Sub,
            left: Box::new(Term::BinaryOperation {
                operator: BinaryOperator::Add,
                left: Box::new(Term::LiteralInteger(1)),
                right: Box::new(Term::LiteralBoolean(true)),
            }),
            right: Box::new(Term::LiteralInteger(2)),
        };
        assert!(type_of(&term).is_err());
    }
}
