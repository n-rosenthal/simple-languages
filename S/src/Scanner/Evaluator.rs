//! S/src/Scanner/Evaluator.rs
//!
//! Avaliação de termos de `S` já validados pelo type-checker.
//!
//! author:  n-rosenthal
//! date:    2026-08-17
//! version: 0.1

use crate::Scanner::Types::{BinaryOperator, Term};

/// Resultado da avaliação de um `Term`.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(b) => write!(f, "{b}"),
        }
    }
}

/// Avalia um `Term` já validado pelo type-checker (`type_of`) e
/// produz seu `Value`.
///
/// # Contrato
/// Chame `type_of(term)` com sucesso antes de `eval(term)`. Termos
/// mal-tipados (ex.: `1 + true`) causam pânico, pois representam
/// violação do contrato do pipeline (`parse -> type_of -> eval`),
/// não um erro de execução esperado.
pub fn eval(term: &Term) -> Value {
    match term {
        Term::LiteralInteger(n) => Value::Integer(*n),
        Term::LiteralBoolean(b) => Value::Boolean(*b),

        Term::BinaryOperation { operator, left, right } => {
            let left_val = eval(left);
            let right_val = eval(right);

            match operator {
                BinaryOperator::Add => match (left_val, right_val) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Integer(a + b),
                    _ => unreachable!("type-checker deveria ter rejeitado Add mal-tipado"),
                },
                BinaryOperator::Sub => match (left_val, right_val) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Integer(a - b),
                    _ => unreachable!("type-checker deveria ter rejeitado Sub mal-tipado"),
                },
                BinaryOperator::Eq => Value::Boolean(left_val == right_val),
            }
        }
    }
}

/// Conveniência: checa o tipo e avalia em sequência, retornando erro
/// de tipo se o termo for mal-tipado (em vez de arriscar o `eval`
/// direto sobre um termo não validado).
pub fn type_check_and_eval(
    term: &Term,
) -> Result<Value, crate::Scanner::TypeChecker::TypeError> {
    crate::Scanner::TypeChecker::type_of(term)?;
    Ok(eval(term))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn avalia_soma() {
        let term = Term::BinaryOperation {
            operator: BinaryOperator::Add,
            left: Box::new(Term::LiteralInteger(1)),
            right: Box::new(Term::LiteralInteger(2)),
        };
        assert_eq!(eval(&term), Value::Integer(3));
    }

    #[test]
    fn avalia_subtracao() {
        let term = Term::BinaryOperation {
            operator: BinaryOperator::Sub,
            left: Box::new(Term::LiteralInteger(5)),
            right: Box::new(Term::LiteralInteger(3)),
        };
        assert_eq!(eval(&term), Value::Integer(2));
    }

    #[test]
    fn avalia_igualdade_entre_inteiros() {
        let term = Term::BinaryOperation {
            operator: BinaryOperator::Eq,
            left: Box::new(Term::LiteralInteger(4)),
            right: Box::new(Term::LiteralInteger(4)),
        };
        assert_eq!(eval(&term), Value::Boolean(true));
    }

    #[test]
    fn avalia_igualdade_entre_booleanos() {
        let term = Term::BinaryOperation {
            operator: BinaryOperator::Eq,
            left: Box::new(Term::LiteralBoolean(true)),
            right: Box::new(Term::LiteralBoolean(true)),
        };
        assert_eq!(eval(&term), Value::Boolean(true));
    }

    #[test]
    fn avalia_expressao_aninhada() {
        // (1 + 2) - 3  =>  0
        let term = Term::BinaryOperation {
            operator: BinaryOperator::Sub,
            left: Box::new(Term::BinaryOperation {
                operator: BinaryOperator::Add,
                left: Box::new(Term::LiteralInteger(1)),
                right: Box::new(Term::LiteralInteger(2)),
            }),
            right: Box::new(Term::LiteralInteger(3)),
        };
        assert_eq!(eval(&term), Value::Integer(0));
    }

    #[test]
    fn type_check_and_eval_rejeita_termo_mal_tipado() {
        let term = Term::BinaryOperation {
            operator: BinaryOperator::Add,
            left: Box::new(Term::LiteralInteger(1)),
            right: Box::new(Term::LiteralBoolean(true)),
        };
        assert!(type_check_and_eval(&term).is_err());
    }

    #[test]
    fn type_check_and_eval_aceita_termo_bem_tipado() {
        let term = Term::BinaryOperation {
            operator: BinaryOperator::Eq,
            left: Box::new(Term::LiteralInteger(1)),
            right: Box::new(Term::LiteralInteger(1)),
        };
        assert_eq!(type_check_and_eval(&term), Ok(Value::Boolean(true)));
    }
}
