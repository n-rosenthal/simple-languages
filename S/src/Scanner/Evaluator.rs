//! S/src/Scanner/Evaluator.rs
//!
//! Avaliação de termos de `S` já validados pelo type-checker.
//!
//! author:  n-rosenthal
//! date:    2026-08-17
//! version: 0.5 (Sequence + REPL state support)

use std::collections::HashMap;

use crate::Scanner::Types::{BinaryOperator, Term, UnaryOperator};

/// Resultado da avaliação de um `Term`.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Unit,
    Location(usize),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Unit => write!(f, "unit"),
            Value::Location(loc) => write!(f, "<loc {loc}>"),
        }
    }
}

pub type ValueEnv = HashMap<String, Value>;
pub type Store = Vec<Value>;

/// Avalia um termo sob um ambiente e memória existentes (permite persistência no REPL).
pub fn eval_in(term: &Term, env: &mut ValueEnv, store: &mut Store) -> Value {
    match term {
        Term::LiteralInteger { value, .. } => Value::Integer(*value),
        Term::LiteralBoolean { value, .. } => Value::Boolean(*value),
        Term::Unit { .. } => Value::Unit,

        Term::Variable { name, .. } => env
            .get(name)
            .cloned()
            .expect("type-checker deveria ter garantido que a variável está ligada"),

        Term::UnaryOperation { operator, operand, .. } => {
            let value = eval_in(operand, env, store);
            match operator {
                UnaryOperator::Not => match value {
                    Value::Boolean(b) => Value::Boolean(!b),
                    _ => unreachable!("type-checker rejeitou Not mal-tipado"),
                },
            }
        }

        Term::Conditional { condition, then_branch, else_branch, .. } => {
            match eval_in(condition, env, store) {
                Value::Boolean(true) => eval_in(then_branch, env, store),
                Value::Boolean(false) => eval_in(else_branch, env, store),
                _ => unreachable!("type-checker garantiu condição booleana"),
            }
        }

        Term::BinaryOperation { operator, left, right, .. } => {
            let left_val = eval_in(left, env, store);
            let right_val = eval_in(right, env, store);

            match operator {
                BinaryOperator::Add => match (left_val, right_val) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Integer(a + b),
                    _ => unreachable!(),
                },
                BinaryOperator::Sub => match (left_val, right_val) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Integer(a - b),
                    _ => unreachable!(),
                },
                BinaryOperator::Eq => Value::Boolean(left_val == right_val),
                BinaryOperator::Neq => Value::Boolean(left_val != right_val),
                BinaryOperator::Lt => match (left_val, right_val) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Boolean(a < b),
                    _ => unreachable!(),
                },
                BinaryOperator::Leq => match (left_val, right_val) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Boolean(a <= b),
                    _ => unreachable!(),
                },
                BinaryOperator::Gt => match (left_val, right_val) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Boolean(a > b),
                    _ => unreachable!(),
                },
                BinaryOperator::Geq => match (left_val, right_val) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Boolean(a >= b),
                    _ => unreachable!(),
                },
            }
        }

        Term::Let { name, value, body, .. } => {
            let bound_value = eval_in(value, env, store);
            env.insert(name.clone(), bound_value);
            eval_in(body, env, store)
        }

        Term::Ref { inner, .. } => {
            let value = eval_in(inner, env, store);
            store.push(value);
            Value::Location(store.len() - 1)
        }

        Term::Deref { inner, .. } => match eval_in(inner, env, store) {
            Value::Location(loc) => store
                .get(loc)
                .cloned()
                .expect("posição de memória válida"),
            _ => unreachable!(),
        },

        Term::Assign { target, value, .. } => {
            let target_val = eval_in(target, env, store);
            let new_val = eval_in(value, env, store);

            match target_val {
                Value::Location(loc) => {
                    store[loc] = new_val;
                    Value::Unit
                }
                _ => unreachable!(),
            }
        }

        Term::Sequence { left, right, .. } => {
            let _ = eval_in(left, env, store);
            eval_in(right, env, store)
        }
    }
}

/// Conveniência: checa o tipo e avalia, retornando erro de tipo se houver.
pub fn type_check_and_eval(
    term: &Term,
    env: &mut ValueEnv,
    store: &mut Store,
) -> Result<Value, crate::Scanner::TypeChecker::TypeError> {
    crate::Scanner::TypeChecker::type_of(term)?;
    Ok(eval_in(term, env, store))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Scanner::Types::{Span, Type};

    fn dummy_span() -> Span {
        Span::new(1, 1)
    }

    #[test]
    fn avalia_sequencia() {
        let term = Term::Sequence {
            left: Box::new(Term::Unit { span: dummy_span() }),
            right: Box::new(Term::LiteralInteger { value: 42, span: dummy_span() }),
            span: dummy_span(),
        };
        let mut env = ValueEnv::new();
        let mut store = Store::new();
        assert_eq!(eval_in(&term, &mut env, &mut store), Value::Integer(42));
    }

    #[test]
    fn avalia_let_persistente() {
        let term = Term::Let {
            name: "x".to_string(),
            declared_type: Type::Integer,
            value: Box::new(Term::LiteralInteger { value: 10, span: dummy_span() }),
            body: Box::new(Term::Unit { span: dummy_span() }),
            span: dummy_span(),
        };
        let mut env = ValueEnv::new();
        let mut store = Store::new();
        let result = eval_in(&term, &mut env, &mut store);
        assert_eq!(result, Value::Unit);
        assert_eq!(env.get("x"), Some(&Value::Integer(10)));
    }
}
