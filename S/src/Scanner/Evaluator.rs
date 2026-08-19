//! S/src/Scanner/Evaluator.rs
//!
//! Avaliação de termos de `S` já validados pelo type-checker.
//!
//! author:  n-rosenthal
//! date:    2026-08-17
//! version: 0.4

use std::collections::HashMap;

use crate::Scanner::Types::{BinaryOperator, Term, UnaryOperator};

/// Resultado da avaliação de um `Term`.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Unit,
    /// Referência para uma posição da `Store` (índice do vetor de
    /// memória). Nunca aparece na sintaxe de superfície — só é
    /// produzida pela avaliação de `Term::Ref`.
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

/// Ambiente de valores: associa nomes de variáveis ligadas (via
/// `let`) ao seu `Value` já avaliado.
pub type ValueEnv = HashMap<String, Value>;

/// A memória de `S`: um vetor de `Value`, endereçado por índice.
/// `ref t` aloca uma nova posição (`push`); `!t` lê (`store[loc]`);
/// `t1 := t2` escreve (`store[loc] = ...`).
pub type Store = Vec<Value>;

/// Avalia um `Term` já validado pelo type-checker, no escopo vazio e
/// com memória própria (criada do zero para esta avaliação).
///
/// # Contrato
/// Chame `type_of(term)` com sucesso antes de `eval(term)`. Termos
/// mal-tipados causam pânico, pois representam violação do contrato
/// do pipeline (`parse -> type_of -> eval`), não um erro de execução
/// esperado.
pub fn eval(term: &Term) -> Value {
    let mut store = Store::new();
    eval_in(term, &ValueEnv::new(), &mut store)
}

fn eval_in(term: &Term, env: &ValueEnv, store: &mut Store) -> Value {
    match term {
        Term::LiteralInteger(n) => Value::Integer(*n),
        Term::LiteralBoolean(b) => Value::Boolean(*b),
        Term::Unit => Value::Unit,

        Term::Variable(name) => env
            .get(name)
            .cloned()
            .expect("type-checker deveria ter garantido que a variável está ligada"),

        Term::UnaryOperation { operator, operand } => {
            let value = eval_in(operand, env, store);
            match operator {
                UnaryOperator::Not => match value {
                    Value::Boolean(b) => Value::Boolean(!b),
                    _ => unreachable!("type-checker deveria ter rejeitado Not mal-tipado"),
                },
            }
        }

        Term::Conditional { condition, then_branch, else_branch } => {
            match eval_in(condition, env, store) {
                Value::Boolean(true) => eval_in(then_branch, env, store),
                Value::Boolean(false) => eval_in(else_branch, env, store),
                _ => unreachable!("type-checker deveria ter garantido condição booleana"),
            }
        }

        Term::BinaryOperation { operator, left, right } => {
            let left_val = eval_in(left, env, store);
            let right_val = eval_in(right, env, store);

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
                BinaryOperator::Neq => Value::Boolean(left_val != right_val),
                BinaryOperator::Lt => match (left_val, right_val) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Boolean(a < b),
                    _ => unreachable!("type-checker deveria ter rejeitado Lt mal-tipado"),
                },
                BinaryOperator::Leq => match (left_val, right_val) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Boolean(a <= b),
                    _ => unreachable!("type-checker deveria ter rejeitado Leq mal-tipado"),
                },
                BinaryOperator::Gt => match (left_val, right_val) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Boolean(a > b),
                    _ => unreachable!("type-checker deveria ter rejeitado Gt mal-tipado"),
                },
                BinaryOperator::Geq => match (left_val, right_val) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Boolean(a >= b),
                    _ => unreachable!("type-checker deveria ter rejeitado Geq mal-tipado"),
                },
            }
        }

        Term::Let { name, value, body, .. } => {
            let bound_value = eval_in(value, env, store);
            let mut inner_env = env.clone();
            inner_env.insert(name.clone(), bound_value);
            eval_in(body, &inner_env, store)
        }

        Term::Ref(inner) => {
            let value = eval_in(inner, env, store);
            store.push(value);
            Value::Location(store.len() - 1)
        }

        Term::Deref(inner) => match eval_in(inner, env, store) {
            Value::Location(loc) => store
                .get(loc)
                .cloned()
                .expect("type-checker deveria ter garantido uma posição de memória válida"),
            _ => unreachable!("type-checker deveria ter rejeitado Deref de não-referência"),
        },

        Term::Assign { target, value } => {
            let target_val = eval_in(target, env, store);
            let new_val = eval_in(value, env, store);

            match target_val {
                Value::Location(loc) => {
                    store[loc] = new_val;
                    Value::Unit
                }
                _ => unreachable!("type-checker deveria ter rejeitado Assign sobre não-referência"),
            }
        }
    }
}

/// Conveniência: checa o tipo e avalia em sequência, retornando erro
/// de tipo se o termo for mal-tipado.
pub fn type_check_and_eval(
    term: &Term,
) -> Result<Value, crate::Scanner::TypeChecker::TypeError> {
    crate::Scanner::TypeChecker::type_of(term)?;
    Ok(eval(term))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Scanner::Types::Type;

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
    fn avalia_let_simples() {
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
        assert_eq!(eval(&term), Value::Integer(2));
    }

    #[test]
    fn avalia_ref_e_deref() {
        // !(ref 42)  =>  42
        let term = Term::Deref(Box::new(Term::Ref(Box::new(Term::LiteralInteger(42)))));
        assert_eq!(eval(&term), Value::Integer(42));
    }

    #[test]
    fn avalia_assign_e_leitura_subsequente() {
        // let r: Ref Integer = ref 1 in let _: Unit = (r := 99) in !r  =>  99
        let term = Term::Let {
            name: "r".to_string(),
            declared_type: Type::Reference(Box::new(Type::Integer)),
            value: Box::new(Term::Ref(Box::new(Term::LiteralInteger(1)))),
            body: Box::new(Term::Let {
                name: "_ignored".to_string(),
                declared_type: Type::Unit,
                value: Box::new(Term::Assign {
                    target: Box::new(Term::Variable("r".to_string())),
                    value: Box::new(Term::LiteralInteger(99)),
                }),
                body: Box::new(Term::Deref(Box::new(Term::Variable("r".to_string())))),
            }),
        };
        assert_eq!(eval(&term), Value::Integer(99));
    }

    #[test]
    fn duas_referencias_sao_independentes() {
        // let a: Ref Integer = ref 1 in
        // let b: Ref Integer = ref 1 in
        // let _: Unit = (a := 100) in
        // !b   =>  1 (não afetado pela escrita em `a`)
        let term = Term::Let {
            name: "a".to_string(),
            declared_type: Type::Reference(Box::new(Type::Integer)),
            value: Box::new(Term::Ref(Box::new(Term::LiteralInteger(1)))),
            body: Box::new(Term::Let {
                name: "b".to_string(),
                declared_type: Type::Reference(Box::new(Type::Integer)),
                value: Box::new(Term::Ref(Box::new(Term::LiteralInteger(1)))),
                body: Box::new(Term::Let {
                    name: "_ignored".to_string(),
                    declared_type: Type::Unit,
                    value: Box::new(Term::Assign {
                        target: Box::new(Term::Variable("a".to_string())),
                        value: Box::new(Term::LiteralInteger(100)),
                    }),
                    body: Box::new(Term::Deref(Box::new(Term::Variable("b".to_string())))),
                }),
            }),
        };
        assert_eq!(eval(&term), Value::Integer(1));
    }

    #[test]
    fn type_check_and_eval_com_referencia_mal_tipada_e_erro() {
        // 1 := 2
        let term = Term::Assign {
            target: Box::new(Term::LiteralInteger(1)),
            value: Box::new(Term::LiteralInteger(2)),
        };
        assert!(type_check_and_eval(&term).is_err());
    }

    #[test]
    fn type_check_and_eval_com_referencia_bem_tipada() {
        // let r: Ref Integer = ref 10 in !r
        let term = Term::Let {
            name: "r".to_string(),
            declared_type: Type::Reference(Box::new(Type::Integer)),
            value: Box::new(Term::Ref(Box::new(Term::LiteralInteger(10)))),
            body: Box::new(Term::Deref(Box::new(Term::Variable("r".to_string())))),
        };
        assert_eq!(type_check_and_eval(&term), Ok(Value::Integer(10)));
    }
}
