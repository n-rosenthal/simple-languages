///!    Valores de `arith-extensions`: inteiros e booleanos, com operadores binários.

use std::fmt;

use super::terms::{BinaryOp, Term};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(b) => write!(f, "{b}"),
        }
    }
}

impl From<Value> for Term {
    fn from(value: Value) -> Term {
        match value {
            Value::Integer(n) => Term::Integer(n),
            Value::Boolean(b) => Term::Boolean(b),
        }
    }
}

/// A tabela de casos dos operadores sobre valores (as regras δ). É a
/// única definição: a semântica natural a transforma em erro, e a
/// estrutural, em termo travado.
///
/// `None` quando os operandos não servem ao operador. A aritmética dá a
/// volta em 64 bits (*wrapping*): o livro usa naturais sem limite, e
/// tratar o estouro como "travado" faria termos bem tipados travarem,
/// violando o teorema de progresso.
pub fn apply(op: BinaryOp, lhs: Value, rhs: Value) -> Option<Value> {
    use Value::{Boolean, Integer};

    match (op, lhs, rhs) {
        (BinaryOp::Add, Integer(a), Integer(b)) => Some(Integer(a.wrapping_add(b))),
        (BinaryOp::Sub, Integer(a), Integer(b)) => Some(Integer(a.wrapping_sub(b))),
        (BinaryOp::Mul, Integer(a), Integer(b)) => Some(Integer(a.wrapping_mul(b))),
        (BinaryOp::LessThan, Integer(a), Integer(b)) => Some(Boolean(a < b)),
        (BinaryOp::Equal, Integer(a), Integer(b)) => Some(Boolean(a == b)),
        (BinaryOp::Equal, Boolean(a), Boolean(b)) => Some(Boolean(a == b)),
        (BinaryOp::And, Boolean(a), Boolean(b)) => Some(Boolean(a && b)),
        (BinaryOp::Or, Boolean(a), Boolean(b)) => Some(Boolean(a || b)),
        (BinaryOp::Div, Integer(a), Integer(b)) => Some(Integer(a.wrapping_div(b))),
        (BinaryOp::Mod, Integer(a), Integer(b)) => Some(Integer(a.wrapping_rem(b))),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn int(n: i64) -> Value {
        Value::Integer(n)
    }

    #[test]
    fn arithmetic() {
        assert_eq!(apply(BinaryOp::Add, int(1), int(2)), Some(int(3)));
        assert_eq!(apply(BinaryOp::Sub, int(4), int(5)), Some(int(-1)));
        assert_eq!(apply(BinaryOp::Mul, int(2), int(3)), Some(int(6)));
    }

    #[test]
    fn comparisons_and_logic() {
        let (t, f) = (Value::Boolean(true), Value::Boolean(false));
        assert_eq!(apply(BinaryOp::LessThan, int(1), int(2)), Some(t));
        assert_eq!(apply(BinaryOp::Equal, t, f), Some(f));
        assert_eq!(apply(BinaryOp::And, t, f), Some(f));
        assert_eq!(apply(BinaryOp::Or, t, f), Some(t));
    }

    #[test]
    fn wrong_operand_types_have_no_rule() {
        assert_eq!(apply(BinaryOp::Add, Value::Boolean(true), int(1)), None);
        assert_eq!(apply(BinaryOp::Equal, int(1), Value::Boolean(true)), None);
    }

    #[test]
    fn arithmetic_wraps_instead_of_getting_stuck() {
        assert_eq!(apply(BinaryOp::Add, int(i64::MAX), int(1)), Some(int(i64::MIN)));
        assert_eq!(apply(BinaryOp::Sub, int(i64::MIN), int(1)), Some(int(i64::MAX)));
        assert_eq!(apply(BinaryOp::Mul, int(i64::MAX), int(2)), Some(int(-2)));
    }
}
