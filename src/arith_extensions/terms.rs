///! Termos de `arith-extensions`.

use std::fmt;


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LessThan,
    Equal,
    And,
    Or,
}

impl BinaryOp {
    pub fn symbol(self) -> &'static str {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::LessThan => "<",
            BinaryOp::Equal => "==",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.symbol())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    //  Naturals with Peano arithmetic
    Zero,
    Succ(Box<Term>),
    Pred(Box<Term>),
    IsZero(Box<Term>),



    Integer(i64),
    Boolean(bool),

    Binary { op: BinaryOp, lhs: Box<Term>, rhs: Box<Term> },
    If { condition: Box<Term>, then_branch: Box<Term>, else_branch: Box<Term> },


}

impl Term {
    //  Construtores de valores
    pub fn integer(n: i64) -> Self {
        Self::Integer(n)
    }

    pub fn boolean(b: bool) -> Self {
        Self::Boolean(b)
    }

    pub fn zero() -> Self {
        Self::Zero
    }

    pub fn succ(n: Term) -> Self {
        Self::Succ(Box::new(n))
    }

    pub fn pred(n: Term) -> Self {
        Self::Pred(Box::new(n))
    }

    pub fn is_zero(n: Term) -> Self {
        Self::IsZero(Box::new(n))
    }

    pub fn natural(n: u64) -> Self {
        let mut term = Term::Zero;
        for _ in 0..n {
            term = Term::Succ(Box::new(term));
        }
        term
    }

    pub fn binary(op: BinaryOp, lhs: Term, rhs: Term) -> Self {
        Self::Binary { op, lhs: Box::new(lhs), rhs: Box::new(rhs) }
    }

    pub fn if_then_else(condition: Term, then_branch: Term, else_branch: Term) -> Self {
        Self::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        }
    }

    /// Valores: literais.
    pub fn is_value(&self) -> bool {
        match self {
            Term::Integer(_) => true,
            Term::Boolean(_) => true,
            Term::Zero => true,

            // Um sucessor é valor somente quando seu argumento é valor.
            Term::Succ(t) => t.is_value(),

            // pred e iszero nunca são valores.
            Term::Pred(_) | Term::IsZero(_) => false,

            Term::Binary { .. } | Term::If { .. } => false,
        }
    }
}

/// Um operando de operador binário: um `if` precisa de parênteses, porque o
/// `else` iria engolir o resto da expressão.
fn operand(f: &mut fmt::Formatter<'_>, term: &Term) -> fmt::Result {
    match term {
        Term::If { .. } => write!(f, "({term})"),
        Term::Zero => write!(f, "0"),
        Term::Succ(n) => write!(f, "S({n})"),
        Term::Pred(n) => write!(f, "P({n})"),
        Term::IsZero(n) => write!(f, "IsZero({n})"),
        _ => write!(f, "{term}"),
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Integer(n) => write!(f, "{n}"),
            Term::Boolean(b) => write!(f, "{b}"),
            Term::Zero => write!(f, "0"),
            Term::Succ(n) => write!(f, "S({n})"),
            Term::Pred(n) => write!(f, "P({n})"),
            Term::IsZero(n) => write!(f, "IsZero({n})"),
            Term::Binary { op, lhs, rhs } => {
                f.write_str("(")?;
                operand(f, lhs)?;
                write!(f, " {op} ")?;
                operand(f, rhs)?;
                f.write_str(")")
            }
            Term::If { condition, then_branch, else_branch } => {
                write!(f, "if {condition} then {then_branch} else {else_branch}")
            }
        }
    }
}
