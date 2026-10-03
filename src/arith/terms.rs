use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
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
    Integer(i64),
    Boolean(bool),
    Binary { op: BinaryOp, lhs: Box<Term>, rhs: Box<Term> },
    If { condition: Box<Term>, then_branch: Box<Term>, else_branch: Box<Term> },
}

impl Term {
    pub fn integer(n: i64) -> Self {
        Self::Integer(n)
    }

    pub fn boolean(b: bool) -> Self {
        Self::Boolean(b)
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
        matches!(self, Term::Integer(_) | Term::Boolean(_))
    }
}

/// Um operando de operador binário: um `if` precisa de parênteses, porque o
/// `else` iria engolir o resto da expressão.
fn operand(f: &mut fmt::Formatter<'_>, term: &Term) -> fmt::Result {
    match term {
        Term::If { .. } => write!(f, "({term})"),
        _ => write!(f, "{term}"),
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Integer(n) => write!(f, "{n}"),
            Term::Boolean(b) => write!(f, "{b}"),
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
