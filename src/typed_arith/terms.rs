//! Termos de `typed-arith` (TAPL, fig. 3-2).
//!
//! ```text
//! t ::= true | false | if t then t else t | 0 | succ t | pred t | iszero t
//! v ::= true | false | nv
//! nv ::= 0 | succ nv
//! ```

use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    True,
    False,
    If { condition: Box<Term>, then_branch: Box<Term>, else_branch: Box<Term> },
    Zero,
    Succ(Box<Term>),
    Pred(Box<Term>),
    IsZero(Box<Term>),
}

impl Term {
    pub fn boolean(value: bool) -> Self {
        if value {
            Self::True
        } else {
            Self::False
        }
    }

    pub fn if_then_else(condition: Term, then_branch: Term, else_branch: Term) -> Self {
        Self::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        }
    }

    pub fn succ(term: Term) -> Self {
        Self::Succ(Box::new(term))
    }

    pub fn pred(term: Term) -> Self {
        Self::Pred(Box::new(term))
    }

    pub fn is_zero(term: Term) -> Self {
        Self::IsZero(Box::new(term))
    }

    /// O numeral `n`: `succ (... (succ 0))`.
    pub fn numeral(n: usize) -> Self {
        (0..n).fold(Self::Zero, |term, _| Self::succ(term))
    }

    /// `nv ::= 0 | succ nv`.
    pub fn is_numeric_value(&self) -> bool {
        self.numeral_value().is_some()
    }

    /// O `n` de um valor numérico `succ^n 0`.
    pub fn numeral_value(&self) -> Option<usize> {
        let mut n = 0;
        let mut term = self;
        loop {
            match term {
                Term::Zero => return Some(n),
                Term::Succ(inner) => {
                    n += 1;
                    term = inner;
                }
                _ => return None,
            }
        }
    }

    /// `v ::= true | false | nv`.
    pub fn is_value(&self) -> bool {
        matches!(self, Term::True | Term::False) || self.is_numeric_value()
    }
}

/// Valores numéricos e átomos não precisam de parênteses como operando.
fn is_atom(term: &Term) -> bool {
    matches!(term, Term::True | Term::False) || term.is_numeric_value()
}

fn operand(f: &mut fmt::Formatter<'_>, term: &Term) -> fmt::Result {
    if is_atom(term) {
        write!(f, "{term}")
    } else {
        write!(f, "({term})")
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(n) = self.numeral_value() {
            return write!(f, "{n}");
        }

        match self {
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::If { condition, then_branch, else_branch } => {
                write!(f, "if {condition} then {then_branch} else {else_branch}")
            }
            Self::Zero => write!(f, "0"),
            Self::Succ(t) => {
                write!(f, "succ ")?;
                operand(f, t)
            }
            Self::Pred(t) => {
                write!(f, "pred ")?;
                operand(f, t)
            }
            Self::IsZero(t) => {
                write!(f, "iszero ")?;
                operand(f, t)
            }
        }
    }
}
