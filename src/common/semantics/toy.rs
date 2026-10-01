//! Linguagem de brinquedo, só para testar o `core/semantics`.

use std::fmt;

use crate::common::ToLatex;

use super::big_step::{BigStep, EvalDerivation};
use super::derivation::{Derivation, Eval, Typed};
use super::typing::{Typing, TypingDerivation};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Num(i64),
    Bool(bool),
    Add(Box<Term>, Box<Term>),
}

pub fn num(n: i64) -> Term { Term::Num(n) }
pub fn boolean(b: bool) -> Term { Term::Bool(b) }
pub fn add(l: Term, r: Term) -> Term { Term::Add(Box::new(l), Box::new(r)) }

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Num(n) => write!(f, "{n}"),
            Term::Bool(b) => write!(f, "{b}"),
            Term::Add(l, r) => write!(f, "({l} + {r})"),
        }
    }
}

impl ToLatex for Term {
    fn to_latex(&self) -> String {
        match self {
            Term::Num(n) => n.to_string(),
            Term::Bool(b) => format!(r"\text{{{b}}}"),
            Term::Add(l, r) => format!("({} + {})", l.to_latex(), r.to_latex()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type { Nat, Bool }

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self { Type::Nat => "Nat", Type::Bool => "Bool" })
    }
}

impl ToLatex for Type {
    fn to_latex(&self) -> String {
        format!(r"\mathsf{{{self}}}")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Value { Num(i64), Bool(bool) }

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Num(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
        }
    }
}

impl ToLatex for Value {
    fn to_latex(&self) -> String {
        match self {
            Value::Num(n) => n.to_string(),
            Value::Bool(b) => format!(r"\text{{{b}}}"),
        }
    }
}

crate::rules! {
    pub enum TypingRule { Num => "T-Num", Bool => "T-Bool", Add => "T-Add" }
}

crate::rules! {
    pub enum EvalRule { Num => "E-Num", Bool => "E-Bool", Add => "E-Add" }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToyError(pub String);

impl fmt::Display for ToyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for ToyError {}

pub struct ToyTyping;

impl Typing for ToyTyping {
    type Term = Term;
    type Type = Type;
    type Rule = TypingRule;
    type Error = ToyError;

    fn check(term: &Term) -> Result<TypingDerivation<Self>, ToyError> {
        match term {
            Term::Num(_) => Ok(Derivation::axiom(
                Typed::closed(term.clone(), Type::Nat),
                TypingRule::Num,
            )),
            Term::Bool(_) => Ok(Derivation::axiom(
                Typed::closed(term.clone(), Type::Bool),
                TypingRule::Bool,
            )),
            Term::Add(l, r) => {
                let left = Self::check(l)?;
                let right = Self::check(r)?;

                if left.conclusion.ty != Type::Nat || right.conclusion.ty != Type::Nat {
                    return Err(ToyError(format!(
                        "cannot add `{}` and `{}`",
                        left.conclusion.ty, right.conclusion.ty
                    )));
                }

                Ok(Derivation::node(
                    Typed::closed(term.clone(), Type::Nat),
                    TypingRule::Add,
                    vec![left, right],
                ))
            }
        }
    }
}

pub struct ToyBigStep;

impl BigStep for ToyBigStep {
    type Term = Term;
    type Value = Value;
    type Rule = EvalRule;
    type Error = ToyError;

    fn evaluate(term: &Term) -> Result<EvalDerivation<Self>, ToyError> {
        match term {
            Term::Num(n) => Ok(Derivation::axiom(
                Eval { term: term.clone(), value: Value::Num(*n) },
                EvalRule::Num,
            )),
            Term::Bool(b) => Ok(Derivation::axiom(
                Eval { term: term.clone(), value: Value::Bool(*b) },
                EvalRule::Bool,
            )),
            Term::Add(l, r) => {
                let left = Self::evaluate(l)?;
                let right = Self::evaluate(r)?;

                match (left.conclusion.value, right.conclusion.value) {
                    (Value::Num(a), Value::Num(b)) => Ok(Derivation::node(
                        Eval { term: term.clone(), value: Value::Num(a + b) },
                        EvalRule::Add,
                        vec![left, right],
                    )),
                    (a, b) => Err(ToyError(format!("stuck: {a} + {b}"))),
                }
            }
        }
    }
}