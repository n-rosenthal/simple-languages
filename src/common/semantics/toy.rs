//! Linguagem de brinquedo, só para testar o `common::semantics`.
//!
//! Naturais, booleanos e soma, com tipagem, big-step, small-step e uma
//! pequena máquina de pilha. Compilada apenas em testes.

use std::fmt;

use crate::common::ToLatex;

use super::big_step::{BigStep, EvalDerivation};
use super::derivation::{Derivation, Eval, Typed};
use super::machine::Machine;
use super::step::{Step, Transition};
use super::typing::{Typing, TypingDerivation};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Num(i64),
    Bool(bool),
    Add(Box<Term>, Box<Term>),
}

pub fn num(n: i64) -> Term {
    Term::Num(n)
}

pub fn boolean(b: bool) -> Term {
    Term::Bool(b)
}

pub fn add(l: Term, r: Term) -> Term {
    Term::Add(Box::new(l), Box::new(r))
}

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
pub enum Type {
    Nat,
    Bool,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Type::Nat => "Nat",
            Type::Bool => "Bool",
        })
    }
}

impl ToLatex for Type {
    fn to_latex(&self) -> String {
        format!(r"\mathsf{{{}}}", self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Value {
    Num(i64),
    Bool(bool),
}

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

impl From<Value> for Term {
    fn from(v: Value) -> Term {
        match v {
            Value::Num(n) => Term::Num(n),
            Value::Bool(b) => Term::Bool(b),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToyError(pub String);

impl fmt::Display for ToyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for ToyError {}

// --- tipagem ----------------------------------------------------------------

crate::rules! {
    pub enum TypingRule {
        Num => "T-Num",
        Bool => "T-Bool",
        Add => "T-Add",
    }
}

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

// --- big-step ---------------------------------------------------------------

crate::rules! {
    pub enum EvalRule {
        Num => "E-Num",
        Bool => "E-Bool",
        Add => "E-Add",
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

// --- small-step -------------------------------------------------------------

crate::rules! {
    pub enum StepRule {
        AddLeft => "E-Add1",
        AddRight => "E-Add2",
        AddCompute => "E-AddConst",
    }
}

pub struct ToySmallStep;

impl Step for ToySmallStep {
    type State = Term;
    type Rule = StepRule;

    fn is_final(term: &Term) -> bool {
        matches!(term, Term::Num(_) | Term::Bool(_))
    }

    fn step(term: &Term) -> Option<Transition<StepRule, Term>> {
        let Term::Add(l, r) = term else { return None };

        if !Self::is_final(l) {
            let inner = Self::step(l)?;
            let next = add(inner.to, (**r).clone());
            return Some(Transition::new(StepRule::AddLeft, term.clone(), next));
        }

        if !Self::is_final(r) {
            let inner = Self::step(r)?;
            let next = add((**l).clone(), inner.to);
            return Some(Transition::new(StepRule::AddRight, term.clone(), next));
        }

        match (&**l, &**r) {
            (Term::Num(a), Term::Num(b)) => Some(Transition::new(
                StepRule::AddCompute,
                term.clone(),
                Term::Num(a + b),
            )),
            _ => None, // travado: `true + 1`
        }
    }
}

// --- máquina de pilha -------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Instr {
    Push(Value),
    Add,
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Push(v) => write!(f, "push {v}"),
            Instr::Add => f.write_str("add"),
        }
    }
}

crate::rules! {
    pub enum MachineRule {
        Push => "M-Push",
        Add => "M-Add",
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MachineState {
    pub code: Vec<Instr>,
    pub stack: Vec<Value>,
}

impl fmt::Display for MachineState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let join = |items: Vec<String>| items.join("; ");
        write!(
            f,
            "⟨[{}], [{}]⟩",
            join(self.code.iter().map(|i| i.to_string()).collect()),
            join(self.stack.iter().map(|v| v.to_string()).collect()),
        )
    }
}

pub struct ToyMachine;

fn compile(term: &Term, out: &mut Vec<Instr>) {
    match term {
        Term::Num(n) => out.push(Instr::Push(Value::Num(*n))),
        Term::Bool(b) => out.push(Instr::Push(Value::Bool(*b))),
        Term::Add(l, r) => {
            compile(l, out);
            compile(r, out);
            out.push(Instr::Add);
        }
    }
}

impl Step for ToyMachine {
    type State = MachineState;
    type Rule = MachineRule;

    fn is_final(state: &MachineState) -> bool {
        state.code.is_empty()
    }

    fn step(state: &MachineState) -> Option<Transition<MachineRule, MachineState>> {
        let (first, rest) = state.code.split_first()?;
        let mut stack = state.stack.clone();

        let rule = match first {
            Instr::Push(v) => {
                stack.push(*v);
                MachineRule::Push
            }
            Instr::Add => {
                let (b, a) = (stack.pop()?, stack.pop()?); // pilha vazia: travado
                match (a, b) {
                    (Value::Num(x), Value::Num(y)) => stack.push(Value::Num(x + y)),
                    _ => return None, // operandos incompatíveis: travado
                }
                MachineRule::Add
            }
        };

        let next = MachineState { code: rest.to_vec(), stack };
        Some(Transition::new(rule, state.clone(), next))
    }
}

impl Machine for ToyMachine {
    type Term = Term;
    type Value = Value;

    fn load(term: &Term) -> MachineState {
        let mut code = Vec::new();
        compile(term, &mut code);
        MachineState { code, stack: Vec::new() }
    }

    fn unload(state: &MachineState) -> Option<Value> {
        match (state.code.is_empty(), state.stack.as_slice()) {
            (true, [only]) => Some(*only),
            _ => None,
        }
    }
}
