use super::machine::Machine;
use super::step::{Step, Transition};

impl From<Value> for Term {
    fn from(v: Value) -> Term {
        match v {
            Value::Num(n) => Term::Num(n),
            Value::Bool(b) => Term::Bool(b),
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
            (Term::Num(a), Term::Num(b)) => {
                Some(Transition::new(StepRule::AddCompute, term.clone(), Term::Num(a + b)))
            }
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