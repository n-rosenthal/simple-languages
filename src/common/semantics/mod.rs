//! Os traits de semântica: regras, derivações, passos, big-step,
//! máquinas e tipagem. Cada linguagem os implementa.

pub mod rule;
pub mod derivation;
pub mod step;
pub mod big_step;
pub mod machine;
pub mod typing;
pub mod laws;

#[cfg(test)]
pub(crate) mod toy;

pub use big_step::{BigStep, EvalDerivation};
pub use derivation::{Derivation, Eval, Reduces, Typed};
pub use machine::{Execution, Machine};
pub use rule::{Rule, Schema};
pub use step::{run, run_with_fuel, steps, Outcome, Step, Trace, Transition, DEFAULT_FUEL};
pub use typing::{NoTyping, NoTypingRule, Typing, TypingDerivation, Untyped};
