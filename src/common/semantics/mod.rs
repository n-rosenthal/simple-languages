

//! `src/common/semantics/mod.rs` ...
pub mod rule;
pub mod derivation;
pub mod typing;
pub mod big_step;
pub mod laws;
// pub mod step;
// pub mod machine;

#[cfg(test)]
pub(crate) mod toy;

pub use rule::Rule;
pub use derivation::{Derivation, Eval, Reduces, Typed};
pub use typing::{Typing, TypingDerivation};
pub use big_step::{BigStep, EvalDerivation};
