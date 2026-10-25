//! Booleanos e números naturais, com tipos (TAPL, caps. 3 e 8).
//!
//! É a linguagem do livro: `true`, `false`, `if`, `0`, `succ`, `pred` e
//! `iszero`, com os tipos `Bool` e `Nat`. Termos como `succ true` ou
//! `if 0 then ...` são mal tipados *e* travados, o caso de estudo de
//! progresso e preservação.

pub mod token;
pub mod lexer;
pub mod terms;
pub mod types;
pub mod parser;
pub mod typing;
pub mod small_step;
pub mod big_step;
pub mod compile;
pub mod language;
mod latex_impls;

/// O scanner é o compartilhado.
pub use crate::common::source::{LineScanner as TypedArithScanner, ScanError};

pub use big_step::{EvalError, EvalRule, TypedArithBigStep};
pub use compile::{CompileError, TypedArithCompiler};
pub use language::{SyntaxError, TypedArith};
pub use lexer::TypedArithLexer;
pub use parser::TypedArithParser;
pub use small_step::{StepRule, TypedArithSmallStep};
pub use terms::Term;
pub use typing::{TypeError, TypedArithTyping, TypingRule};
pub use types::Type;
