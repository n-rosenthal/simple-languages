//! Termos lambda sem nomes explícitos, usando índices de De Bruijn.
//!
//! Inspirado em *Types and Programming Languages* (Benjamin C. Pierce),
//! capítulos 6–7.
//!
//! A representação nameless elimina os nomes das variáveis ligadas:
//! uma variável é representada por um índice que indica a distância
//! até seu ligador (binder) mais próximo.
//!
//! Por exemplo, a identidade λx. x é representada por:
//!
//!     Abs(Var(0))
//!
//! Os índices de De Bruijn permitem comparar termos módulo renomeação
//! de variáveis ligadas (α-equivalência), mas exigem operações de
//! deslocamento (shifting) e substituição que respeitem os ligadores.

pub mod token;
pub mod lexer;
pub mod terms;
pub mod parser;
pub mod shifting;
pub mod substitution;
pub mod small_step;
pub mod big_step;
pub mod compile;
pub mod language;

mod latex_impls;

/// Scanner compartilhado pelo projeto.
pub use crate::common::source::{
    LineScanner as NamelessTermsScanner,
    ScanError,
};

pub use big_step::{
    EvalError,
    EvalRule,
    NamelessTermsBigStep,
};

pub use compile::{
    CompileError,
    NamelessTermsCompiler,
};

pub use language::{
    NamelessTerms,
    NamelessTermsName,
    NamelessTermsNormal,
    NamelessTermsValue,
    SyntaxError,
    Strategy,
};

pub use lexer::NamelessTermsLexer;
pub use parser::NamelessTermsParser;

pub use small_step::{
    ByNameRule,
    ByValueRule,
    CallByName,
    CallByValue,
    NormalOrder,
    NormalOrderRule,
};

pub use terms::Term;