//! O cálculo λ sem tipos (TAPL, caps. 5 a 7).
//!
//! `t ::= x | λx. t | t t`. Uma só sintaxe, e três estratégias de avaliação,
//! cada uma uma linguagem do interpretador:
//!
//! | linguagem       | estratégia                                | valores                 |
//! |-----------------|-------------------------------------------|-------------------------|
//! | `lambda`        | call-by-value (a do livro, com semântica natural e máquina) | abstrações |
//! | `lambda-name`   | call-by-name                              | abstrações              |
//! | `lambda-normal` | ordem normal (mais externo, mais à esquerda, sob λ) | formas normais |
//!
//! A β-redução completa do livro é uma *relação* (qualquer redex pode ser
//! escolhido); `Step` é uma função, então a ordem normal faz o papel de
//! sua estratégia determinística.

pub mod token;
pub mod lexer;
pub mod terms;
pub mod parser;
pub mod small_step;
pub mod big_step;
pub mod compile;
pub mod language;
mod latex_impls;

/// O scanner é o compartilhado.
pub use crate::common::source::{LineScanner as LambdaScanner, ScanError};

pub use big_step::{EvalError, EvalRule, LambdaBigStep};
pub use compile::{CompileError, LambdaCompiler};
pub use language::{Lambda, LambdaName, LambdaNormal, LambdaValue, Strategy, SyntaxError};
pub use lexer::LambdaLexer;
pub use parser::LambdaParser;
pub use small_step::{ByNameRule, ByValueRule, CallByName, CallByValue, NormalOrder, NormalOrderRule};
pub use terms::Term;
