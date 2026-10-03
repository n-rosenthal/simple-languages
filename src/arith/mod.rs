//! Expressões aritméticas e booleanas (TAPL, caps. 3 e 8).

pub mod lexer;
pub mod token;
pub mod terms;
pub mod types;
pub mod values;
pub mod parser;
pub mod typing;
pub mod small_step;
pub mod big_step;
pub mod compile;
pub mod language;
mod latex_impls;

/// O scanner de `arith` é o compartilhado.
pub use crate::common::source::{LineScanner as ArithScanner, ScanError};

pub use big_step::{ArithBigStep, EvalError, EvalRule};
pub use compile::ArithCompiler;
pub use language::{Arith, SyntaxError};
pub use lexer::ArithLexer;
pub use parser::ArithParser;
pub use small_step::{ArithSmallStep, SmallStepRule};
pub use terms::{BinaryOp, Term};
pub use typing::{ArithTyping, TypeError, TypingRule};
pub use types::Type;
pub use values::Value;

#[cfg(test)]
mod laws {
    use super::*;
    use crate::common::language::{law_violations, Language};

    /// Termos de exemplo: bem tipados, mal tipados, travados e com estouro.
    const SAMPLES: &[&str] = &[
        "42",
        "true",
        "(2 * 3) + (4 - 5)",
        "1 + 2 * 3 - 4",
        "if 1 < 2 then 10 else 20",
        "if (1 < 2) == true then 1 + 1 else 0",
        "true == (1 < 2)",
        "(1 < 2) && (2 < 1) || true",
        "if true then (if false then 1 else 2) else 3",
        // travados ou mal tipados
        "true + 1",
        "1 + true",
        "(1 + 2) + true",
        "if 1 then 2 else 3",
        "1 == true",
        "if true then 1 else (true + 1)",
        "if false then 1 else (true + 1)",
        "true && 1",
        // estouro de i64: dá a volta, e continua bem tipado e seguro
        "9223372036854775807 + 1",
        "9223372036854775807 * 2",
    ];

    #[test]
    fn every_law_holds_on_every_sample() {
        for source in SAMPLES {
            let term = Arith::parse(source).unwrap_or_else(|e| panic!("{source}: {e}"));
            let violated = law_violations::<Arith>(&term);
            assert!(violated.is_empty(), "{source}: {violated:?}");
        }
    }

    #[test]
    fn well_typed_terms_never_get_stuck() {
        use crate::common::semantics::{run, Typing};

        for source in SAMPLES {
            let term = Arith::parse(source).unwrap();
            if ArithTyping::is_well_typed(&term) {
                assert!(!run::<ArithSmallStep>(term).is_stuck(), "{source}");
            }
        }
    }

    #[test]
    fn the_examples_all_parse() {
        for example in Arith::examples() {
            assert!(Arith::parse(example.source).is_ok(), "{}", example.title);
        }
    }
}
