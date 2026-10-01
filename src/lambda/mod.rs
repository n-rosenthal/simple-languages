pub mod lexer;
pub mod scanner;
pub mod token;
pub mod terms;
pub mod types;
pub mod parser;
pub mod typing;
pub mod small_step;
pub mod big_step;
pub mod compile;
mod latex_impls;

pub use lexer::LambdaLexer;
pub use scanner::LambdaScanner;
pub use parser::LambdaParser;
pub use terms::Term;
pub use types::Type;
pub use typing::{LambdaTyping, TypeError, TypingRule};
pub use small_step::{LambdaSmallStep, StepRule};
pub use big_step::{EvalError, EvalRule, LambdaBigStep};
pub use compile::{CompileError, LambdaCompiler};

/// Termos de exemplo compartilhados pelos testes de `lambda`.
#[cfg(test)]
pub(crate) mod testing {
    use super::{LambdaLexer, LambdaParser, LambdaScanner, Term};
    use crate::common::{Lexer, Parser, Scanner};

    pub fn parse(source: &str) -> Term {
        let lines = LambdaScanner::scan(source).expect("scan");
        let tokens = LambdaLexer::analyze(&lines).expect("lex");
        LambdaParser::parse(&tokens).expect("parse")
    }

    /// Bem tipados (e fechados).
    pub const WELL_TYPED: &[&str] = &[
        "λx:A. x",
        "λx:A. λy:A. x",
        "(λf:A->A. f) (λy:A. y)",
        "(λf:A->A. λx:A. f x) (λy:A. y)",
        "λf:A->A. λx:A. f (f x)",
        "(λf:A->A. λx:A. f (f x)) (λy:A. y)",
        "(λk:A->A->A. k) (λx:A. λy:A. x)",
    ];

    /// Mal tipados ou abertos, mas que terminam: variáveis livres
    /// (travam), um erro de tipo que mesmo assim avalia, e uma captura.
    pub const OTHERS: &[&str] = &[
        "x",
        "x y",
        "(λx:A. x) (λy:A. y)",
        "(λx:A. x x) (λy:A. y)",
        "(λx:A. λy:A. x) y",
    ];

    pub fn all() -> Vec<Term> {
        WELL_TYPED.iter().chain(OTHERS).map(|s| parse(s)).collect()
    }
}

#[cfg(test)]
mod laws {
    use super::testing::{all, parse, WELL_TYPED};
    use super::*;
    use crate::common::machine_language::compilation_is_correct;
    use crate::common::semantics::laws::*;
    use crate::common::semantics::{run, Typing};

    #[test]
    fn the_well_typed_samples_are_well_typed() {
        for source in WELL_TYPED {
            assert!(LambdaTyping::is_well_typed(&parse(source)), "{source}");
        }
    }

    #[test]
    fn final_states_do_not_step() {
        for t in all() {
            assert!(final_states_do_not_step::<LambdaSmallStep>(&t), "{t}");
        }
    }

    #[test]
    fn traces_are_connected() {
        for t in all() {
            assert!(trace_is_connected(&run::<LambdaSmallStep>(t.clone())), "{t}");
        }
    }

    #[test]
    fn small_step_agrees_with_big_step() {
        for t in all() {
            assert!(
                small_step_agrees_with_big_step::<LambdaSmallStep, LambdaBigStep>(&t),
                "{t}"
            );
        }
    }

    #[test]
    fn well_typed_terms_evaluate() {
        for t in all() {
            assert!(well_typed_evaluates::<LambdaTyping, LambdaBigStep>(&t), "{t}");
        }
    }

    #[test]
    fn progress() {
        for t in all() {
            assert!(well_typed_never_gets_stuck::<LambdaTyping, LambdaSmallStep>(&t), "{t}");
        }
    }

    #[test]
    fn preservation_holds() {
        for t in all() {
            assert!(preservation::<LambdaTyping, LambdaSmallStep>(&t), "{t}");
        }
    }

    #[test]
    fn compilation_is_correct_for_lambda() {
        for t in all() {
            assert!(compilation_is_correct::<LambdaCompiler, LambdaBigStep>(&t), "{t}");
        }
    }
}