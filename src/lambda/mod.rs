//! O cálculo λ simplesmente tipado (TAPL, caps. 5 e 9).

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
pub mod language;
mod latex_impls;

pub use big_step::{EvalError, EvalRule, LambdaBigStep};
pub use compile::{CompileError, LambdaCompiler};
pub use language::{Lambda, SyntaxError};
pub use lexer::LambdaLexer;
pub use parser::LambdaParser;
pub use scanner::LambdaScanner;
pub use small_step::{LambdaSmallStep, StepRule};
pub use terms::Term;
pub use typing::{LambdaTyping, TypeError, TypingRule};
pub use types::Type;

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
    /// (travam), erros de tipo que mesmo assim avaliam, e uma captura.
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
    use crate::common::language::law_violations;
    use crate::common::semantics::Typing;

    #[test]
    fn the_well_typed_samples_are_well_typed() {
        for source in WELL_TYPED {
            assert!(LambdaTyping::is_well_typed(&parse(source)), "{source}");
        }
    }

    #[test]
    fn every_law_holds_on_every_sample() {
        for t in all() {
            let violated = law_violations::<Lambda>(&t);
            assert!(violated.is_empty(), "{t}: {violated:?}");
        }
    }
}
