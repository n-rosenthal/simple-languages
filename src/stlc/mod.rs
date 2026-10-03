//! O cálculo λ simplesmente tipado com booleanos (TAPL, caps. 9 e 10).

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

pub use big_step::{EvalError, EvalRule, StlcBigStep};
pub use compile::{CompileError, StlcCompiler};
pub use language::{Stlc, SyntaxError};
pub use lexer::StlcLexer;
pub use parser::StlcParser;
pub use scanner::StlcScanner;
pub use small_step::{StlcSmallStep, StepRule};
pub use terms::Term;
pub use typing::{StlcTyping, TypeError, TypingRule};
pub use types::Type;

/// Termos de exemplo compartilhados pelos testes de `lambda`.
#[cfg(test)]
pub(crate) mod testing {
    use super::{StlcLexer, StlcParser, StlcScanner, Term};
    use crate::common::{Lexer, Parser, Scanner};

    pub fn parse(source: &str) -> Term {
        let lines = StlcScanner::scan(source).expect("scan");
        let tokens = StlcLexer::analyze(&lines).expect("lex");
        StlcParser::parse(&tokens).expect("parse")
    }

    /// Bem tipados (e fechados).
    pub const WELL_TYPED: &[&str] = &[
        "true",
        "false",
        "λb:Bool. if b then false else true",
        "(λb:Bool. if b then false else true) true",
        "if true then (λx:Bool. x) else (λx:Bool. if x then false else true)",
        "(λf:Bool->Bool. f true) (λb:Bool. b)",
        "λf:Bool->Bool. λb:Bool. f (f b)",
        "(λf:Bool->Bool. λb:Bool. f (f b)) (λb:Bool. if b then false else true) true",
        "if (if true then false else true) then (λx:A. x) else (λx:A. x)",
        "(λb:Bool. λx:A. if b then x else x) true",
        "λx:A. x",
        "λx:A. λy:A. x",
        "(λf:A->A. f) (λy:A. y)",
        "(λf:A->A. λx:A. f x) (λy:A. y)",
        "λf:A->A. λx:A. f (f x)",
        "(λf:A->A. λx:A. f (f x)) (λy:A. y)",
        "(λk:A->A->A. k) (λx:A. λy:A. x)",
    ];

    /// Mal tipados ou abertos, mas que terminam: variáveis livres e
    /// aplicações de booleanos (travam), erros de tipo que mesmo assim
    /// avaliam, e uma captura.
    pub const OTHERS: &[&str] = &[
        "true false",
        "if (λx:Bool. x) then true else false",
        "if true then true else (true true)",
        "if false then true else (true true)",
        "(λb:Bool. b) (λx:Bool. x)",
        "if x then true else false",
        "x true",
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
    fn the_examples_all_parse() {
        use crate::common::language::Language;

        for example in Stlc::examples() {
            // um exemplo pode ser uma definição `nome = termo`
            let term = example.source.split_once(" = ").map_or(example.source, |(_, rhs)| rhs);
            assert!(Stlc::parse(term).is_ok(), "{}", example.title);
        }
    }

    #[test]
    fn the_well_typed_samples_are_well_typed() {
        for source in WELL_TYPED {
            assert!(StlcTyping::is_well_typed(&parse(source)), "{source}");
        }
    }

    /// Formas canônicas (TAPL, lema 9.3.4): um valor fechado de tipo `Bool` é
    /// `true` ou `false`, e um de tipo `T1->T2` é uma abstração.
    #[test]
    fn canonical_forms() {
        for source in WELL_TYPED {
            let term = parse(source);
            if !term.is_value() {
                continue;
            }
            match StlcTyping::type_of(&term).unwrap() {
                Type::Bool => assert!(matches!(term, Term::True | Term::False), "{source}"),
                Type::Arrow(..) => assert!(matches!(term, Term::Lambda { .. }), "{source}"),
                Type::Base(_) => panic!("{source}: um valor fechado de tipo base abstrato"),
            }
        }
    }

    /// Um tipo base abstrato não tem valores fechados (TAPL, cap. 9.1).
    #[test]
    fn abstract_base_types_have_no_closed_values() {
        for source in WELL_TYPED {
            let term = parse(source);
            if term.is_value() {
                assert!(!matches!(StlcTyping::type_of(&term).unwrap(), Type::Base(_)), "{source}");
            }
        }
    }

    /// A avaliação de um termo bem tipado fechado devolve um valor do mesmo
    /// tipo (preservação, aplicada até o fim, e não só a um passo).
    #[test]
    fn evaluation_preserves_the_type() {
        use crate::common::semantics::run;

        for source in WELL_TYPED {
            let term = parse(source);
            let before = StlcTyping::type_of(&term).unwrap();
            let trace = run::<StlcSmallStep>(term);

            assert!(trace.is_final(), "{source}");
            assert_eq!(StlcTyping::type_of(&trace.final_state).unwrap(), before, "{source}");
        }
    }

    #[test]
    fn every_law_holds_on_every_sample() {
        for t in all() {
            let violated = law_violations::<Stlc>(&t);
            assert!(violated.is_empty(), "{t}: {violated:?}");
        }
    }
}
