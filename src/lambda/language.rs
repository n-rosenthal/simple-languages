//! `lambda` como instância de [`Language`], uma por estratégia de avaliação.
//!
//! O que as estratégias têm em comum (sintaxe, parser, substituição,
//! exemplos) está em [`Lambda`]. Cada estratégia ([`Strategy`]) só escolhe a
//! sua semântica estrutural e diz quais das outras semânticas ela tem:
//! só o call-by-value tem semântica natural e compilação para a máquina.

use std::marker::PhantomData;

use crate::common::frontend::{parse_source, FrontendError};
use crate::common::language::{Example, Language, Syntax};
use crate::common::machine_language::{Compile, NoCompile};
use crate::common::semantics::{BigStep, NoBigStep, NoTyping, Step, Untyped};

use super::big_step::LambdaBigStep;
use super::compile::LambdaCompiler;
use super::lexer::{LambdaLexer, LexError};
use super::parser::{LambdaParser, ParseError};
use super::small_step::{CallByName, CallByValue, NormalOrder};
use super::terms::Term;
use super::{LambdaScanner, ScanError};

/// O erro de qualquer estágio do front-end.
pub type SyntaxError = FrontendError<ScanError, LexError, ParseError>;

/// Uma estratégia de avaliação: a semântica estrutural e o que mais existe.
pub trait Strategy: Step<State = Term> + 'static {
    /// O nome da linguagem no CLI.
    const NAME: &'static str;
    const DESCRIPTION: &'static str;

    type Big: BigStep<Term = Term, Value = Term>;
    type Compiler: Compile<Source = Term, Value = Term>;

    /// As categorias sintáticas específicas da estratégia (valores, formas normais).
    fn results() -> &'static [Syntax];
}

impl Strategy for CallByValue {
    const NAME: &'static str = "lambda";
    const DESCRIPTION: &'static str =
        "untyped lambda calculus, call-by-value (TAPL chs. 5-7); type λ or a backslash";

    type Big = LambdaBigStep;
    type Compiler = LambdaCompiler;

    fn results() -> &'static [Syntax] {
        &[Syntax { title: "valores", meta: "v", productions: &[r"\lambda x.\, t"] }]
    }
}

impl Strategy for CallByName {
    const NAME: &'static str = "lambda-name";
    const DESCRIPTION: &'static str =
        "untyped lambda calculus, call-by-name (TAPL ch. 5.3): arguments are not evaluated before the call";

    type Big = NoBigStep<Term, Term>;
    type Compiler = NoCompile<Term, Term>;

    fn results() -> &'static [Syntax] {
        &[Syntax { title: "valores", meta: "v", productions: &[r"\lambda x.\, t"] }]
    }
}

impl Strategy for NormalOrder {
    const NAME: &'static str = "lambda-normal";
    const DESCRIPTION: &'static str =
        "untyped lambda calculus, normal order (TAPL ch. 5.3): leftmost-outermost, also under λ; the result is a normal form";

    type Big = NoBigStep<Term, Term>;
    type Compiler = NoCompile<Term, Term>;

    fn results() -> &'static [Syntax] {
        &[
            Syntax {
                title: "formas normais",
                meta: r"\mathit{nf}",
                productions: &[r"\lambda x.\, \mathit{nf}", r"n"],
            },
            Syntax {
                title: "formas neutras",
                meta: "n",
                productions: &[r"x", r"n\ \mathit{nf}"],
            },
        ]
    }
}

/// O cálculo λ sem tipos, com a estratégia `S`.
pub struct Lambda<S>(PhantomData<fn() -> S>);

pub type LambdaValue = Lambda<CallByValue>;
pub type LambdaName = Lambda<CallByName>;
pub type LambdaNormal = Lambda<NormalOrder>;

const EXAMPLES: &[Example] = &[
    Example { title: "Identidade", source: "λx. x" },
    Example { title: "Aplicação", source: "(λx. x) (λy. y)" },
    Example {
        title: "Booleanos de Church (programa: instruções separadas por `;;`)",
        source: "tru = λt. λf. t ;; fls = λt. λf. f ;; and = λb. λc. b c fls ;; and tru fls",
    },
    Example {
        title: "Pares",
        source: "tru = λt. λf. t ;; pair = λf. λs. λb. b f s ;; fst = λp. p tru ;; fst (pair tru (λx. x))",
    },
    Example {
        title: "Numerais de Church: 1 + 1",
        source: "zero = λs. λz. z ;; succ = λn. λs. λz. s (n s z) ;; plus = λm. λn. λs. λz. m s (n s z) ;; one = succ zero ;; plus one one",
    },
    Example {
        title: "Substituição sem captura (só a ordem normal reduz sob λ)",
        source: "λy. (λx. λy. x) y",
    },
    Example {
        title: "Argumento que diverge (só o call-by-value diverge)",
        source: "(λx. λy. y) ((λx. x x) (λx. x x))",
    },
    Example { title: "ω: diverge em todas", source: "(λx. x x) (λx. x x)" },
    Example { title: "Variável livre (trava)", source: "x (λy. y)" },
];

const TERMS: Syntax = Syntax {
    title: "termos",
    meta: "t",
    productions: &[r"x", r"\lambda x.\, t", r"t_1\ t_2"],
};

impl<S: Strategy> Language for Lambda<S> {
    const NAME: &'static str = S::NAME;
    const DESCRIPTION: &'static str = S::DESCRIPTION;
    const SUPPORTS_DEFINITIONS: bool = true;

    type Term = Term;
    type Type = Untyped;
    type Value = Term;

    type SyntaxError = SyntaxError;

    type Typing = NoTyping<Term>;
    type Small = S;
    type Big = S::Big;
    type Compiler = S::Compiler;

    fn parse(source: &str) -> Result<Term, SyntaxError> {
        parse_source::<LambdaScanner, LambdaLexer, LambdaParser>(source)
    }

    fn examples() -> &'static [Example] {
        EXAMPLES
    }

    fn syntax() -> &'static [Syntax] {
        // `Language::syntax` devolve uma fatia estática: monta-se uma vez por estratégia.
        // As categorias são poucas, então vazamos a fatia (uma por tipo, uma vez).
        use std::collections::HashMap;
        use std::sync::{Mutex, OnceLock};

        static CACHE: OnceLock<Mutex<HashMap<&'static str, &'static [Syntax]>>> = OnceLock::new();

        let mut cache = CACHE.get_or_init(Default::default).lock().expect("syntax cache");
        cache.entry(S::NAME).or_insert_with(|| {
            let mut all = vec![TERMS];
            all.extend_from_slice(S::results());
            Box::leak(all.into_boxed_slice())
        })
    }

    fn substitute(term: &Term, name: &str, value: &Term) -> Term {
        term.substitute(name, value)
    }
}
