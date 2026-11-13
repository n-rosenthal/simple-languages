//! `nameless_terms` como instância de [`Language`].
//!
//! O cálculo lambda puro utiliza índices de De Bruijn em vez de nomes
//! de variáveis. A sintaxe de entrada continua permitindo abstrações
//! nomeadas, que o parser converte para a representação nameless.
//!
//! As estratégias de avaliação são call-by-value, call-by-name e ordem
//! normal. Apenas call-by-value possui semântica natural e compilação
//! para a máquina compartilhada.

use std::marker::PhantomData;

use crate::common::frontend::{parse_source, FrontendError, LexError};
use crate::common::language::{Example, Language, Syntax};
use crate::common::machine_language::{Compile, NoCompile};
use crate::common::semantics::{BigStep, NoBigStep, NoTyping, Step, Untyped};
use crate::common::semantics::substitution::{NoDefinitions};

use super::big_step::NamelessTermsBigStep;
use super::compile::NamelessTermsCompiler;
use super::parser::{NamelessTermsParser, ParseError};
use super::small_step::{CallByName, CallByValue, NormalOrder};
use super::terms::Term;
use super::{NamelessTermsScanner, ScanError};

/// Erro de qualquer estágio do front-end.
pub type SyntaxError =
    FrontendError<ScanError, LexError, ParseError>;

/// Estratégia de avaliação: semântica estrutural e capacidades adicionais.
pub trait Strategy: Step<State = Term> + 'static {
    /// Nome da linguagem no CLI.
    const NAME: &'static str;

    /// Descrição da linguagem.
    const DESCRIPTION: &'static str;

    type Big: BigStep<Term = Term, Value = Term>;
    type Compiler: Compile<Source = Term, Value = Term>;

    /// Categorias sintáticas específicas da estratégia.
    fn results() -> &'static [Syntax];
}

impl Strategy for CallByValue {
    const NAME: &'static str = "nameless";
    const DESCRIPTION: &'static str =
        "untyped lambda calculus with De Bruijn indices, call-by-value";

    type Big = NamelessTermsBigStep;
    type Compiler = NamelessTermsCompiler;

    fn results() -> &'static [Syntax] {
        &[Syntax {
            title: "valores",
            meta: "v",
            productions: &[r"\lambda.\, t"],
        }]
    }
}

impl Strategy for CallByName {
    const NAME: &'static str = "nameless-name";
    const DESCRIPTION: &'static str =
        "untyped lambda calculus with De Bruijn indices, call-by-name";

    type Big = NoBigStep<Term, Term>;
    type Compiler = NoCompile<Term, Term>;

    fn results() -> &'static [Syntax] {
        &[Syntax {
            title: "valores",
            meta: "v",
            productions: &[r"\lambda.\, t"],
        }]
    }
}

impl Strategy for NormalOrder {
    const NAME: &'static str = "nameless-normal";
    const DESCRIPTION: &'static str =
        "untyped lambda calculus with De Bruijn indices, normal order";
    
    type Big = NoBigStep<Term, Term>;
    type Compiler = NoCompile<Term, Term>;

    fn results() -> &'static [Syntax] {
        &[
            Syntax {
                title: "formas normais",
                meta: r"\mathit{nf}",
                productions: &[
                    r"\lambda.\, \mathit{nf}",
                    r"n",
                ],
            },
            Syntax {
                title: "formas neutras",
                meta: "n",
                productions: &[
                    r"i",
                    r"n\ \mathit{nf}",
                ],
            },
        ]
    }
}

/// O cálculo lambda nameless com a estratégia `S`.
pub struct NamelessTerms<S>(PhantomData<fn() -> S>);

pub type NamelessTermsValue = NamelessTerms<CallByValue>;
pub type NamelessTermsName = NamelessTerms<CallByName>;
pub type NamelessTermsNormal = NamelessTerms<NormalOrder>;

const EXAMPLES: &[Example] = &[
    Example {
        title: "Identidade",
        source: "λx. x",
    },
    Example {
        title: "Aplicação",
        source: "(λx. x) (λy. y)",
    },
    Example {
        title: "Aplicação de abstrações aninhadas",
        source: "(λx. λy. x) (λz. z)",
    },
    Example {
        title: "Substituição sob abstração",
        source: "(λx. λy. x) (λz. z)",
    },
    Example {
        title: "Argumento que diverge",
        source: "(λx. λy. y) ((λx. x x) (λx. x x))",
    },
    Example {
        title: "ω",
        source: "(λx. x x) (λx. x x)",
    },
];

const TERMS: Syntax = Syntax {
    title: "termos",
    meta: "t",
    productions: &[
        r"i",
        r"\lambda.\, t",
        r"t_1\ t_2",
    ],
};

impl<S: Strategy> Language for NamelessTerms<S> {
    const NAME: &'static str = S::NAME;
    const DESCRIPTION: &'static str = S::DESCRIPTION;

    const SUPPORTS_DEFINITIONS: bool = false;

    type Term = Term;
    type Type = Untyped;
    type Value = Term;

    type SyntaxError = SyntaxError;

    type Typing = NoTyping<Term>;
    type Small = S;
    type Big = S::Big;
    type Compiler = S::Compiler;

    type Definitions = NoDefinitions;

    fn parse(source: &str) -> Result<Term, SyntaxError> {
        parse_source::<
            NamelessTermsScanner,
            NamelessTermsLexer,
            NamelessTermsParser,
        >(source)
    }

    fn examples() -> &'static [Example] {
        EXAMPLES
    }

    fn syntax() -> &'static [Syntax] {
        use std::collections::HashMap;
        use std::sync::{Mutex, OnceLock};

        static CACHE: OnceLock<
            Mutex<HashMap<&'static str, &'static [Syntax]>>,
        > = OnceLock::new();

        let mut cache = CACHE
            .get_or_init(|| Mutex::new(HashMap::new()))
            .lock()
            .expect("syntax cache");

        *cache.entry(S::NAME).or_insert_with(|| {
            let mut all = vec![TERMS];
            all.extend_from_slice(S::results());
            Box::leak(all.into_boxed_slice())
        })
    }

    /// A representação nameless não precisa de substituição por nome.
    ///
    /// A substituição semântica é realizada por índices de De Bruijn,
    /// usando `substitute_top` no processo de β-redução.
    fn substitute(_term: &Term, _name: &str, _value: &Term) -> Term {
        panic!(
            "substitution by variable name is not supported by nameless_terms"
        )
    }
}
