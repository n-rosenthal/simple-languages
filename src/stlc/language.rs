//! `lambda` como instância de [`Language`].

use std::fmt;

use crate::common::language::{Example, Language, Syntax};
use crate::common::{Lexer, Parser, Scanner};

use super::big_step::StlcBigStep;
use super::compile::StlcCompiler;
use super::lexer::{StlcLexer, LexError};
use super::parser::{StlcParser, ParseError};
use super::scanner::{StlcScanner, ScanError};
use super::small_step::StlcSmallStep;
use super::terms::Term;
use super::typing::StlcTyping;
use super::types::Type;

/// O erro de qualquer estágio do front-end.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SyntaxError {
    Scan(ScanError),
    Lex(LexError),
    Parse(ParseError),
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Scan(e) => write!(f, "{e}"),
            Self::Lex(e) => write!(f, "{e}"),
            Self::Parse(e) => write!(f, "{e}"),
        }
    }
}

impl std::error::Error for SyntaxError {}

impl From<ScanError> for SyntaxError {
    fn from(e: ScanError) -> Self {
        Self::Scan(e)
    }
}

impl From<LexError> for SyntaxError {
    fn from(e: LexError) -> Self {
        Self::Lex(e)
    }
}

impl From<ParseError> for SyntaxError {
    fn from(e: ParseError) -> Self {
        Self::Parse(e)
    }
}

/// O cálculo λ simplesmente tipado com booleanos (TAPL, caps. 9 e 10),
/// call-by-value.
pub struct Stlc;

const EXAMPLES: &[Example] = &[
    Example { title: "Identidade", source: "λx:A. x" },
    Example { title: "Negação", source: "λb:Bool. if b then false else true" },
    Example { title: "Aplicando a negação", source: "(λb:Bool. if b then false else true) true" },
    Example { title: "Só o ramo escolhido executa", source: "if true then false else (true true)" },
    Example { title: "Aplicação", source: "(λf:A->A. λx:A. f x) (λy:A. y)" },
    Example {
        title: "Aplicar duas vezes",
        source: "(λf:Bool->Bool. λb:Bool. f (f b)) (λb:Bool. if b then false else true) true",
    },
    Example {
        title: "Definição (use depois como `twice not true`)",
        source: "twice = λf:Bool->Bool. λb:Bool. f (f b)",
    },
    Example { title: "Substituição sem captura (termo aberto)", source: "(λx:A. λy:A. x) (λz:A. y)" },
    Example { title: "Erro de tipo", source: "(λx:Bool. x) (λy:Bool. y)" },
    Example { title: "Condição que não é Bool", source: "if (λx:Bool. x) then true else false" },
    Example { title: "Aplicar um booleano (trava)", source: "true false" },
    Example { title: "Variável livre (trava)", source: "x (λy:A. y)" },
    Example { title: "ω: diverge", source: "(λx:A. x x) (λx:A. x x)" },
];

const SYNTAX: &[Syntax] = &[
    Syntax {
        title: "termos",
        meta: "t",
        productions: &[
            r"x",
            r"\mathsf{true}",
            r"\mathsf{false}",
            r"\mathsf{if}\ t_1\ \mathsf{then}\ t_2\ \mathsf{else}\ t_3",
            r"\lambda x{:}T.\, t",
            r"t_1\ t_2",
        ],
    },
    Syntax {
        title: "valores",
        meta: "v",
        productions: &[r"\mathsf{true}", r"\mathsf{false}", r"\lambda x{:}T.\, t"],
    },
    Syntax {
        title: "tipos",
        meta: "T",
        productions: &[r"\mathsf{Bool}", r"A", r"T_1 \to T_2"],
    },
    Syntax { title: "contextos", meta: r"\Gamma", productions: &[r"\emptyset", r"\Gamma,\, x{:}T"] },
];

impl Language for Stlc {
    const NAME: &'static str = "stlc";
    const DESCRIPTION: &'static str =
        "simply typed lambda calculus with Bool, call-by-value (TAPL chs. 9-10); type λ or a backslash";
    const SUPPORTS_DEFINITIONS: bool = true;

    type Term = Term;
    type Type = Type;
    type Value = Term;

    type SyntaxError = SyntaxError;

    type Typing = StlcTyping;
    type Small = StlcSmallStep;
    type Big = StlcBigStep;
    type Compiler = StlcCompiler;

    fn parse(source: &str) -> Result<Term, SyntaxError> {
        let lines = StlcScanner::scan(source)?;
        let tokens = StlcLexer::analyze(&lines)?;
        Ok(StlcParser::parse(&tokens)?)
    }

    fn examples() -> &'static [Example] {
        EXAMPLES
    }

    fn syntax() -> &'static [Syntax] {
        SYNTAX
    }

    fn substitute(term: &Term, name: &str, value: &Term) -> Term {
        term.substitute(name, value)
    }
}
