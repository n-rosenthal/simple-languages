//! `arith` como instância de [`Language`].

use crate::common::frontend::{parse_source, FrontendError};
use crate::common::language::{Example, Language, Syntax};
use crate::common::source::ScanError;

use super::big_step::ArithBigStep;
use super::compile::ArithCompiler;
use super::lexer::{ArithLexer, LexError};
use super::parser::{ArithParser, ParseError};
use super::small_step::ArithSmallStep;
use super::terms::Term;
use super::typing::ArithTyping;
use super::types::Type;
use super::values::Value;
use super::ArithScanner;

/// O erro de qualquer estágio do front-end.
pub type SyntaxError = FrontendError<ScanError, LexError, ParseError>;

/// Expressões aritméticas e booleanas (TAPL, caps. 3 e 8).
pub struct Arith;

const EXAMPLES: &[Example] = &[
    Example { title: "Soma e produto", source: "(2 * 3) + (4 - 5)" },
    Example { title: "Condicional", source: "if 1 < 2 then 10 else 20" },
    Example { title: "Igualdade de booleanos", source: "true == (1 < 2)" },
    Example { title: "Lógica", source: "(1 < 2) && (2 < 1) || true" },
    Example { title: "Só o ramo escolhido executa", source: "if true then 1 else (true + 1)" },
    Example { title: "Mal tipado e travado", source: "true + 1" },
    Example { title: "Condição que não é booleana", source: "if 1 then 2 else 3" },
];

const SYNTAX: &[Syntax] = &[
    Syntax {
        title: "termos",
        meta: "t",
        productions: &[
            r"n",
            r"\mathsf{true}",
            r"\mathsf{false}",
            r"t_1 + t_2",
            r"t_1 - t_2",
            r"t_1 \times t_2",
            r"t_1 < t_2",
            r"t_1 = t_2",
            r"t_1 \wedge t_2",
            r"t_1 \vee t_2",
            r"\mathsf{if}\ t_1\ \mathsf{then}\ t_2\ \mathsf{else}\ t_3",
        ],
    },
    Syntax { title: "valores", meta: "v", productions: &[r"n", r"\mathsf{true}", r"\mathsf{false}"] },
    Syntax { title: "tipos", meta: "T", productions: &[r"\mathsf{Integer}", r"\mathsf{Boolean}"] },
];

impl Language for Arith {
    const NAME: &'static str = "arith-extensions";
    const DESCRIPTION: &'static str =
        "integers, booleans, + - * < == && ||, if/then/else (TAPL chs. 3 and 8)";

    type Term = Term;
    type Type = Type;
    type Value = Value;

    type SyntaxError = SyntaxError;

    type Typing = ArithTyping;
    type Small = ArithSmallStep;
    type Big = ArithBigStep;
    type Compiler = ArithCompiler;

    fn parse(source: &str) -> Result<Term, SyntaxError> {
        parse_source::<ArithScanner, ArithLexer, ArithParser>(source)
    }

    fn examples() -> &'static [Example] {
        EXAMPLES
    }

    fn syntax() -> &'static [Syntax] {
        SYNTAX
    }
}
