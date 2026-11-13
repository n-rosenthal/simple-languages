//! `typed-arith` como instância de [`Language`].

/// parsing: source-code -> term 
use crate::common::frontend::{parse_source, FrontendError};

/// 
use crate::common::language::{Example, Syntax, Language};

use super::big_step::TypedArithBigStep;
use super::compile::TypedArithCompiler;
use super::lexer::{LexError, TypedArithLexer};
use super::parser::{ParseError, TypedArithParser};
use super::small_step::TypedArithSmallStep;
use super::terms::Term;
use super::typing::TypedArithTyping;
use super::types::Type;
use super::{ScanError, TypedArithScanner};

/// O erro de qualquer estágio do front-end.
pub type SyntaxError = FrontendError<ScanError, LexError, ParseError>;

/// Booleanos e naturais com tipos (TAPL, caps. 3 e 8).
pub struct TypedArith;

const EXAMPLES: &[Example] = &[
    Example { title: "Um numeral", source: "3" },
    Example { title: "Sucessor e predecessor", source: "pred (succ (succ 0))" },
    Example { title: "Predecessor de zero", source: "pred 0" },
    Example { title: "Teste de zero", source: "iszero (pred (succ 0))" },
    Example { title: "Condicional", source: "if iszero 0 then succ 0 else 0" },
    Example { title: "Só o ramo escolhido executa", source: "if true then 1 else succ true" },
    Example { title: "Mal tipado e travado", source: "succ true" },
    Example { title: "Condição que não é Bool", source: "if 0 then true else false" },
    Example { title: "Ramos de tipos diferentes", source: "if true then 0 else false" },
    Example { title: "Mal tipado, mas ainda assim avalia", source: "if true then 0 else iszero 0" },
    Example { title: "Travado dentro de um valor", source: "succ (pred true)" },
];

const SYNTAX: &[Syntax] = &[
    Syntax {
        title: "termos",
        meta: "t",
        productions: &[
            r"\mathsf{true}",
            r"\mathsf{false}",
            r"\mathsf{if}\ t_1\ \mathsf{then}\ t_2\ \mathsf{else}\ t_3",
            r"0",
            r"\mathsf{succ}\ t_1",
            r"\mathsf{pred}\ t_1",
            r"\mathsf{iszero}\ t_1",
        ],
    },
    Syntax {
        title: "valores",
        meta: "v",
        productions: &[r"\mathsf{true}", r"\mathsf{false}", r"nv"],
    },
    Syntax {
        title: "valores numéricos",
        meta: "nv",
        productions: &[r"0", r"\mathsf{succ}\ nv"],
    },
    Syntax { title: "tipos", meta: "T", productions: &[r"\mathsf{Bool}", r"\mathsf{Nat}"] },
];

impl Language for TypedArith {
    const NAME: &'static str = "typed-arith";
    const DESCRIPTION: &'static str =
        "typed booleans and naturals: true, false, if, 0, succ, pred, iszero (TAPL chs. 3 and 8); a numeral n is sugar for succ^n 0";

    type Term = Term;
    type Type = Type;
    type Value = Term;

    type SyntaxError = SyntaxError;

    type Typing = TypedArithTyping;
    type Small = TypedArithSmallStep;
    type Big = TypedArithBigStep;
    type Compiler = TypedArithCompiler;

    fn parse(source: &str) -> Result<Term, SyntaxError> {
        parse_source::<TypedArithScanner, TypedArithLexer, TypedArithParser>(source)
    }

    fn examples() -> &'static [Example] {
        EXAMPLES
    }

    fn syntax() -> &'static [Syntax] {
        SYNTAX
    }
}
