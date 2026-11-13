//! `arith-extensions` como instância de [`Language`].

/// o scanner de código-fonte em linguagem `arith-extensions`
use super::ArithScanner;

/// parse_source: source-code (input) -> 
use crate::common::frontend::{parse_source, FrontendError};

/// erro de scanning do código-fonte, da análise léxica ou do parser
use crate::common::source::ScanError;
use super::lexer::{ArithLexer, LexError};
use super::parser::{ArithParser, ParseError};

/// O erro de qualquer estágio do front-end.
pub type SyntaxError = FrontendError<ScanError, LexError, ParseError>;

/// expressões LaTeX para renderizar exemplos de termos da linguagem
/// e suas regras de avaliação e de tipagem; `Language` é uma
/// grande struct que agrupa as componentes necessárias à definição
/// de uma linguagem no sistema atual
use crate::common::language::{Example, Syntax, Language};


/// termos, tipos e valores da linguagem `arith-extensions`
use super::terms::Term;
use super::types::Type;
use super::values::Value;

/// o sistema de tipos simpĺes de `arith-extensions`
use super::typing::ArithTyping;

/// as semânticas operacionais natural (big-step) e estrutural (small-step)
/// de `arith-extensions`
use super::big_step::ArithBigStep;
use super::small_step::ArithSmallStep;

/// compilador de `arith-extensions` para linguagem de máquina abstrata
use super::compile::ArithCompiler;

/// Expressões aritméticas e booleanas (TAPL, caps. 3 e 8).
pub struct ArithExtensions;


/// Exemplos de termos da linguagem ~arith-extensions~
const EXAMPLES: &[Example] = &[
    // extensions
    // Peano arithmetic for natural numbers
    Example { title: "Zero", source: "0" },
    Example { title: "Sucessor", source: "succ 0" },
    Example { title: "Predecessor", source: "pred (succ 0)" },
    Example { title: "IsZero", source: "iszero (pred (succ 0))" },
    
    // Integers
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

/// estratégia de avaliação 




/// implementação do trait ~Language~ para ~arith-extensions~
impl Language for ArithExtensions {
    const NAME: &'static str = "arith-extensions";
    const DESCRIPTION: &'static str =
        "peano naturals, integers, booleans, + - * < == && || div mod, if/then/else (TAPL chs. 3 and 8)";

    /// termos, tipos, valores
    type Term = Term;
    type Type = Type;
    type Value = Value;

    /// erros de sintaxe (scanning, lexing, parsing)
    type SyntaxError = SyntaxError;

    /// sistema de tipos, avaliações e compilação para ling. máquina
    type Typing = ArithTyping;
    type Small = ArithSmallStep;
    type Big = ArithBigStep;
    type Compiler = ArithCompiler;

    /// source-code -> term || error
    fn parse(source: &str) -> Result<Term, SyntaxError> {
        parse_source::<ArithScanner, ArithLexer, ArithParser>(source)
    }

    /// renderização web dos exemplos
    fn examples() -> &'static [Example] {
        EXAMPLES
    }

    /// renderização web das regras sintáticas
    fn syntax() -> &'static [Syntax] {
        SYNTAX
    }
}
