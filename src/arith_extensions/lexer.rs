//! Lexer de `arith-extensions`: uma tabela para o lexer genérico.
//!
//! Tokens: inteiros, `true`/`false`, `if`/`then`/`else`, `+ - * <`, `==`,
//! `&&`, `||`, `/`, `%` e parênteses. Qualquer outra palavra é um erro: `arith-extensions` não tem
//! variáveis.
use crate::common::frontend::{
    ascii_word_continue,
    ascii_word_start,
    LexSpec,
    Words,
    lex
};
use crate::common::{Lexer, SourceLine};

use super::token::{ArithToken, ArithTokenType};

pub use crate::common::frontend::LexError;

use ArithTokenType as T;

const SPEC: LexSpec<ArithTokenType> = LexSpec {
    keywords: &[
        ("true", T::Boolean),
        ("false", T::Boolean),
        ("if", T::If),
        ("then", T::Then),
        ("else", T::Else),
        ("succ", T::Succ),
        ("pred", T::Pred),
        ("iszero", T::IsZero),
    ],

    symbols: &[
        ("+", T::Plus),
        ("-", T::Minus),
        ("*", T::Star),
        ("/", T::Slash),
        ("%", T::Percent),
        ("<", T::LessThan),
        ("==", T::Equal),
        ("&&", T::And),
        ("||", T::Or),
        ("(", T::LeftParen),
        (")", T::RightParen),
    ],

    integer: Some(T::Integer),
    zero: Some(T::Zero),

    words: Words::Reject("arith-extensions has no variables"),

    line_comment: None,
    block_comment: None,

    word_start: ascii_word_start,
    word_continue: ascii_word_continue,
};

pub struct ArithLexer;

impl ArithLexer {
    pub fn new() -> Self {
        Self
    }
}

impl Default for ArithLexer {
    fn default() -> Self {
        Self::new()
    }
}

impl Lexer for ArithLexer {
    type Token = ArithToken;
    type Error = LexError;

    fn analyze(input: &[SourceLine]) -> Result<Vec<ArithToken>, LexError> {
        lex(&SPEC, input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arith::ArithScanner;
    use crate::common::Scanner;
    use ArithTokenType::*;

    fn kinds(source: &str) -> Vec<ArithTokenType> {
        let lines = ArithScanner::scan(source).unwrap();
        ArithLexer::analyze(&lines).unwrap().iter().map(|t| t.kind).collect()
    }

    fn error(source: &str) -> LexError {
        let lines = ArithScanner::scan(source).unwrap();
        ArithLexer::analyze(&lines).unwrap_err()
    }

    #[test]
    fn scans_lines() {
        let lines = ArithScanner::scan("1 + 2\n3 * 4").unwrap();
        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0].number, 1);
        assert_eq!(lines[1].number, 2);
    }

    #[test]
    fn lexes_integer_expression() {
        assert_eq!(kinds("1 + 2 * 3"), vec![Integer, Plus, Integer, Star, Integer]);
    }

    #[test]
    fn lexes_boolean_expression() {
        assert_eq!(kinds("true && false"), vec![Boolean, And, Boolean]);
        assert_eq!(kinds("true || false"), vec![Boolean, Or, Boolean]);
    }

    #[test]
    fn lexes_conditional() {
        assert_eq!(
            kinds("if 1 < 2 then 3 else 4"),
            vec![If, Integer, LessThan, Integer, Then, Integer, Else, Integer]
        );
    }

    #[test]
    fn lexes_equality_and_parentheses() {
        assert_eq!(
            kinds("(1 - 2) == 3"),
            vec![LeftParen, Integer, Minus, Integer, RightParen, Equal, Integer]
        );
    }

    #[test]
    fn numbers_followed_by_letters_split() {
        // `12ab`: o inteiro 12 e depois a palavra desconhecida `ab`
        assert!(matches!(error("12ab"), LexError::UnknownWord { .. }));
    }

    #[test]
    fn unknown_words_are_errors() {
        use crate::common::Span;

        assert!(matches!(
            error("1 + foo"),
            LexError::UnknownWord { ref word, span, .. }
                if word == "foo" && span == Span::new(4, 7)
        ));
    }

    #[test]
    fn half_operators_are_errors() {
        for source in ["a = b", "1 = 2", "1 & 2", "1 | 2", "1 ="] {
            // `a` falha antes, as demais no operador
            let e = error(source);
            assert!(
                matches!(e, LexError::UnknownWord { .. } | LexError::UnexpectedEndOfOperator { .. }),
                "{source}: {e:?}"
            );
        }
        assert!(matches!(error("1 = 2"), LexError::UnexpectedEndOfOperator { .. }));
    }

    #[test]
    fn unknown_characters_are_errors() {
        assert!(matches!(
            error("1 # 2"),
            LexError::UnexpectedCharacter { character: '#', .. }
        ));
    }

    #[test]
    fn spans_and_lexemes() {
        let lines = ArithScanner::scan("12 == 3").unwrap();
        let tokens = ArithLexer::analyze(&lines).unwrap();
        assert_eq!(tokens[0].lexeme, "12");
        assert_eq!((tokens[1].span.start, tokens[1].span.end), (3, 5));
    }
}
