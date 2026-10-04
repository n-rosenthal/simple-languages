//! Lexer da linguagem `stlc` (cálculo λ simplesmente tipado, com `Bool`): uma
//! tabela para o lexer genérico.
//!
//! Tokens reconhecidos:
//!
//! 1. identificadores: uma letra ASCII seguida de letras, dígitos ou `_`
//!    (variáveis e nomes de tipos base), exceto as palavras-chave
//!    `true`, `false`, `if`, `then` e `else`;
//! 2. abstração: `λ` ou `\`;
//! 3. ponto `.`, dois-pontos `:` e seta `->`;
//! 4. parênteses `(` e `)`;
//! 5. espaço em branco, ignorado.
//!
//! As posições (`Span`) contam `char`s, não bytes: `λ` ocupa 2 bytes em
//! UTF-8, mas é uma posição.

use crate::common::frontend::{lex, LexSpec, Words};
use crate::common::{Lexer, SourceLine};

use super::token::{StlcToken, StlcTokenType};

pub use crate::common::frontend::LexError;

use StlcTokenType as T;

const SPEC: LexSpec<StlcTokenType> = LexSpec {
    keywords: &[
        ("true", T::True),
        ("false", T::False),
        ("if", T::If),
        ("then", T::Then),
        ("else", T::Else),
    ],
    symbols: &[
        ("λ", T::Lambda),
        ("\\", T::Lambda),
        (".", T::Dot),
        (":", T::Colon),
        ("->", T::Arrow),
        ("(", T::LParen),
        (")", T::RParen),
    ],
    integer: None,
    words: Words::Identifier(T::Identifier),
    line_comment: None,
};

pub struct StlcLexer;

impl StlcLexer {
    pub fn new() -> Self {
        Self
    }
}

impl Default for StlcLexer {
    fn default() -> Self {
        Self::new()
    }
}

impl Lexer for StlcLexer {
    type Token = StlcToken;
    type Error = LexError;

    fn analyze(input: &[SourceLine]) -> Result<Vec<StlcToken>, LexError> {
        lex(&SPEC, input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::Scanner;
    use crate::stlc::scanner::StlcScanner;

    fn kinds(source: &str) -> Vec<StlcTokenType> {
        let lines = StlcScanner::scan(source).unwrap();
        StlcLexer::analyze(&lines)
            .unwrap()
            .iter()
            .map(|t| t.kind)
            .collect()
    }

    #[test]
    fn lexes_untyped_abstraction_without_spaces() {
        use StlcTokenType::*;
        assert_eq!(kinds("λx.x"), vec![Lambda, Identifier, Dot, Identifier]);
    }

    #[test]
    fn lexes_typed_abstraction() {
        use StlcTokenType::*;
        assert_eq!(
            kinds("λx:Bool->Bool. (f x)"),
            vec![
                Lambda, Identifier, Colon, Identifier, Arrow, Identifier, Dot, LParen,
                Identifier, Identifier, RParen,
            ]
        );
    }

    #[test]
    fn keywords_are_not_identifiers() {
        use StlcTokenType::*;
        assert_eq!(
            kinds("if true then false else x"),
            vec![If, True, Then, False, Else, Identifier]
        );
    }

    #[test]
    fn keyword_prefixes_are_identifiers() {
        use StlcTokenType::*;
        assert_eq!(kinds("iffy trueish thenx"), vec![Identifier, Identifier, Identifier]);
    }

    #[test]
    fn accepts_backslash_as_lambda() {
        assert_eq!(kinds("\\x.x")[0], StlcTokenType::Lambda);
    }

    #[test]
    fn lone_minus_is_an_error() {
        let lines = StlcScanner::scan("Bool - Bool").unwrap();
        assert!(matches!(
            StlcLexer::analyze(&lines),
            Err(LexError::UnexpectedEndOfOperator { .. })
        ));
    }

    #[test]
    fn rejects_unknown_character() {
        let lines = StlcScanner::scan("λx#.x").unwrap();
        assert!(matches!(
            StlcLexer::analyze(&lines),
            Err(LexError::UnexpectedCharacter { character: '#', .. })
        ));
    }

    #[test]
    fn empty_input_yields_no_tokens() {
        assert!(kinds("").is_empty());
        assert!(kinds("   \t ").is_empty());
    }

    #[test]
    fn spans_are_char_offsets() {
        let lines = StlcScanner::scan("λx.x").unwrap();
        let tokens = StlcLexer::analyze(&lines).unwrap();
        let spans: Vec<_> = tokens.iter().map(|t| (t.span.start, t.span.end)).collect();
        assert_eq!(spans, vec![(0, 1), (1, 2), (2, 3), (3, 4)]);
    }

    #[test]
    fn lexemes_are_preserved() {
        let lines = StlcScanner::scan("λfoo_1:Nat->Nat. foo_1").unwrap();
        let tokens = StlcLexer::analyze(&lines).unwrap();
        assert_eq!(tokens[1].lexeme, "foo_1");
        assert_eq!(tokens[4].lexeme, "->");
    }

    #[test]
    fn multiple_lines() {
        assert_eq!(kinds("λx.x\n(y)").len(), 4 + 3);
    }

    #[test]
    fn identifier_cannot_start_with_underscore() {
        let lines = StlcScanner::scan("_x").unwrap();
        assert!(StlcLexer::analyze(&lines).is_err());
    }
}
