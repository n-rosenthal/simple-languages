//! S/src/Scanner/Parser.rs
//!
//! Análise sintática: converte a sequência de tokens em um `Term`.
//!
//! author:  n-rosenthal
//! date:    2026-08-17
//! version: 0.1

use crate::Scanner::Lexer::{Token, TokenKind};
use crate::Scanner::Types::{BinaryOperator, Term};

/// Erros possíveis durante a análise sintática.
#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedToken { found: TokenKind, line: usize, column: usize },
    UnexpectedEof,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken { found, line, column } => {
                write!(f, "token inesperado {found:?} em {line}:{column}")
            }
            ParseError::UnexpectedEof => write!(f, "fim de entrada inesperado"),
        }
    }
}

impl std::error::Error for ParseError {}

/// Consome uma sequência de `Token` e produz um `Term`.
pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, position: 0 }
    }

    /// Analisa a entrada completa e retorna o `Term` resultante.
    ///
    /// Retorna erro se, após consumir um termo válido, ainda sobrarem
    /// tokens não consumidos (exceto `Eof`).
    pub fn parse(mut self) -> Result<Term, ParseError> {
        let term = self.parse_equality()?;

        match self.peek_kind() {
            TokenKind::Eof => Ok(term),
            found => Err(ParseError::UnexpectedToken {
                found: found.clone(),
                line: self.peek().line,
                column: self.peek().column,
            }),
        }
    }

    // equality := additive ( "==" additive )*
    fn parse_equality(&mut self) -> Result<Term, ParseError> {
        let mut left = self.parse_additive()?;

        while matches!(self.peek_kind(), TokenKind::EqEq) {
            self.advance();
            let right = self.parse_additive()?;
            left = Term::BinaryOperation {
                operator: BinaryOperator::Eq,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    // additive := primary ( ("+" | "-") primary )*
    fn parse_additive(&mut self) -> Result<Term, ParseError> {
        let mut left = self.parse_primary()?;

        loop {
            let operator = match self.peek_kind() {
                TokenKind::Plus  => BinaryOperator::Add,
                TokenKind::Minus => BinaryOperator::Sub,
                _ => break,
            };

            self.advance();
            let right = self.parse_primary()?;

            left = Term::BinaryOperation {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    // primary := INTEGER | BOOLEAN | "(" equality ")"
    fn parse_primary(&mut self) -> Result<Term, ParseError> {
        let token = self.peek().clone();

        match token.kind {
            TokenKind::LiteralInteger(n) => {
                self.advance();
                Ok(Term::LiteralInteger(n))
            }
            TokenKind::LiteralBoolean(b) => {
                self.advance();
                Ok(Term::LiteralBoolean(b))
            }
            TokenKind::LParen => {
                self.advance();
                let inner = self.parse_equality()?;
                self.expect(TokenKind::RParen)?;
                Ok(inner)
            }
            TokenKind::Eof => Err(ParseError::UnexpectedEof),
            found => Err(ParseError::UnexpectedToken {
                found,
                line: token.line,
                column: token.column,
            }),
        }
    }

    fn expect(&mut self, expected: TokenKind) -> Result<(), ParseError> {
        if self.peek_kind() == &expected {
            self.advance();
            Ok(())
        } else {
            let token = self.peek();
            Err(ParseError::UnexpectedToken {
                found: token.kind.clone(),
                line: token.line,
                column: token.column,
            })
        }
    }

    fn peek(&self) -> &Token {
        // seguro: `tokenize()` sempre termina com `Eof`, então nunca
        // avançamos além do último token.
        self.tokens.get(self.position).unwrap_or_else(|| {
            self.tokens.last().expect("stream de tokens não deveria estar vazia")
        })
    }

    fn peek_kind(&self) -> &TokenKind {
        &self.peek().kind
    }

    fn advance(&mut self) -> &Token {
        let token = &self.tokens[self.position.min(self.tokens.len() - 1)];
        if self.position < self.tokens.len() - 1 {
            self.position += 1;
        }
        token
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Scanner::Lexer::Lexer;
    use crate::Scanner::Types::SourceFile;
    use std::path::PathBuf;

    fn parse_str(content: &str) -> Result<Term, ParseError> {
        let src = SourceFile {
            path: PathBuf::from("<memória>"),
            content: content.to_string(),
            lines: content.lines().map(str::to_string).collect(),
        };
        let tokens = Lexer::new(&src).tokenize().expect("deveria tokenizar");
        Parser::new(tokens).parse()
    }

    #[test]
    fn parseia_soma_simples() {
        let term = parse_str("1 + 2").expect("deveria parsear");
        assert_eq!(
            term,
            Term::BinaryOperation {
                operator: BinaryOperator::Add,
                left: Box::new(Term::LiteralInteger(1)),
                right: Box::new(Term::LiteralInteger(2)),
            }
        );
    }

    #[test]
    fn soma_e_subtracao_sao_associativas_a_esquerda() {
        // 1 + 2 - 3  =>  (1 + 2) - 3
        let term = parse_str("1 + 2 - 3").expect("deveria parsear");
        assert_eq!(
            term,
            Term::BinaryOperation {
                operator: BinaryOperator::Sub,
                left: Box::new(Term::BinaryOperation {
                    operator: BinaryOperator::Add,
                    left: Box::new(Term::LiteralInteger(1)),
                    right: Box::new(Term::LiteralInteger(2)),
                }),
                right: Box::new(Term::LiteralInteger(3)),
            }
        );
    }

    #[test]
    fn igualdade_tem_precedencia_menor_que_soma() {
        // 1 + 2 == 3  =>  (1 + 2) == 3
        let term = parse_str("1 + 2 == 3").expect("deveria parsear");
        assert_eq!(
            term,
            Term::BinaryOperation {
                operator: BinaryOperator::Eq,
                left: Box::new(Term::BinaryOperation {
                    operator: BinaryOperator::Add,
                    left: Box::new(Term::LiteralInteger(1)),
                    right: Box::new(Term::LiteralInteger(2)),
                }),
                right: Box::new(Term::LiteralInteger(3)),
            }
        );
    }

    #[test]
    fn parenteses_alteram_precedencia() {
        // 1 - (2 - 3)  !=  (1 - 2) - 3
        let term = parse_str("1 - (2 - 3)").expect("deveria parsear");
        assert_eq!(
            term,
            Term::BinaryOperation {
                operator: BinaryOperator::Sub,
                left: Box::new(Term::LiteralInteger(1)),
                right: Box::new(Term::BinaryOperation {
                    operator: BinaryOperator::Sub,
                    left: Box::new(Term::LiteralInteger(2)),
                    right: Box::new(Term::LiteralInteger(3)),
                }),
            }
        );
    }

    #[test]
    fn booleanos_e_comparacao() {
        let term = parse_str("true == false").expect("deveria parsear");
        assert_eq!(
            term,
            Term::BinaryOperation {
                operator: BinaryOperator::Eq,
                left: Box::new(Term::LiteralBoolean(true)),
                right: Box::new(Term::LiteralBoolean(false)),
            }
        );
    }

    #[test]
    fn tokens_sobrando_gera_erro() {
        // dois termos sem operador entre eles
        let result = parse_str("1 2");
        assert!(matches!(result, Err(ParseError::UnexpectedToken { .. })));
    }

    #[test]
    fn entrada_vazia_gera_erro() {
        let result = parse_str("");
        assert!(matches!(result, Err(ParseError::UnexpectedEof)));
    }
}
