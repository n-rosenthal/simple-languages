//! S/src/Scanner/Parser.rs
//!
//! Análise sintática: converte a sequência de tokens em um `Term`.
//! Usa o algoritmo Pratt (precedência subindo) para extensibilidade.
//!
//! author:  n-rosenthal
//! date:    2026-08-17
//! version: 0.6 (let sem 'in' opcional)

use crate::Scanner::Lexer::{Lexer, Token, TokenKind};
use crate::Scanner::Types::{BinaryOperator, Span, Term, Type, UnaryOperator};

/// Erros possíveis durante a análise sintática.
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum ParseError {
    #[error("token inesperado {found:?} em {line}:{column}")]
    UnexpectedToken { found: TokenKind, line: usize, column: usize },
    #[error("tipo desconhecido '{name}' em {line}:{column}")]
    UnknownTypeName { name: String, line: usize, column: usize },
    #[error("fim de entrada inesperado")]
    UnexpectedEof,
}

/// Associatividade de operadores
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Assoc {
    Left,
    Right,
}

type Prec = u8;

/// Consome uma sequência de `Token` e produz um `Term` via Pratt parser.
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    lookahead: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Self {
        let lookahead = lexer.next().transpose().ok().flatten();
        Parser { lexer, lookahead }
    }

    fn peek(&self) -> &Token {
        self.lookahead.as_ref().expect("parser chamado após EOF")
    }

    fn peek_kind(&self) -> &TokenKind {
        &self.peek().kind
    }

    fn advance(&mut self) -> Token {
        let tok = self.lookahead.take().expect("avanço além do EOF");
        self.lookahead = self.lexer.next().transpose().ok().flatten();
        tok
    }

    fn expect(&mut self, expected: TokenKind) -> Result<Span, ParseError> {
        if self.peek_kind() == &expected {
            let tok = self.advance();
            Ok(tok.span)
        } else {
            let tok = self.peek();
            Err(ParseError::UnexpectedToken {
                found: tok.kind.clone(),
                line: tok.span.line,
                column: tok.span.column,
            })
        }
    }

    fn expect_identifier(&mut self) -> Result<(String, Span), ParseError> {
        let tok = self.peek().clone(); // clone to avoid borrow issues
        match &tok.kind {
            TokenKind::Identifier(name) => {
                let span = tok.span;
                self.advance();
                Ok((name.clone(), span))
            }
            _ => Err(ParseError::UnexpectedToken {
                found: tok.kind.clone(),
                line: tok.span.line,
                column: tok.span.column,
            }),
        }
    }

    /// Tabela de precedência e associatividade
    fn operator_info(&self, kind: &TokenKind) -> Option<(Prec, Assoc)> {
        match kind {
            TokenKind::Semicolon => Some((0, Assoc::Left)),
            TokenKind::ColonEquals => Some((1, Assoc::Right)),
            TokenKind::EqualityOperator | TokenKind::InequalityOperator => Some((2, Assoc::Left)),
            TokenKind::LTOperator
            | TokenKind::LEQOperator
            | TokenKind::GTOperator
            | TokenKind::GEQOperator => Some((3, Assoc::Left)),
            TokenKind::AdditionOperator | TokenKind::SubtractionOperator => Some((4, Assoc::Left)),
            _ => None,
        }
    }

    fn make_binary(
        &self,
        op_token: Token,
        left: Term,
        right: Term,
    ) -> Result<Term, ParseError> {
        let span = Span::new(left.span().line, right.span().column);
        let operator = match op_token.kind {
            TokenKind::AdditionOperator => BinaryOperator::Add,
            TokenKind::SubtractionOperator => BinaryOperator::Sub,
            TokenKind::EqualityOperator => BinaryOperator::Eq,
            TokenKind::InequalityOperator => BinaryOperator::Neq,
            TokenKind::LTOperator => BinaryOperator::Lt,
            TokenKind::LEQOperator => BinaryOperator::Leq,
            TokenKind::GTOperator => BinaryOperator::Gt,
            TokenKind::GEQOperator => BinaryOperator::Geq,
            TokenKind::ColonEquals => {
                return Ok(Term::Assign {
                    target: Box::new(left),
                    value: Box::new(right),
                    span,
                });
            }
            TokenKind::Semicolon => {
                return Ok(Term::Sequence {
                    left: Box::new(left),
                    right: Box::new(right),
                    span,
                });
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    found: op_token.kind,
                    line: op_token.span.line,
                    column: op_token.span.column,
                });
            }
        };
        Ok(Term::BinaryOperation {
            operator,
            left: Box::new(left),
            right: Box::new(right),
            span,
        })
    }

    /// Ponto de entrada: parseia a expressão com precedência mínima 0
    pub fn parse(&mut self) -> Result<Term, ParseError> {
        let term = self.parse_expression(0)?;
        match self.peek_kind() {
            TokenKind::Eof => Ok(term),
            found => Err(ParseError::UnexpectedToken {
                found: found.clone(),
                line: self.peek().span.line,
                column: self.peek().span.column,
            }),
        }
    }

    /// Núcleo do Pratt: parseia expressões binárias com precedência
    fn parse_expression(&mut self, min_prec: Prec) -> Result<Term, ParseError> {
        let mut left = self.parse_primary()?;

        while let Some((prec, assoc)) = self.operator_info(self.peek_kind()) {
            if prec < min_prec {
                break;
            }
            let op_token = self.advance();
            let next_min = match assoc {
                Assoc::Left => prec + 1,
                Assoc::Right => prec,
            };
            let right = self.parse_expression(next_min)?;
            left = self.make_binary(op_token, left, right)?;
        }

        Ok(left)
    }

    // primary := INTEGER | BOOLEAN | UNIT | IDENTIFIER
    //          | "(" assign ")"
    //          | "if" equality "then" assign "else" assign
    //          | "let" IDENTIFIER ":" type "=" assign ["in" assign]?
    fn parse_primary(&mut self) -> Result<Term, ParseError> {
        let token = self.peek().clone();
        let span = token.span;

        match token.kind {
            TokenKind::LiteralInteger(n) => {
                self.advance();
                Ok(Term::LiteralInteger { value: n, span })
            }
            TokenKind::LiteralBoolean(b) => {
                self.advance();
                Ok(Term::LiteralBoolean { value: b, span })
            }
            TokenKind::UnitLiteral => {
                self.advance();
                Ok(Term::Unit { span })
            }
            TokenKind::Identifier(name) => {
                self.advance();
                Ok(Term::Variable { name, span })
            }
            TokenKind::LParen => {
                self.advance();
                let inner = self.parse_expression(0)?;
                self.expect(TokenKind::RParen)?;
                Ok(inner)
            }
            TokenKind::If => {
                self.advance();
                let cond_span = self.peek().span;
                let condition = self.parse_expression(0)?;
                self.expect(TokenKind::Then)?;
                let then_branch = self.parse_expression(0)?;
                self.expect(TokenKind::Else)?;
                let else_branch = self.parse_expression(0)?;
                let span = Span::new(cond_span.line, else_branch.span().column);
                Ok(Term::Conditional {
                    condition: Box::new(condition),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                    span,
                })
            }
            TokenKind::Let => {
                self.advance();
                let (name, name_span) = self.expect_identifier()?;
                self.expect(TokenKind::Colon)?;
                let declared_type = self.parse_type()?;
                self.expect(TokenKind::Assign)?;
                let value = self.parse_expression(0)?;
                // Verifica se há 'in' e corpo; se não, body = Unit
                let (body, body_span) = if matches!(self.peek_kind(), TokenKind::In) {
                    self.advance(); // consome 'in'
                    let body = self.parse_expression(0)?;
                    let body_span = body.span();
                    (body, body_span)
                } else {
                    let body_span = Span::new(self.peek().span.line, self.peek().span.column);
                    (Term::Unit { span: body_span }, body_span)
                };
                let span = Span::new(name_span.line, body_span.column);
                Ok(Term::Let {
                    name,
                    declared_type,
                    value: Box::new(value),
                    body: Box::new(body),
                    span,
                })
            }
            TokenKind::NotOperator => {
                self.advance();
                let operand = self.parse_expression(5)?; // prefixo tem precedência alta
                let span = Span::new(span.line, operand.span().column);
                Ok(Term::UnaryOperation {
                    operator: UnaryOperator::Not,
                    operand: Box::new(operand),
                    span,
                })
            }
            TokenKind::RefKeyword => {
                self.advance();
                let operand = self.parse_expression(5)?;
                let span = Span::new(span.line, operand.span().column);
                Ok(Term::Ref {
                    inner: Box::new(operand),
                    span,
                })
            }
            TokenKind::Bang => {
                self.advance();
                let operand = self.parse_expression(5)?;
                let span = Span::new(span.line, operand.span().column);
                Ok(Term::Deref {
                    inner: Box::new(operand),
                    span,
                })
            }
            TokenKind::Eof => Err(ParseError::UnexpectedEof),
            found => Err(ParseError::UnexpectedToken {
                found,
                line: span.line,
                column: span.column,
            }),
        }
    }

    /// Parseia uma expressão de tipo: `Integer`, `Boolean`, `Unit`,
    /// ou `Ref <type>` (recursivo).
    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let token = self.peek().clone();
        match &token.kind {
            TokenKind::Identifier(name) => match name.as_str() {
                "Integer" => {
                    self.advance();
                    Ok(Type::Integer)
                }
                "Boolean" => {
                    self.advance();
                    Ok(Type::Boolean)
                }
                "Unit" => {
                    self.advance();
                    Ok(Type::Unit)
                }
                "Ref" => {
                    self.advance();
                    let inner = self.parse_type()?;
                    Ok(Type::Reference(Box::new(inner)))
                }
                _ => Err(ParseError::UnknownTypeName {
                    name: name.clone(),
                    line: token.span.line,
                    column: token.span.column,
                }),
            },
            found => Err(ParseError::UnexpectedToken {
                found: found.clone(),
                line: token.span.line,
                column: token.span.column,
            }),
        }
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
        let lexer = Lexer::new(&src);
        let mut parser = Parser::new(lexer);
        parser.parse()
    }

    #[test]
    fn parseia_soma_simples() {
        let term = parse_str("1 + 2").expect("deveria parsear");
        match term {
            Term::BinaryOperation { operator, left, right, .. } => {
                assert_eq!(operator, BinaryOperator::Add);
                assert!(matches!(*left, Term::LiteralInteger { value: 1, .. }));
                assert!(matches!(*right, Term::LiteralInteger { value: 2, .. }));
            }
            _ => panic!("não é BinaryOperation"),
        }
    }

    #[test]
    fn parseia_sequencia() {
        let term = parse_str("1 ; 2").expect("deveria parsear");
        assert!(matches!(term, Term::Sequence { .. }));
    }

    #[test]
    fn parseia_let_simples() {
        let term = parse_str("let x: Integer = 1 in x + 1").expect("deveria parsear");
        assert!(matches!(term, Term::Let { name, .. } if name == "x"));
    }

    #[test]
    fn parseia_let_sem_in() {
        let term = parse_str("let x: Integer = 10").expect("deveria parsear");
        match term {
            Term::Let { name, body, .. } => {
                assert_eq!(name, "x");
                assert!(matches!(*body, Term::Unit { .. }));
            }
            _ => panic!("não é Let"),
        }
    }

    #[test]
    fn parseia_ref_e_deref() {
        let term = parse_str("!(ref 1)").expect("deveria parsear");
        assert!(matches!(term, Term::Deref { inner, .. } if matches!(**inner, Term::Ref { .. })));
    }

    #[test]
    fn parseia_assign() {
        let term = parse_str("r := 5").expect("deveria parsear");
        assert!(matches!(term, Term::Assign { target, value, .. } if matches!(*target, Term::Variable { name, .. } if name == "r")));
    }

    #[test]
    fn parseia_tipo_ref_no_let() {
        let term = parse_str("let r: Ref Integer = ref 1 in !r").expect("deveria parsear");
        match term {
            Term::Let { declared_type, .. } => {
                assert_eq!(declared_type, Type::Reference(Box::new(Type::Integer)));
            }
            _ => panic!("não é Let"),
        }
    }

    #[test]
    fn erro_tipo_desconhecido() {
        let result = parse_str("let x: Wat = 1 in x");
        assert!(matches!(result, Err(ParseError::UnknownTypeName { name, .. }) if name == "Wat"));
    }

    #[test]
    fn entrada_vazia_gera_erro() {
        assert!(matches!(parse_str(""), Err(ParseError::UnexpectedEof)));
    }
}
