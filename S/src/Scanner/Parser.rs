//! S/src/Scanner/Parser.rs
//!
//! Análise sintática: converte a sequência de tokens em um `Term`.
//!
//! author:  n-rosenthal
//! date:    2026-08-17
//! version: 0.4

use crate::Scanner::Lexer::{Token, TokenKind};
use crate::Scanner::Types::{BinaryOperator, Term, Type, UnaryOperator};

/// Erros possíveis durante a análise sintática.
#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedToken { found: TokenKind, line: usize, column: usize },
    UnknownTypeName { name: String, line: usize, column: usize },
    UnexpectedEof,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken { found, line, column } => {
                write!(f, "token inesperado {found:?} em {line}:{column}")
            }
            ParseError::UnknownTypeName { name, line, column } => {
                write!(f, "tipo desconhecido '{name}' em {line}:{column}")
            }
            ParseError::UnexpectedEof => write!(f, "fim de entrada inesperado"),
        }
    }
}

impl std::error::Error for ParseError {}

/// Consome uma sequência de `Token` e produz um `Term`.
///
/// Precedência (da mais baixa para a mais alta):
/// `:=` (atribuição) < `==`/`<>` < `<`/`<=`/`>`/`>=` < `+`/`-` <
/// `not`/`ref`/`!` (prefixos unários) < literais/variáveis/parênteses.
pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, position: 0 }
    }

    pub fn parse(mut self) -> Result<Term, ParseError> {
        let term = self.parse_assign()?;

        match self.peek_kind() {
            TokenKind::Eof => Ok(term),
            found => Err(ParseError::UnexpectedToken {
                found: found.clone(),
                line: self.peek().line,
                column: self.peek().column,
            }),
        }
    }

    // assign := equality ( ":=" assign )?   (associativo à direita)
    fn parse_assign(&mut self) -> Result<Term, ParseError> {
        let target = self.parse_equality()?;

        if matches!(self.peek_kind(), TokenKind::ColonEquals) {
            self.advance();
            let value = self.parse_assign()?;
            return Ok(Term::Assign { target: Box::new(target), value: Box::new(value) });
        }

        Ok(target)
    }

    // equality := relational ( ("==" | "<>") relational )*
    fn parse_equality(&mut self) -> Result<Term, ParseError> {
        let mut left = self.parse_relational()?;
        loop {
            let operator = match self.peek_kind() {
                TokenKind::EqualityOperator => BinaryOperator::Eq,
                TokenKind::InequalityOperator => BinaryOperator::Neq,
                _ => break,
            };
            self.advance();
            let right = self.parse_relational()?;
            left = Term::BinaryOperation { operator, left: Box::new(left), right: Box::new(right) };
        }
        Ok(left)
    }

    // relational := additive ( ("<" | "<=" | ">" | ">=") additive )*
    fn parse_relational(&mut self) -> Result<Term, ParseError> {
        let mut left = self.parse_additive()?;
        loop {
            let operator = match self.peek_kind() {
                TokenKind::LTOperator => BinaryOperator::Lt,
                TokenKind::LEQOperator => BinaryOperator::Leq,
                TokenKind::GTOperator => BinaryOperator::Gt,
                TokenKind::GEQOperator => BinaryOperator::Geq,
                _ => break,
            };
            self.advance();
            let right = self.parse_additive()?;
            left = Term::BinaryOperation { operator, left: Box::new(left), right: Box::new(right) };
        }
        Ok(left)
    }

    // additive := unary ( ("+" | "-") unary )*
    fn parse_additive(&mut self) -> Result<Term, ParseError> {
        let mut left = self.parse_unary()?;
        loop {
            let operator = match self.peek_kind() {
                TokenKind::AdditionOperator => BinaryOperator::Add,
                TokenKind::SubtractionOperator => BinaryOperator::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_unary()?;
            left = Term::BinaryOperation { operator, left: Box::new(left), right: Box::new(right) };
        }
        Ok(left)
    }

    // unary := "not" unary | "ref" unary | "!" unary | primary
    fn parse_unary(&mut self) -> Result<Term, ParseError> {
        if matches!(self.peek_kind(), TokenKind::NotOperator) {
            self.advance();
            let operand = self.parse_unary()?;
            return Ok(Term::UnaryOperation { operator: UnaryOperator::Not, operand: Box::new(operand) });
        }

        if matches!(self.peek_kind(), TokenKind::RefKeyword) {
            self.advance();
            let operand = self.parse_unary()?;
            return Ok(Term::Ref(Box::new(operand)));
        }

        if matches!(self.peek_kind(), TokenKind::Bang) {
            self.advance();
            let operand = self.parse_unary()?;
            return Ok(Term::Deref(Box::new(operand)));
        }

        self.parse_primary()
    }

    // primary := INTEGER | BOOLEAN | UNIT | IDENTIFIER
    //          | "(" assign ")"
    //          | "if" equality "then" assign "else" assign
    //          | "let" IDENTIFIER ":" type "=" assign "in" assign
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
            TokenKind::UnitLiteral => {
                self.advance();
                Ok(Term::Unit)
            }
            TokenKind::Identifier(name) => {
                self.advance();
                Ok(Term::Variable(name))
            }
            TokenKind::LParen => {
                self.advance();
                let inner = self.parse_assign()?;
                self.expect(TokenKind::RParen)?;
                Ok(inner)
            }
            TokenKind::If => {
                self.advance();
                let condition = self.parse_equality()?;
                self.expect(TokenKind::Then)?;
                let then_branch = self.parse_assign()?;
                self.expect(TokenKind::Else)?;
                let else_branch = self.parse_assign()?;
                Ok(Term::Conditional {
                    condition: Box::new(condition),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                })
            }
            TokenKind::Let => {
                self.advance();
                let name = self.expect_identifier()?;
                self.expect(TokenKind::Colon)?;
                let declared_type = self.parse_type()?;
                self.expect(TokenKind::Assign)?;
                let value = self.parse_assign()?;
                self.expect(TokenKind::In)?;
                let body = self.parse_assign()?;
                Ok(Term::Let {
                    name,
                    declared_type,
                    value: Box::new(value),
                    body: Box::new(body),
                })
            }
            TokenKind::Eof => Err(ParseError::UnexpectedEof),
            found => Err(ParseError::UnexpectedToken { found, line: token.line, column: token.column }),
        }
    }

    /// Consome um `Identifier`, ou retorna erro se o token atual não for um.
    fn expect_identifier(&mut self) -> Result<String, ParseError> {
        let token = self.peek().clone();
        match token.kind {
            TokenKind::Identifier(name) => {
                self.advance();
                Ok(name)
            }
            found => Err(ParseError::UnexpectedToken { found, line: token.line, column: token.column }),
        }
    }

    /// Consome uma expressão de tipo: `Integer`, `Boolean`, `Unit`,
    /// ou `Ref <type>` (recursivo, para referências aninhadas).
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
                    line: token.line,
                    column: token.column,
                }),
            },
            found => Err(ParseError::UnexpectedToken {
                found: found.clone(),
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
    fn parseia_let_simples() {
        let term = parse_str("let x: Integer = 1 in x + 1").expect("deveria parsear");
        assert_eq!(
            term,
            Term::Let {
                name: "x".to_string(),
                declared_type: Type::Integer,
                value: Box::new(Term::LiteralInteger(1)),
                body: Box::new(Term::BinaryOperation {
                    operator: BinaryOperator::Add,
                    left: Box::new(Term::Variable("x".to_string())),
                    right: Box::new(Term::LiteralInteger(1)),
                }),
            }
        );
    }

    #[test]
    fn parseia_ref_e_deref() {
        let term = parse_str("!(ref 1)").expect("deveria parsear");
        assert_eq!(
            term,
            Term::Deref(Box::new(Term::Ref(Box::new(Term::LiteralInteger(1)))))
        );
    }

    #[test]
    fn parseia_assign() {
        let term = parse_str("r := 5").expect("deveria parsear");
        assert_eq!(
            term,
            Term::Assign {
                target: Box::new(Term::Variable("r".to_string())),
                value: Box::new(Term::LiteralInteger(5)),
            }
        );
    }

    #[test]
    fn parseia_tipo_ref_no_let() {
        let term = parse_str("let r: Ref Integer = ref 1 in !r").expect("deveria parsear");
        assert_eq!(
            term,
            Term::Let {
                name: "r".to_string(),
                declared_type: Type::Reference(Box::new(Type::Integer)),
                value: Box::new(Term::Ref(Box::new(Term::LiteralInteger(1)))),
                body: Box::new(Term::Deref(Box::new(Term::Variable("r".to_string())))),
            }
        );
    }

    #[test]
    fn parseia_unit() {
        let term = parse_str("unit").expect("deveria parsear");
        assert_eq!(term, Term::Unit);
    }

    #[test]
    fn assign_dentro_de_let_como_valor() {
        // let ignored: Unit = (r := 5) in !r
        let term = parse_str("let ignored: Unit = (r := 5) in !r").expect("deveria parsear");
        match term {
            Term::Let { declared_type, value, .. } => {
                assert_eq!(declared_type, Type::Unit);
                assert!(matches!(*value, Term::Assign { .. }));
            }
            _ => panic!("deveria ser um Let"),
        }
    }

    #[test]
    fn condicional_completo() {
        let term = parse_str("if true then 1 else 2").expect("deveria parsear");
        assert_eq!(
            term,
            Term::Conditional {
                condition: Box::new(Term::LiteralBoolean(true)),
                then_branch: Box::new(Term::LiteralInteger(1)),
                else_branch: Box::new(Term::LiteralInteger(2)),
            }
        );
    }

    #[test]
    fn tipo_desconhecido_em_let_e_erro() {
        let result = parse_str("let x: Wat = 1 in x");
        assert!(matches!(result, Err(ParseError::UnknownTypeName { .. })));
    }

    #[test]
    fn entrada_vazia_gera_erro() {
        assert!(matches!(parse_str(""), Err(ParseError::UnexpectedEof)));
    }
}
