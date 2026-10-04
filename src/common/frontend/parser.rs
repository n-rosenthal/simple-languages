//! Peças comuns dos parsers de descida recursiva: o fluxo de tokens, os erros
//! e uma tabela de precedência para operadores binários.

use std::fmt;

use crate::common::Span;

use super::lexer::Token;

// =============================================================================
// TokenStream
// =============================================================================

/// O fluxo de tokens de um parser: olha o próximo, consome, espera um tipo.
pub struct TokenStream<'a, K> {
    tokens: &'a [Token<K>],
    position: usize,
}

impl<'a, K: Copy + Eq> TokenStream<'a, K> {
    pub fn new(tokens: &'a [Token<K>]) -> Self {
        Self { tokens, position: 0 }
    }

    /// O próximo token, sem consumi-lo.
    pub fn peek(&self) -> Option<&'a Token<K>> {
        self.tokens.get(self.position)
    }

    /// O tipo do próximo token.
    pub fn peek_kind(&self) -> Option<K> {
        self.peek().map(|token| token.kind)
    }

    /// Consome o próximo token.
    pub fn next(&mut self) -> Option<&'a Token<K>> {
        let token = self.tokens.get(self.position);
        if token.is_some() {
            self.position += 1;
        }
        token
    }

    pub fn is_at_end(&self) -> bool {
        self.position >= self.tokens.len()
    }

    pub fn position(&self) -> usize {
        self.position
    }

    /// Consome um token do tipo esperado, ou falha dizendo o que encontrou.
    pub fn expect(&mut self, expected: K) -> Result<Token<K>, ParseError<K>> {
        match self.next() {
            Some(token) if token.kind == expected => Ok(token.clone()),
            Some(token) => Err(ParseError::UnexpectedToken {
                expected: Some(expected),
                found: token.kind,
                span: token.span,
            }),
            None => Err(ParseError::UnexpectedEnd { expected: Some(expected) }),
        }
    }

    /// Consome o próximo token, que não pode faltar.
    pub fn advance(&mut self) -> Result<Token<K>, ParseError<K>> {
        self.next()
            .cloned()
            .ok_or(ParseError::UnexpectedEnd { expected: None })
    }
}

// =============================================================================
// ParseError
// =============================================================================

/// Erros dos parsers.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError<K> {
    /// Um token diferente do esperado foi encontrado.
    UnexpectedToken { expected: Option<K>, found: K, span: Span },
    /// A entrada acabou antes de terminar a expressão.
    UnexpectedEnd { expected: Option<K> },
    /// Restaram tokens depois da expressão principal.
    UnexpectedTrailingToken { found: K, span: Span },
    /// Um literal que não cabe em `i64`.
    InvalidInteger { lexeme: String, span: Span },
}

impl<K: fmt::Debug> fmt::Display for ParseError<K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedToken { expected: Some(expected), found, span } => write!(
                f,
                "expected `{expected:?}`, found `{found:?}` at {}..{}",
                span.start, span.end
            ),
            Self::UnexpectedToken { expected: None, found, span } => write!(
                f,
                "unexpected token `{found:?}` at {}..{}",
                span.start, span.end
            ),
            Self::UnexpectedEnd { expected: Some(expected) } => {
                write!(f, "expected `{expected:?}`, found end of input")
            }
            Self::UnexpectedEnd { expected: None } => write!(f, "unexpected end of input"),
            Self::UnexpectedTrailingToken { found, span } => write!(
                f,
                "unexpected trailing token `{found:?}` at {}..{}",
                span.start, span.end
            ),
            Self::InvalidInteger { lexeme, span } => write!(
                f,
                "integer `{lexeme}` at {}..{} does not fit in 64 bits",
                span.start, span.end
            ),
        }
    }
}

impl<K: fmt::Debug> std::error::Error for ParseError<K> {}

// =============================================================================
// Ajudantes
// =============================================================================

/// Roda `parse` sobre os tokens e exige que não sobre nenhum.
pub fn parse_complete<K, T>(
    tokens: &[Token<K>],
    parse: impl FnOnce(&mut TokenStream<'_, K>) -> Result<T, ParseError<K>>,
) -> Result<T, ParseError<K>>
where
    K: Copy + Eq,
{
    let mut stream = TokenStream::new(tokens);
    let value = parse(&mut stream)?;

    match stream.peek() {
        Some(token) => Err(ParseError::UnexpectedTrailingToken {
            found: token.kind,
            span: token.span,
        }),
        None => Ok(value),
    }
}

/// Um literal inteiro: o lexema vira `i64`, ou falha com `InvalidInteger`.
pub fn parse_integer<K>(token: &Token<K>) -> Result<i64, ParseError<K>> {
    token.lexeme.parse::<i64>().map_err(|_| ParseError::InvalidInteger {
        lexeme: token.lexeme.clone(),
        span: token.span,
    })
}

/// Uma tabela de precedência para operadores binários associativos à
/// esquerda: do nível mais fraco (índice 0) ao mais forte.
///
/// ```ignore
/// const LEVELS: &[&[(Kind, Op)]] = &[
///     &[(Kind::Or, Op::Or)],
///     &[(Kind::Plus, Op::Add), (Kind::Minus, Op::Sub)],
///     &[(Kind::Star, Op::Mul)],
/// ];
/// parse_binary(stream, LEVELS, &Self::parse_primary, &Term::binary)
/// ```
///
/// `operand` lê o operando mais forte (o que está abaixo do último nível) e
/// `combine` monta o nó de um operador.
pub fn parse_binary<K, O, T>(
    stream: &mut TokenStream<'_, K>,
    levels: &[&[(K, O)]],
    operand: &dyn Fn(&mut TokenStream<'_, K>) -> Result<T, ParseError<K>>,
    combine: &dyn Fn(O, T, T) -> T,
) -> Result<T, ParseError<K>>
where
    K: Copy + Eq,
    O: Copy,
{
    climb(stream, levels, 0, operand, combine)
}

fn climb<K, O, T>(
    stream: &mut TokenStream<'_, K>,
    levels: &[&[(K, O)]],
    level: usize,
    operand: &dyn Fn(&mut TokenStream<'_, K>) -> Result<T, ParseError<K>>,
    combine: &dyn Fn(O, T, T) -> T,
) -> Result<T, ParseError<K>>
where
    K: Copy + Eq,
    O: Copy,
{
    let Some(operators) = levels.get(level) else {
        return operand(stream);
    };

    let mut lhs = climb(stream, levels, level + 1, operand, combine)?;

    while let Some(op) = stream.peek_kind().and_then(|kind| {
        operators
            .iter()
            .find(|(token, _)| *token == kind)
            .map(|(_, op)| *op)
    }) {
        stream.next();
        let rhs = climb(stream, levels, level + 1, operand, combine)?;
        lhs = combine(op, lhs, rhs);
    }

    Ok(lhs)
}
