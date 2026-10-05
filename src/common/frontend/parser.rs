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

    /// O próximo token é do tipo `kind`?
    pub fn at(&self, kind: K) -> bool {
        self.peek_kind() == Some(kind)
    }

    /// O próximo token é de algum dos tipos?
    pub fn at_any(&self, kinds: &[K]) -> bool {
        self.peek_kind().is_some_and(|kind| kinds.contains(&kind))
    }

    /// Consome o próximo token se for do tipo `kind`.
    pub fn eat(&mut self, kind: K) -> Option<&'a Token<K>> {
        if self.at(kind) {
            self.next()
        } else {
            None
        }
    }

    /// O erro para o token atual, que não serve ao que o parser esperava (ou
    /// `UnexpectedEnd`, se a entrada acabou). Não consome nada.
    pub fn unexpected(&self) -> ParseError<K> {
        match self.peek() {
            Some(token) => ParseError::UnexpectedToken {
                expected: None,
                found: token.kind,
                span: token.span,
            },
            None => ParseError::UnexpectedEnd { expected: None },
        }
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

// =============================================================================
// Operadores binários
// =============================================================================

/// Como operadores do mesmo nível se agrupam.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Assoc {
    /// `a - b - c` é `(a - b) - c`.
    Left,
    /// `a -> b -> c` é `a -> (b -> c)`.
    Right,
    /// `a < b < c` é um erro.
    Non,
}

/// Um nível de precedência: operadores que se agrupam do mesmo modo.
#[derive(Debug, Clone, Copy)]
pub struct Level<K: 'static, O: 'static> {
    pub operators: &'static [(K, O)],
    pub assoc: Assoc,
}

impl<K, O> Level<K, O> {
    pub const fn left(operators: &'static [(K, O)]) -> Self {
        Self { operators, assoc: Assoc::Left }
    }

    pub const fn right(operators: &'static [(K, O)]) -> Self {
        Self { operators, assoc: Assoc::Right }
    }

    pub const fn non(operators: &'static [(K, O)]) -> Self {
        Self { operators, assoc: Assoc::Non }
    }
}

fn operator_at<K: Copy + Eq, O: Copy>(stream: &TokenStream<'_, K>, level: &Level<K, O>) -> Option<O> {
    let kind = stream.peek_kind()?;
    level
        .operators
        .iter()
        .find(|(token, _)| *token == kind)
        .map(|(_, op)| *op)
}

/// Operadores binários por níveis de precedência, do mais fraco (índice 0) ao
/// mais forte, cada nível com a sua associatividade.
///
/// ```ignore
/// const LEVELS: &[Level<Kind, Op>] = &[
///     Level::left(&[(Kind::Or, Op::Or)]),
///     Level::non(&[(Kind::Lt, Op::Lt)]),
///     Level::left(&[(Kind::Plus, Op::Add), (Kind::Minus, Op::Sub)]),
///     Level::right(&[(Kind::Cons, Op::Cons)]),
/// ];
/// parse_binary(stream, LEVELS, &Self::parse_primary, &Term::binary)
/// ```
///
/// `operand` lê o operando mais forte (o que está abaixo do último nível) e
/// `combine` monta o nó de um operador.
pub fn parse_binary<K, O, T>(
    stream: &mut TokenStream<'_, K>,
    levels: &[Level<K, O>],
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
    levels: &[Level<K, O>],
    index: usize,
    operand: &dyn Fn(&mut TokenStream<'_, K>) -> Result<T, ParseError<K>>,
    combine: &dyn Fn(O, T, T) -> T,
) -> Result<T, ParseError<K>>
where
    K: Copy + Eq,
    O: Copy,
{
    let Some(level) = levels.get(index) else {
        return operand(stream);
    };

    let mut lhs = climb(stream, levels, index + 1, operand, combine)?;

    match level.assoc {
        Assoc::Left => {
            while let Some(op) = operator_at(stream, level) {
                stream.next();
                let rhs = climb(stream, levels, index + 1, operand, combine)?;
                lhs = combine(op, lhs, rhs);
            }
            Ok(lhs)
        }

        Assoc::Right => match operator_at(stream, level) {
            Some(op) => {
                stream.next();
                // o lado direito é do mesmo nível: `a -> (b -> c)`
                let rhs = climb(stream, levels, index, operand, combine)?;
                Ok(combine(op, lhs, rhs))
            }
            None => Ok(lhs),
        },

        Assoc::Non => match operator_at(stream, level) {
            Some(op) => {
                stream.next();
                let rhs = climb(stream, levels, index + 1, operand, combine)?;
                if operator_at(stream, level).is_some() {
                    return Err(stream.unexpected()); // `a < b < c`
                }
                Ok(combine(op, lhs, rhs))
            }
            None => Ok(lhs),
        },
    }
}

/// Operadores prefixos: `prefix* operando`, aplicados de dentro para fora
/// (`succ succ 0` é `succ (succ 0)`).
pub fn parse_prefix<K, O, T>(
    stream: &mut TokenStream<'_, K>,
    prefixes: &[(K, O)],
    operand: &dyn Fn(&mut TokenStream<'_, K>) -> Result<T, ParseError<K>>,
    apply: &dyn Fn(O, T) -> T,
) -> Result<T, ParseError<K>>
where
    K: Copy + Eq,
    O: Copy,
{
    let mut operators = Vec::new();
    while let Some(op) = stream.peek_kind().and_then(|kind| {
        prefixes
            .iter()
            .find(|(token, _)| *token == kind)
            .map(|(_, op)| *op)
    }) {
        stream.next();
        operators.push(op);
    }

    let mut value = operand(stream)?;
    for op in operators.into_iter().rev() {
        value = apply(op, value);
    }
    Ok(value)
}

// =============================================================================
// Combinadores
// =============================================================================

/// `open inner close`: um grupo entre delimitadores, como `( termo )`.
pub fn delimited<K, T>(
    stream: &mut TokenStream<'_, K>,
    open: K,
    close: K,
    inner: impl FnOnce(&mut TokenStream<'_, K>) -> Result<T, ParseError<K>>,
) -> Result<T, ParseError<K>>
where
    K: Copy + Eq,
{
    stream.expect(open)?;
    let value = inner(stream)?;
    stream.expect(close)?;
    Ok(value)
}

/// `item (separator item)*`: pelo menos um item.
pub fn separated<K, T>(
    stream: &mut TokenStream<'_, K>,
    separator: K,
    mut item: impl FnMut(&mut TokenStream<'_, K>) -> Result<T, ParseError<K>>,
) -> Result<Vec<T>, ParseError<K>>
where
    K: Copy + Eq,
{
    let mut items = vec![item(stream)?];
    while stream.eat(separator).is_some() {
        items.push(item(stream)?);
    }
    Ok(items)
}

/// `open (item (separator item)*)? close`: uma lista, possivelmente vazia,
/// como `{a, b}` ou `()`.
pub fn delimited_list<K, T>(
    stream: &mut TokenStream<'_, K>,
    open: K,
    close: K,
    separator: K,
    item: impl FnMut(&mut TokenStream<'_, K>) -> Result<T, ParseError<K>>,
) -> Result<Vec<T>, ParseError<K>>
where
    K: Copy + Eq,
{
    stream.expect(open)?;
    if stream.eat(close).is_some() {
        return Ok(Vec::new());
    }

    let items = separated(stream, separator, item)?;
    stream.expect(close)?;
    Ok(items)
}

/// `item*`: itens enquanto o próximo token for de algum dos tipos `starts`.
pub fn many<K, T>(
    stream: &mut TokenStream<'_, K>,
    starts: &[K],
    mut item: impl FnMut(&mut TokenStream<'_, K>) -> Result<T, ParseError<K>>,
) -> Result<Vec<T>, ParseError<K>>
where
    K: Copy + Eq,
{
    let mut items = Vec::new();
    while stream.at_any(starts) {
        items.push(item(stream)?);
    }
    Ok(items)
}

/// `operando operando*`, associado à esquerda: a aplicação `f x y` é
/// `(f x) y`. Continua enquanto o próximo token começar um operando (`starts`).
pub fn left_chain<K, T>(
    stream: &mut TokenStream<'_, K>,
    starts: &[K],
    operand: &dyn Fn(&mut TokenStream<'_, K>) -> Result<T, ParseError<K>>,
    combine: &dyn Fn(T, T) -> T,
) -> Result<T, ParseError<K>>
where
    K: Copy + Eq,
{
    let mut lhs = operand(stream)?;
    while stream.at_any(starts) {
        let rhs = operand(stream)?;
        lhs = combine(lhs, rhs);
    }
    Ok(lhs)
}
