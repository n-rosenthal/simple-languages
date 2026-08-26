//! Tipos e abstrações comuns às linguagens implementadas pelo projeto.
//!
//! A arquitetura geral é:
//
//! ```text
//! Source
//!   │
//!   ▼
//! Scanner
//!   │
//!   ▼
//! SourceLine
//!   │
//!   ▼
//! Lexer
//!   │
//!   ▼
//! Token
//!   │
//!   ▼
//! Parser
//!   │
//!   ▼
//! Term
//!   │
//!   ├───────────────┐
//!   ▼               ▼
//! TypeChecker     Evaluator
//!   │               │
//!   ▼               ▼
//! Type            Value
//! ```


// =============================================================================
// Source
// =============================================================================

/// Uma linha da fonte de um programa.
///
/// Mantemos o número da linha juntamente com o seu conteúdo para que
/// scanners, lexers e futuros diagnósticos de erro possam preservar
/// informação de localização.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceLine {
    /// Número da linha, começando em 1.
    pub number: usize,

    /// Conteúdo da linha, sem o terminador de linha.
    pub text: String,
}

impl SourceLine {
    pub fn new(
        number: usize,
        text: impl Into<String>,
    ) -> Self {
        Self {
            number,
            text: text.into(),
        }
    }
}


// =============================================================================
// Span
// =============================================================================

/// Intervalo de posições dentro de uma fonte.
///
/// O intervalo é semiaberto:
///
/// ```text
/// start <= posição < end
/// ```
///
/// Assim, um span `(0, 3)` representa os três caracteres nas posições
/// `0`, `1` e `2`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub const fn new(
        start: usize,
        end: usize,
    ) -> Self {
        Self { start, end }
    }

    pub const fn len(self) -> usize {
        self.end - self.start
    }

    pub const fn is_empty(self) -> bool {
        self.start == self.end
    }
}


// =============================================================================
// Token
// =============================================================================

/// Interface mínima que um tipo de token deve fornecer.
///
/// Cada linguagem pode possuir seu próprio tipo concreto de token.
pub trait Token {
    type Type: TokenType;

    fn kind(&self) -> Self::Type;

    fn span(&self) -> Span;

    fn lexeme(&self) -> &str;
}


// =============================================================================
// TokenType
// =============================================================================

/// Trait para os tipos que representam categorias de tokens.
///
/// Por exemplo, `arith` terá:
///
/// ```text
/// Integer
/// Boolean
/// Plus
/// Minus
/// Star
/// ...
/// ```
pub trait TokenType:
    Copy
    + Eq
    + std::fmt::Debug
{
}


// =============================================================================
// Scanner
// =============================================================================

/// Primeira etapa do pipeline de compilação.
///
/// O scanner recebe texto bruto e produz linhas estruturadas.
pub trait Scanner {
    type Error;

    fn scan(
        input: &str,
    ) -> Result<Vec<SourceLine>, Self::Error>;
}


// =============================================================================
// Lexer
// =============================================================================

/// Segunda etapa do pipeline.
///
/// O lexer transforma linhas da fonte em tokens.
pub trait Lexer {
    type Token: Token;
    type Error;

    fn analyze(
        input: &[SourceLine],
    ) -> Result<Vec<Self::Token>, Self::Error>;
}


// =============================================================================
// Parser
// =============================================================================

/// Terceira etapa do pipeline.
///
/// O parser transforma tokens em uma árvore sintática.
pub trait Parser {
    type Token: Token;
    type Term;
    type Error;

    fn parse(
        input: &[Self::Token],
    ) -> Result<Self::Term, Self::Error>;
}


// =============================================================================
// Type checking
// =============================================================================

/// Verificador de tipos.
///
/// O typechecker recebe um termo e produz:
///
/// 1. o tipo do termo;
/// 2. as regras de tipagem utilizadas na derivação.
pub trait TypeChecker {
    type Term;
    type Type;
    type Rule;
    type Error;

    fn check(
        term: &Self::Term,
    ) -> Result<(Self::Type, Vec<Self::Rule>), Self::Error>;
}


// =============================================================================
// Evaluation
// =============================================================================

/// Avaliador da linguagem.
///
/// A implementação pode representar uma semântica big-step ou small-step.
///
/// Na semântica big-step:
///
/// ```text
/// t ⇓ v
/// ```
///
/// Na semântica small-step:
///
/// ```text
/// t → t'
/// ```
pub trait Evaluator {
    type Term;
    type Value;
    type Rule;
    type Error;

    fn evaluate(
        term: &Self::Term,
    ) -> Result<(Self::Value, Vec<Self::Rule>), Self::Error>;
}


// =============================================================================
// Language
// =============================================================================

/// Descrição dos componentes fundamentais de uma linguagem.
///
/// Esta trait funciona como uma associação entre os tipos específicos
/// utilizados por uma linguagem.
pub trait Language {
    type Term;
    type Type;
    type Value;
    type Error;
}


// =============================================================================
// Compiler
// =============================================================================

/// Representa uma linguagem de máquina alvo.
pub trait MachineLanguage {
    type Instruction;
}


/// Compilador de uma linguagem fonte para uma linguagem alvo.
pub trait Compiler {
    type SourceTerm;
    type Target: MachineLanguage;
    type Error;

    fn compile(
        term: &Self::SourceTerm,
    ) -> Result<
        Vec<<Self::Target as MachineLanguage>::Instruction>,
        Self::Error,
    >;
}


// =============================================================================
// Abstract machine
// =============================================================================

/// Máquina abstrata.
///
/// Uma implementação concreta poderá acrescentar registradores, memória,
/// pilha, contador de programa etc.
pub trait AbstractMachine {
    type Instruction;
    type State;
    type Value;
    type Error;

    fn execute(
        &mut self,
        instruction: &Self::Instruction,
    ) -> Result<Self::Value, Self::Error>;
}
