//! Termos do lambda-cálculo sem nomes explícitos (TAPL, caps. 6–7).
//!
//! A representação utiliza índices de De Bruijn para identificar variáveis
//! ligadas. O índice 0 representa a variável ligada pelo binder mais próximo,
//! o índice 1 representa a próxima variável externa, e assim por diante.
//!
//! ```text
//! t ::= x | λ.t | t t
//! ```
//!
//! Exemplos:
//!
//! ```text
//! λx. x       -> Abs(Var(0))
//! λx. λy. x   -> Abs(Abs(Var(1)))
//! λx. λy. y   -> Abs(Abs(Var(0)))
//! ```
//!
//! Diferentemente da representação nomeada, termos que diferem apenas pela
//! renomeação de variáveis ligadas possuem a mesma representação nameless.
//!
//! A substituição e a redução beta devem respeitar a profundidade dos
//! binders. Este módulo oferece operações de shifting para esse propósito.

use std::fmt;

/// Termos do lambda-cálculo na representação nameless.
///
/// `Var(usize)` armazena um índice de De Bruijn. `Abs` representa uma
/// abstração lambda e `App` representa uma aplicação.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
    /// Variável identificada por um índice de De Bruijn.
    Var(usize),

    /// Abstração lambda. O corpo é interpretado sob um binder adicional.
    Abs(Box<Term>),

    /// Aplicação de um termo a outro.
    App(Box<Term>, Box<Term>),
}

impl Term {
    /// Constrói uma variável com índice de De Bruijn.
    pub const fn var(index: usize) -> Self {
        Self::Var(index)
    }

    /// Constrói uma abstração lambda.
    ///
    /// Exemplo: `λx. x` é representado por `Term::abs(Term::var(0))`.
    pub fn abs(body: Term) -> Self {
        Self::Abs(Box::new(body))
    }

    /// Constrói uma aplicação.
    ///
    /// Exemplo: `(λx. x) y`.
    pub fn app(function: Term, argument: Term) -> Self {
        Self::App(Box::new(function), Box::new(argument))
    }

    /// Constrói a identidade: `λ. 0`.
    pub fn identity() -> Self {
        Self::abs(Self::var(0))
    }

    /// Retorna `true` se o termo for uma variável.
    pub const fn is_variable(&self) -> bool {
        matches!(self, Self::Var(_))
    }

    /// Retorna `true` se o termo for uma abstração.
    pub const fn is_abstraction(&self) -> bool {
        matches!(self, Self::Abs(_))
    }

    /// Retorna `true` se o termo for uma aplicação.
    pub const fn is_application(&self) -> bool {
        matches!(self, Self::App(_, _))
    }

    /// Retorna `true` se o termo for uma abstração lambda.
    ///
    /// No cálculo lambda puro, as abstrações são os valores da semântica
    /// operacional de ordem normal.
    pub const fn is_value(&self) -> bool {
        self.is_abstraction()
    }

    /// Retorna o corpo da abstração, se o termo for `Abs`.
    pub fn abstraction_body(&self) -> Option<&Term> {
        match self {
            Self::Abs(body) => Some(body),
            _ => None,
        }
    }

    /// Retorna a função e o argumento, se o termo for `App`.
    pub fn application_parts(&self) -> Option<(&Term, &Term)> {
        match self {
            Self::App(function, argument) => Some((function, argument)),
            _ => None,
        }
    }

    /// Calcula a profundidade máxima de abstrações que contém.
    ///
    /// Para `λ. λ. (1 0)`, retorna 2.
    ///
    /// Este método descreve a profundidade estrutural máxima de abstrações,
    /// não a quantidade de variáveis livres.
    pub fn abstraction_depth(&self) -> usize {
        fn depth(term: &Term, current: usize) -> usize {
            match term {
                Term::Var(_) => current,
                Term::Abs(body) => depth(body, current + 1),
                Term::App(function, argument) => {
                    depth(function, current).max(depth(argument, current))
                }
            }
        }

        depth(self, 0)
    }

    /// Verifica se todas as variáveis do termo estão ligadas.
    ///
    /// `depth` indica quantos binders estão disponíveis no contexto atual.
    ///
    /// Por exemplo:
    ///
    /// - `λ. 0` é fechado.
    /// - `λ. 1` não é fechado.
    /// - `λ. (0 1)` não é fechado.
    pub fn is_closed(&self) -> bool {
        fn go(term: &Term, depth: usize) -> bool {
            match term {
                Term::Var(index) => *index < depth,
                Term::Abs(body) => go(body, depth + 1),
                Term::App(function, argument) => {
                    go(function, depth) && go(argument, depth)
                }
            }
        }

        go(self, 0)
    }

    /// Desloca os índices de De Bruijn maiores ou iguais a `cutoff`
    /// por `delta`.
    ///
    /// `cutoff` indica o limite inferior dos índices que devem ser afetados,
    /// considerando a profundidade dos binders.
    ///
    /// O deslocamento positivo é útil ao inserir um termo em um contexto
    /// com novos binders. O deslocamento negativo remove níveis do contexto,
    /// desde que nenhum índice afetado fique negativo.
    ///
    /// Retorna `None` se um deslocamento negativo tornar algum índice inválido.
    pub fn shift(&self, delta: isize, cutoff: usize) -> Option<Self> {
        fn go(term: &Term, delta: isize, cutoff: usize) -> Option<Term> {
            match term {
                Term::Var(index) => {
                    if *index < cutoff {
                        Some(Term::Var(*index))
                    } else {
                        let shifted = (*index as i128) + (delta as i128);

                        if shifted < cutoff as i128 || shifted > usize::MAX as i128 {
                            return None;
                        }

                        Some(Term::Var(shifted as usize))
                    }
                }

                Term::Abs(body) => {
                    Some(Term::abs(go(body, delta, cutoff + 1)?))
                }

                Term::App(function, argument) => Some(Term::app(
                    go(function, delta, cutoff)?,
                    go(argument, delta, cutoff)?,
                )),
            }
        }

        go(self, delta, cutoff)
    }

    /// Retorna o maior índice de variável livre, se houver.
    ///
    /// O resultado é calculado relativamente ao contexto de binders:
    /// índices abaixo da profundidade atual representam variáveis ligadas;
    /// índices acima dela representam variáveis livres.
    ///
    /// O índice retornado é relativo ao contexto externo do termo.
    pub fn max_free_variable(&self) -> Option<usize> {
        fn go(term: &Term, depth: usize) -> Option<usize> {
            match term {
                Term::Var(index) => {
                    if *index >= depth {
                        Some(index - depth)
                    } else {
                        None
                    }
                }

                Term::Abs(body) => go(body, depth + 1),

                Term::App(function, argument) => {
                    match (go(function, depth), go(argument, depth)) {
                        (Some(a), Some(b)) => Some(a.max(b)),
                        (Some(a), None) => Some(a),
                        (None, Some(b)) => Some(b),
                        (None, None) => None,
                    }
                }
            }
        }

        go(self, 0)
    }
}

/// Formata um termo usando uma notação nameless explícita.
///
/// A notação é:
///
/// - `index` para variáveis;
/// - `λ.body` para abstrações;
/// - `(function argument)` para aplicações.
///
/// Exemplos:
///
/// ```text
/// 0
/// λ. 0
/// λ. λ. 1
/// (λ. 0 1)
/// ```
impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(index) => write!(f, "{index}"),

            Self::Abs(body) => write!(f, "λ. {body}"),

            Self::App(function, argument) => {
                write!(f, "(")?;

                match function.as_ref() {
                    Self::Abs(_) => write!(f, "{function}")?,
                    Self::App(_, _) => write!(f, "{function}")?,
                    Self::Var(_) => write!(f, "{function}")?,
                }

                write!(f, " ")?;

                match argument.as_ref() {
                    Self::Var(_) => write!(f, "{argument}")?,
                    Self::Abs(_) | Self::App(_, _) => write!(f, "{argument}")?,
                }

                write!(f, ")")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Term;

    #[test]
    fn identity_is_closed() {
        let identity = Term::identity();

        assert_eq!(identity, Term::abs(Term::var(0)));
        assert!(identity.is_closed());
        assert!(identity.is_value());
    }

    #[test]
    fn nested_abstraction_uses_de_bruijn_indices() {
        // λx. λy. x  =>  λ. λ. 1
        let term = Term::abs(Term::abs(Term::var(1)));

        assert!(term.is_closed());
        assert_eq!(term.to_string(), "λ. λ. 1");
        assert_eq!(term.abstraction_depth(), 2);
    }

    #[test]
    fn free_variable_is_detected() {
        assert!(!Term::var(0).is_closed());
        assert!(!Term::abs(Term::var(1)).is_closed());
        assert!(Term::abs(Term::var(0)).is_closed());
    }

    #[test]
    fn shifting_respects_binders() {
        // λ. 0: o índice 0 está ligado e não deve ser deslocado.
        let term = Term::abs(Term::var(0));

        assert_eq!(
            term.shift(1, 0),
            Some(Term::abs(Term::var(0)))
        );
    }

    #[test]
    fn shifting_updates_free_variables() {
        // λ. 1: o índice 1 é livre relativamente ao binder interno.
        let term = Term::abs(Term::var(1));

        assert_eq!(
            term.shift(1, 0),
            Some(Term::abs(Term::var(2)))
        );
    }

    #[test]
    fn negative_shift_rejects_invalid_indices() {
        assert_eq!(Term::var(0).shift(-1, 0), None);
    }

    #[test]
    fn application_is_formatted() {
        let term = Term::app(Term::identity(), Term::var(0));

        assert_eq!(term.to_string(), "(λ. 0 0)");
    }

    #[test]
    fn max_free_variable_respects_binders() {
        // λ. (1 0): somente o índice 1 é livre,
        // e seu índice relativo ao contexto externo é 0.
        let term = Term::abs(Term::app(Term::var(1), Term::var(0)));

        assert_eq!(term.max_free_variable(), Some(0));
        assert!(!term.is_closed());
    }
}