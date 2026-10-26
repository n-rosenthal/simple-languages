//! Substituição de variáveis e redução beta para termos nameless.
//!
//! Inspirado em *Types and Programming Languages*, de Benjamin C. Pierce.
//!
//! A substituição respeita os índices de De Bruijn e ajusta o termo
//! substituído ao atravessar abstrações.
//!
//! A redução beta segue a estratégia clássica:
//!
//!     (λ. body) argument
//!       -> shift(-1, substitute(0, shift(1, argument), body))
//!
//! O deslocamento positivo prepara o argumento para ser inserido no corpo
//! da abstração; o deslocamento negativo remove o nível do binder eliminado.

use super::shifting::shift;
use super::terms::Term;

/// Substitui as ocorrências do índice `index` por `replacement`.
///
/// A busca considera a profundidade das abstrações:
///
/// - uma variável igual ao índice procurado é substituída;
/// - ao entrar em uma abstração, o índice procurado aumenta em um;
/// - o termo substituído também é deslocado para preservar suas ligações.
///
/// `index` é relativo ao contexto externo do termo.
///
/// Retorna `None` se um deslocamento necessário for inválido.
pub fn substitute(
    term: &Term,
    index: usize,
    replacement: &Term,
) -> Option<Term> {
    fn go(
        term: &Term,
        index: usize,
        replacement: &Term,
    ) -> Option<Term> {
        match term {
            Term::Var(variable) => {
                if *variable == index {
                    Some(replacement.clone())
                } else {
                    Some(Term::var(*variable))
                }
            }

            Term::Abs(body) => {
                let shifted_replacement = shift(replacement, 1, 0)?;

                Some(Term::abs(go(
                    body,
                    index.checked_add(1)?,
                    &shifted_replacement,
                )?))
            }

            Term::App(function, argument) => Some(Term::app(
                go(function, index, replacement)?,
                go(argument, index, replacement)?,
            )),
        }
    }

    go(term, index, replacement)
}

/// Substitui a variável ligada pela abstração eliminada.
///
/// Esta operação implementa a substituição usada na regra beta:
///
///     (λ. body) argument
///       -> shift(-1, substitute(0, shift(1, argument), body))
///
/// O resultado é o corpo da abstração com o argumento inserido e o nível
/// do binder removido.
///
/// Retorna `None` se algum deslocamento for inválido.
pub fn substitute_top(body: &Term, argument: &Term) -> Option<Term> {
    let shifted_argument = shift(argument, 1, 0)?;
    let substituted = substitute(body, 0, &shifted_argument)?;

    shift(&substituted, -1, 0)
}

/// Realiza uma única redução beta.
///
/// Se `term` for uma aplicação cuja função é uma abstração,
/// retorna o resultado da substituição.
///
/// Para outros formatos, retorna `None`.
///
/// A função não procura redexes em subtermos: essa busca pertence
/// à estratégia de avaliação implementada em `small_step.rs`.
pub fn beta_reduce(term: &Term) -> Option<Term> {
    match term {
        Term::App(function, argument) => match function.as_ref() {
            Term::Abs(body) => substitute_top(body, argument),
            _ => None,
        },

        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn substitutes_identity_body() {
        // (λ. 0) (λ. 0) -> λ. 0
        let body = Term::var(0);
        let argument = Term::identity();

        assert_eq!(
            substitute_top(&body, &argument),
            Some(Term::identity())
        );
    }

    #[test]
    fn beta_reduces_identity_application() {
        let term = Term::app(
            Term::identity(),
            Term::identity(),
        );

        assert_eq!(beta_reduce(&term), Some(Term::identity()));
    }

    #[test]
    fn substitution_respects_nested_binders() {
        // λ. 1: substituímos o índice 0 no contexto externo.
        //
        // O índice 1 dentro da abstração corresponde à variável
        // procurada, que é substituída pelo argumento.
        let body = Term::abs(Term::var(1));
        let replacement = Term::var(0);

        assert_eq!(
            substitute(&body, 0, &replacement),
            Some(Term::abs(Term::var(1)))
        );
    }

    #[test]
    fn beta_preserves_outer_binding() {
        // λ. ((λ. 1) 0)
        //
        // A aplicação interna retorna a variável ligada pela abstração
        // externa. A forma normal resultante é λ. 0.
        let term = Term::abs(Term::app(
            Term::abs(Term::var(1)),
            Term::var(0),
        ));

        let inner_redex = match &term {
            Term::Abs(body) => body.as_ref(),
            _ => unreachable!(),
        };

        assert_eq!(
            beta_reduce(inner_redex),
            Some(Term::var(0))
        );
    }

    #[test]
    fn beta_reduces_constant_function() {
        // (λ. λ. 1) (λ. 0) -> λ. 0
        let term = Term::app(
            Term::abs(Term::abs(Term::var(1))),
            Term::identity(),
        );

        assert_eq!(beta_reduce(&term), Some(Term::identity()));
    }

    #[test]
    fn beta_reduces_function_returning_argument() {
        // (λ. λ. 0) (λ. 0) -> λ. 0
        let term = Term::app(
            Term::abs(Term::abs(Term::var(0))),
            Term::identity(),
        );

        assert_eq!(
            beta_reduce(&term),
            Some(Term::identity())
        );
    }

    #[test]
    fn does_not_reduce_non_redex() {
        assert_eq!(beta_reduce(&Term::var(0)), None);
        assert_eq!(
            beta_reduce(&Term::app(
                Term::var(0),
                Term::identity(),
            )),
            None
        );
    }

    #[test]
    fn rejects_invalid_substitution_shift() {
        // Uma substituição que exigisse um deslocamento impossível
        // deve falhar explicitamente.
        let body = Term::var(0);
        let replacement = Term::var(usize::MAX);

        assert_eq!(substitute_top(&body, &replacement), None);
    }
}