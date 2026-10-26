//! Deslocamento de índices de De Bruijn.
//!
//! Inspirado em *Types and Programming Languages*, de Benjamin C. Pierce.
//!
//! O deslocamento ajusta índices de variáveis livres relativamente a um
//! contexto de binders. Ao atravessar uma abstração, o cutoff aumenta.
//!
//! `shift(term, delta, cutoff)`:
//! - índices menores que cutoff permanecem inalterados;
//! - índices maiores ou iguais a cutoff são deslocados por delta;
//! - retorna `None` se um índice ficar negativo ou ocorrer overflow.
//!
//! O deslocamento negativo é utilizado, por exemplo, após a substituição
//! beta para remover o nível do binder eliminado.

use super::terms::Term;

/// Desloca os índices de De Bruijn de um termo.
///
/// `cutoff` é relativo à profundidade atual de abstrações.
///
/// # Exemplos
///
/// ```
/// # use simple_languages::nameless_terms::shifting::shift;
/// # use simple_languages::nameless_terms::terms::Term;
/// assert_eq!(shift(&Term::var(0), 1, 0), Some(Term::var(1)));
/// assert_eq!(shift(&Term::var(0), 1, 1), Some(Term::var(0)));
/// ```
pub fn shift(term: &Term, delta: isize, cutoff: usize) -> Option<Term> {
    match term {
        Term::Var(index) => {
            if *index < cutoff {
                return Some(Term::var(*index));
            }

            let shifted = (*index as i128) + (delta as i128);

            if shifted < 0 || shifted > usize::MAX as i128 {
                return None;
            }

            Some(Term::var(shifted as usize))
        }

        Term::Abs(body) => {
            let next_cutoff = cutoff.checked_add(1)?;
            Some(Term::abs(shift(body, delta, next_cutoff)?))
        }

        Term::App(function, argument) => Some(Term::app(
            shift(function, delta, cutoff)?,
            shift(argument, delta, cutoff)?,
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn shifts_variable() {
        assert_eq!(shift(&Term::var(0), 1, 0), Some(Term::var(1)));
        assert_eq!(shift(&Term::var(2), 3, 0), Some(Term::var(5)));
    }

    #[test]
    fn preserves_indices_below_cutoff() {
        assert_eq!(shift(&Term::var(0), 1, 1), Some(Term::var(0)));
        assert_eq!(shift(&Term::var(1), 1, 2), Some(Term::var(1)));
    }

    #[test]
    fn increases_cutoff_under_abstraction() {
        // λ. 0: o índice 0 é ligado e não muda.
        let term = Term::abs(Term::var(0));

        assert_eq!(shift(&term, 1, 0), Some(term));
    }

    #[test]
    fn shifts_free_variable_under_abstraction() {
        // λ. 1: o índice 1 refere-se a uma variável externa.
        let term = Term::abs(Term::var(1));

        assert_eq!(
            shift(&term, 1, 0),
            Some(Term::abs(Term::var(2)))
        );
    }

    #[test]
    fn negative_shift_rejects_negative_index() {
        assert_eq!(shift(&Term::var(0), -1, 0), None);
    }

    #[test]
    fn negative_shift_can_remove_outer_context_level() {
        // O índice 1, ao remover um nível do contexto externo,
        // passa a ser o índice 0.
        assert_eq!(shift(&Term::var(1), -1, 0), Some(Term::var(0)));
    }

    #[test]
    fn shifts_both_sides_of_application() {
        let term = Term::app(Term::var(0), Term::var(1));

        assert_eq!(
            shift(&term, 1, 0),
            Some(Term::app(Term::var(1), Term::var(2)))
        );
    }

    #[test]
    fn rejects_underflow_inside_application() {
        let term = Term::app(Term::var(0), Term::var(1));

        assert_eq!(shift(&term, -1, 0), None);
    }
}