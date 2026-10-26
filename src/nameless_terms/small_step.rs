//! Semântica estrutural de `nameless_terms`: três estratégias de avaliação.
//!
//! Os termos usam índices de De Bruijn.
//!
//! Call-by-value:
//!
//!   t1 → t1'                 t1 t2 → t1' t2
//!   t2 → t2'                 v1 t2 → v1 t2'
//!   (λ. t12) v2 → t12[v2/0]
//!
//! Call-by-name:
//!
//!   t1 → t1'                 t1 t2 → t1' t2
//!   (λ. t12) t2 → t12[t2/0]
//!
//! Ordem normal:
//!
//!   a1 → a1'                 a1 t2 → a1' t2
//!   t2 → t2'                 n1 t2 → n1 t2'
//!   (λ. t12) t2 → t12[t2/0]
//!   t → t'                   λ. t → λ. t'
//!
//! A substituição é feita por `substitute_top`, que ajusta os índices de
//! De Bruijn para preservar o significado das variáveis sob abstrações.

use crate::common::semantics::{Step, Transition};

use super::substitution::substitute_top;
use super::terms::Term;

crate::rules! {
    pub enum ByValueRule {
        App1 => "E-App1" {
            [r"t_1 \to t_1'"] => r"t_1\ t_2 \to t_1'\ t_2"
        },
        App2 => "E-App2" {
            [r"t_2 \to t_2'"] => r"v_1\ t_2 \to v_1\ t_2'"
        },
        AppAbs => "E-AppAbs" {
            [] => r"(\lambda.\, t_{12})\ v_2 \to t_{12}[v_2/0]"
        },
    }
}

crate::rules! {
    pub enum ByNameRule {
        App1 => "E-App1" {
            [r"t_1 \to t_1'"] => r"t_1\ t_2 \to t_1'\ t_2"
        },
        AppAbs => "E-AppAbs" {
            [] => r"(\lambda.\, t_{12})\ t_2 \to t_{12}[t_2/0]"
        },
    }
}

crate::rules! {
    pub enum NormalOrderRule {
        App1 => "E-App1" {
            [r"a_1 \to a_1'"] => r"a_1\ t_2 \to a_1'\ t_2"
        },
        App2 => "E-App2" {
            [r"t_2 \to t_2'"] => r"n_1\ t_2 \to n_1\ t_2'"
        },
        AppAbs => "E-AppAbs" {
            [] => r"(\lambda.\, t_{12})\ t_2 \to t_{12}[t_2/0]"
        },
        Abs => "E-Abs" {
            [r"t \to t'"] => r"\lambda.\, t \to \lambda.\, t'"
        },
    }
}

/// Regra aplicada durante a redução.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Kind {
    App1,
    App2,
    AppAbs,
    Abs,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Value,
    Name,
    Normal,
}

/// Executa um único passo de redução, sem construir a derivação.
///
/// Retorna `None` se o termo não puder ser reduzido pela estratégia
/// selecionada ou se a substituição de índices falhar.
fn reduce(mode: Mode, term: &Term) -> Option<(Kind, Term)> {
    match term {
        Term::Var(_) => None,

        // Somente a ordem normal reduz sob abstrações.
        Term::Abs(body) => {
            if mode != Mode::Normal {
                return None;
            }

            let (_, reduced_body) = reduce(mode, body)?;

            Some((
                Kind::Abs,
                Term::abs(reduced_body),
            ))
        }

        Term::App(func, arg) => match mode {
            Mode::Value => {
                // E-App1: primeiro avalia a função.
                if !func.is_value() {
                    let (_, reduced_func) = reduce(mode, func)?;

                    return Some((
                        Kind::App1,
                        Term::app(reduced_func, (**arg).clone()),
                    ));
                }

                // E-App2: depois avalia o argumento.
                if !arg.is_value() {
                    let (_, reduced_arg) = reduce(mode, arg)?;

                    return Some((
                        Kind::App2,
                        Term::app((**func).clone(), reduced_arg),
                    ));
                }

                // E-AppAbs: função e argumento são valores.
                match func.as_ref() {
                    Term::Abs(body) => {
                        let result = substitute_top(body, arg)?;

                        Some((Kind::AppAbs, result))
                    }
                    _ => None,
                }
            }

            Mode::Name | Mode::Normal => {
                // E-AppAbs tem prioridade: não se avalia o argumento antes
                // de substituir a variável ligada.
                if let Term::Abs(body) = func.as_ref() {
                    let result = substitute_top(body, arg)?;

                    return Some((Kind::AppAbs, result));
                }

                // E-App1: tenta reduzir a função.
                if let Some((_, reduced_func)) = reduce(mode, func) {
                    return Some((
                        Kind::App1,
                        Term::app(reduced_func, (**arg).clone()),
                    ));
                }

                // Call-by-name não reduz o argumento de uma função neutra.
                //
                // A ordem normal, por outro lado, continua pelo argumento
                // para encontrar redexes mais à direita.
                if mode == Mode::Normal {
                    let (_, reduced_arg) = reduce(mode, arg)?;

                    return Some((
                        Kind::App2,
                        Term::app((**func).clone(), reduced_arg),
                    ));
                }

                None
            }
        },
    }
}

// -----------------------------------------------------------------------------
// Call-by-value
// -----------------------------------------------------------------------------

pub struct CallByValue;

impl Step for CallByValue {
    type State = Term;
    type Rule = ByValueRule;

    fn is_final(term: &Term) -> bool {
        // Uma abstração é um valor; uma variável ou aplicação travada não é.
        term.is_value()
    }

    fn step(term: &Term) -> Option<Transition<ByValueRule, Term>> {
        let (kind, next) = reduce(Mode::Value, term)?;

        let rule = match kind {
            Kind::App1 => ByValueRule::App1,
            Kind::App2 => ByValueRule::App2,
            Kind::AppAbs => ByValueRule::AppAbs,

            // Não existe redução sob abstrações em call-by-value.
            Kind::Abs => return None,
        };

        Some(Transition::new(rule, term.clone(), next))
    }
}

// -----------------------------------------------------------------------------
// Call-by-name
// -----------------------------------------------------------------------------

pub struct CallByName;

impl Step for CallByName {
    type State = Term;
    type Rule = ByNameRule;

    fn is_final(term: &Term) -> bool {
        term.is_value()
    }

    fn step(term: &Term) -> Option<Transition<ByNameRule, Term>> {
        let (kind, next) = reduce(Mode::Name, term)?;

        let rule = match kind {
            Kind::App1 => ByNameRule::App1,
            Kind::AppAbs => ByNameRule::AppAbs,

            // Essas regras não são produzidas pelo modo Name.
            Kind::App2 | Kind::Abs => return None,
        };

        Some(Transition::new(rule, term.clone(), next))
    }
}

// -----------------------------------------------------------------------------
// Ordem normal
// -----------------------------------------------------------------------------

pub struct NormalOrder;

impl Step for NormalOrder {
    type State = Term;
    type Rule = NormalOrderRule;

    /// Uma forma normal não admite nenhum passo de redução.
    fn is_final(term: &Term) -> bool {
        reduce(Mode::Normal, term).is_none()
    }

    fn step(term: &Term) -> Option<Transition<NormalOrderRule, Term>> {
        let (kind, next) = reduce(Mode::Normal, term)?;

        let rule = match kind {
            Kind::App1 => NormalOrderRule::App1,
            Kind::App2 => NormalOrderRule::App2,
            Kind::AppAbs => NormalOrderRule::AppAbs,
            Kind::Abs => NormalOrderRule::Abs,
        };

        Some(Transition::new(rule, term.clone(), next))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::semantics::Step;

    fn identity() -> Term {
        Term::abs(Term::var(0))
    }

    fn omega() -> Term {
        // (λ. 0 0) (λ. 0 0)
        let delta = Term::abs(Term::app(
            Term::var(0),
            Term::var(0),
        ));

        Term::app(delta.clone(), delta)
    }

    #[test]
    fn call_by_value_reduces_identity_application() {
        let term = Term::app(identity(), identity());

        let transition = CallByValue::step(&term).unwrap();

        assert_eq!(transition.to, identity());
    }

    #[test]
    fn call_by_name_does_not_evaluate_argument_first() {
        // (λ. 0) Ω → λ. 0
        let term = Term::app(identity(), omega());

        let transition = CallByName::step(&term).unwrap();

        assert_eq!(transition.to, identity());
    }

    #[test]
    fn call_by_value_gets_stuck_on_diverging_argument() {
        // (λ. 0) Ω não consegue reduzir o argumento até um valor.
        let term = Term::app(identity(), omega());

        assert!(CallByValue::step(&term).is_none());
        assert!(!CallByValue::is_final(&term));
    }

    #[test]
    fn normal_order_reduces_under_abstraction() {
        // λ. ((λ. 0) 0) → λ. 0
        let term = Term::abs(Term::app(
            identity(),
            Term::var(0),
        ));

        let transition = NormalOrder::step(&term).unwrap();

        assert_eq!(transition.to, identity());
    }

    #[test]
    fn normal_order_recognizes_abstraction_as_normal_form() {
        assert!(NormalOrder::is_final(&identity()));
    }

    #[test]
    fn call_by_name_does_not_reduce_under_abstraction() {
        let term = Term::abs(Term::app(
            identity(),
            Term::var(0),
        ));

        assert!(CallByName::step(&term).is_none());
        assert!(CallByName::is_final(&term));
    }
}