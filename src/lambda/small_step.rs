//! Semântica estrutural de `lambda`: três estratégias (TAPL, cap. 5.3).
//!
//! **Call-by-value** (valores: as abstrações):
//!
//! ```text
//!   t1 → t1'            ⟹  t1 t2 → t1' t2            (E-App1)
//!   t2 → t2'            ⟹  v1 t2 → v1 t2'            (E-App2)
//!   (λx. t12) v2 → [x ↦ v2] t12                      (E-AppAbs)
//! ```
//!
//! **Call-by-name**: o argumento não é avaliado antes da chamada.
//!
//! ```text
//!   t1 → t1'            ⟹  t1 t2 → t1' t2            (E-App1)
//!   (λx. t12) t2 → [x ↦ t2] t12                      (E-AppAbs)
//! ```
//!
//! **Ordem normal**: o redex mais externo e mais à esquerda, inclusive sob λ.
//! Com `a` um termo que não é abstração e `n` uma forma neutra (`x` ou `n nf`):
//!
//! ```text
//!   a1 → a1'            ⟹  a1 t2 → a1' t2            (E-App1)
//!   t2 → t2'            ⟹  n1 t2 → n1 t2'            (E-App2)
//!   (λx. t12) t2 → [x ↦ t2] t12                      (E-AppAbs)
//!   t → t'              ⟹  λx. t → λx. t'            (E-Abs)
//! ```
//!
//! Uma variável livre, ou `x v`, é uma forma normal que não é valor (travada)
//! no call-by-value e no call-by-name. Na ordem normal o alvo da avaliação é a
//! forma normal, e essas são finais.

use crate::common::semantics::{Step, Transition};

use super::terms::Term;

crate::rules! {
    pub enum ByValueRule {
        App1 => "E-App1" { [r"t_1 \to t_1'"] => r"t_1\ t_2 \to t_1'\ t_2" },
        App2 => "E-App2" { [r"t_2 \to t_2'"] => r"v_1\ t_2 \to v_1\ t_2'" },
        AppAbs => "E-AppAbs" {
            [] => r"(\lambda x.\, t_{12})\ v_2 \to [x \mapsto v_2]\, t_{12}"
        },
    }
}

crate::rules! {
    pub enum ByNameRule {
        App1 => "E-App1" { [r"t_1 \to t_1'"] => r"t_1\ t_2 \to t_1'\ t_2" },
        AppAbs => "E-AppAbs" {
            [] => r"(\lambda x.\, t_{12})\ t_2 \to [x \mapsto t_2]\, t_{12}"
        },
    }
}

crate::rules! {
    pub enum NormalOrderRule {
        App1 => "E-App1" { [r"a_1 \to a_1'"] => r"a_1\ t_2 \to a_1'\ t_2" },
        App2 => "E-App2" { [r"t_2 \to t_2'"] => r"n_1\ t_2 \to n_1\ t_2'" },
        AppAbs => "E-AppAbs" {
            [] => r"(\lambda x.\, t_{12})\ t_2 \to [x \mapsto t_2]\, t_{12}"
        },
        Abs => "E-Abs" { [r"t \to t'"] => r"\lambda x.\, t \to \lambda x.\, t'" },
    }
}

/// A regra aplicada, sem nome de estratégia: cada estratégia converte para a sua.
#[derive(Clone, Copy)]
enum Kind {
    App1,
    App2,
    AppAbs,
    Abs,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Mode {
    Value,
    Name,
    Normal,
}

/// A regra mais externa e o termo seguinte, sem montar a `Transition`.
fn reduce(mode: Mode, term: &Term) -> Option<(Kind, Term)> {
    match term {
        Term::Var(_) => None,

        // só a ordem normal reduz sob λ
        Term::Lambda { param, body } => {
            if mode != Mode::Normal {
                return None;
            }
            let (_, inner) = reduce(mode, body)?;
            Some((Kind::Abs, Term::lambda(param.clone(), inner)))
        }

        Term::App { func, arg } => match mode {
            Mode::Value => {
                if !func.is_value() {
                    let (_, inner) = reduce(mode, func)?; // função travada: o termo todo trava
                    return Some((Kind::App1, Term::app(inner, (**arg).clone())));
                }
                if !arg.is_value() {
                    let (_, inner) = reduce(mode, arg)?;
                    return Some((Kind::App2, Term::app((**func).clone(), inner)));
                }
                match &**func {
                    Term::Lambda { param, body } => Some((Kind::AppAbs, body.substitute(param, arg))),
                    _ => None,
                }
            }

            Mode::Name | Mode::Normal => {
                if let Term::Lambda { param, body } = &**func {
                    return Some((Kind::AppAbs, body.substitute(param, arg)));
                }
                if let Some((_, inner)) = reduce(mode, func) {
                    return Some((Kind::App1, Term::app(inner, (**arg).clone())));
                }
                // a função é neutra: só a ordem normal continua pelo argumento
                if mode == Mode::Normal {
                    let (_, inner) = reduce(mode, arg)?;
                    return Some((Kind::App2, Term::app((**func).clone(), inner)));
                }
                None
            }
        },
    }
}

pub struct CallByValue;

impl Step for CallByValue {
    type State = Term;
    type Rule = ByValueRule;

    fn is_final(term: &Term) -> bool {
        term.is_value()
    }

    fn step(term: &Term) -> Option<Transition<ByValueRule, Term>> {
        let (kind, next) = reduce(Mode::Value, term)?;
        let rule = match kind {
            Kind::App1 => ByValueRule::App1,
            Kind::App2 => ByValueRule::App2,
            Kind::AppAbs | Kind::Abs => ByValueRule::AppAbs,
        };
        Some(Transition::new(rule, term.clone(), next))
    }
}

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
            Kind::App1 | Kind::App2 => ByNameRule::App1,
            Kind::AppAbs | Kind::Abs => ByNameRule::AppAbs,
        };
        Some(Transition::new(rule, term.clone(), next))
    }
}

pub struct NormalOrder;

impl Step for NormalOrder {
    type State = Term;
    type Rule = NormalOrderRule;

    /// Uma forma normal: nenhuma regra se aplica.
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
