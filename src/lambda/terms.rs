//! Termos de `lambda` (TAPL, fig. 5-3): `t ::= x | λx. t | t t`.

use std::collections::BTreeSet;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    /// `x`
    Var(String),
    /// `λx. body`
    Lambda { param: String, body: Box<Term> },
    /// `f x`
    App { func: Box<Term>, arg: Box<Term> },
}

impl Term {
    pub fn variable(name: impl Into<String>) -> Self {
        Self::Var(name.into())
    }

    pub fn lambda(param: impl Into<String>, body: Term) -> Self {
        Self::Lambda { param: param.into(), body: Box::new(body) }
    }

    pub fn app(func: Term, arg: Term) -> Self {
        Self::App { func: Box::new(func), arg: Box::new(arg) }
    }

    /// Os valores do call-by-value e do call-by-name: as abstrações.
    pub fn is_value(&self) -> bool {
        matches!(self, Term::Lambda { .. })
    }

    /// Variáveis livres.
    pub fn free_vars(&self) -> BTreeSet<String> {
        match self {
            Term::Var(x) => BTreeSet::from([x.clone()]),
            Term::Lambda { param, body } => {
                let mut vars = body.free_vars();
                vars.remove(param);
                vars
            }
            Term::App { func, arg } => {
                let mut vars = func.free_vars();
                vars.extend(arg.free_vars());
                vars
            }
        }
    }

    /// `[name ↦ replacement] self`, evitando captura: se um binder
    /// capturaria uma variável livre de `replacement`, ele é renomeado.
    pub fn substitute(&self, name: &str, replacement: &Term) -> Term {
        match self {
            Term::Var(x) if x == name => replacement.clone(),
            Term::Var(_) => self.clone(),
            Term::App { func, arg } => Term::app(
                func.substitute(name, replacement),
                arg.substitute(name, replacement),
            ),
            Term::Lambda { param, body } => {
                if param == name || !body.free_vars().contains(name) {
                    return self.clone();
                }
                let replacement_vars = replacement.free_vars();
                if replacement_vars.contains(param) {
                    let mut avoid = replacement_vars;
                    avoid.extend(body.free_vars());
                    avoid.insert(name.to_string());
                    let fresh = fresh_name(param, &avoid);
                    let renamed = body.substitute(param, &Term::variable(fresh.clone()));
                    Term::lambda(fresh, renamed.substitute(name, replacement))
                } else {
                    Term::lambda(param.clone(), body.substitute(name, replacement))
                }
            }
        }
    }

    /// Igualdade módulo renomeação de variáveis ligadas (α-equivalência).
    pub fn alpha_eq(&self, other: &Term) -> bool {
        fn go(a: &Term, b: &Term, env_a: &mut Vec<String>, env_b: &mut Vec<String>) -> bool {
            match (a, b) {
                (Term::Var(x), Term::Var(y)) => {
                    let i = env_a.iter().rposition(|n| n == x);
                    let j = env_b.iter().rposition(|n| n == y);
                    match (i, j) {
                        (Some(i), Some(j)) => i == j,
                        (None, None) => x == y,
                        _ => false,
                    }
                }
                (Term::Lambda { param: p, body: bp }, Term::Lambda { param: q, body: bq }) => {
                    env_a.push(p.clone());
                    env_b.push(q.clone());
                    let equal = go(bp, bq, env_a, env_b);
                    env_a.pop();
                    env_b.pop();
                    equal
                }
                (Term::App { func: f1, arg: a1 }, Term::App { func: f2, arg: a2 }) => {
                    go(f1, f2, env_a, env_b) && go(a1, a2, env_a, env_b)
                }
                _ => false,
            }
        }
        go(self, other, &mut Vec::new(), &mut Vec::new())
    }
}

/// `base1`, `base2`, ... o primeiro que não está em `avoid`.
fn fresh_name(base: &str, avoid: &BTreeSet<String>) -> String {
    (1..)
        .map(|n| format!("{base}{n}"))
        .find(|candidate| !avoid.contains(candidate))
        .expect("infinite iterator")
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(x) => write!(f, "{x}"),
            Self::Lambda { param, body } => write!(f, "λ{param}. {body}"),
            Self::App { func, arg } => {
                // só uma abstração à esquerda precisa de parênteses (ela se
                // estende o mais à direita possível)
                match **func {
                    Term::Lambda { .. } => write!(f, "({func})")?,
                    _ => write!(f, "{func}")?,
                }
                write!(f, " ")?;
                match **arg {
                    Term::Var(_) => write!(f, "{arg}"),
                    _ => write!(f, "({arg})"),
                }
            }
        }
    }
}
