//! `simple-languages/lambda/terms.rs` defines the term types for the lambda calculus language. 
//! 
//! The `Term` type is defined in `simple-languages/lambda/types.rs`.
//! 
//! Author:     n-rosenthal
//! Date:       2026-10-01
//! Version:    0.1.1

use std::fmt;

use std::collections::BTreeSet;

//  ===
//  Term
//  ===

/// Terms for the simply typed lambda calculus language.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    /// Variable.
    Var(String),

    /// Lambda abstraction.
    Lambda {
        /// Parameter of the lambda abstraction.
        param: String,

        /// Body of the lambda abstraction.
        body: Box<Term>,
    },

    /// Application.
    App(Box<Term>, Box<Term>),
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(name) => write!(f, "{}", name),
            Self::Lambda { param, body } => write!(f, "λ{}.{body}", param),
            Self::App(lhs, rhs) => write!(f, "({lhs} {rhs})"),
        }
    }
}

impl Term {
    /// Valores: as abstrações.
    pub fn is_value(&self) -> bool {
        matches!(self, Term::Lambda { .. })
    }

    /// Variáveis livres.
    pub fn free_vars(&self) -> BTreeSet<String> {
        match self {
            Term::Var(x) => BTreeSet::from([x.clone()]),
            Term::Lambda { param, body, .. } => {
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

            Term::Lambda { param, ty, body } => {
                // `name` está sombreado, ou nem ocorre livre: nada a fazer.
                if param == name || !body.free_vars().contains(name) {
                    return self.clone();
                }

                let replacement_vars = replacement.free_vars();

                if replacement_vars.contains(param) {
                    // renomeia o binder para um nome que não colida com nada
                    let mut avoid = replacement_vars;
                    avoid.extend(body.free_vars());
                    avoid.insert(name.to_string());

                    let fresh = fresh_name(param, &avoid);
                    let renamed = body.substitute(param, &Term::variable(fresh.clone()));

                    Term::lambda(fresh, ty.clone(), renamed.substitute(name, replacement))
                } else {
                    Term::lambda(param.clone(), ty.clone(), body.substitute(name, replacement))
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
                (
                    Term::Lambda { param: p, ty: t, body: bp },
                    Term::Lambda { param: q, ty: u, body: bq },
                ) => {
                    if t != u {
                        return false;
                    }
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

#[cfg(test)]
mod tests {
    use crate::lambda::testing::parse;

    #[test]
    fn free_variables() {
        assert!(parse("λx:A. x").free_vars().is_empty());
        let vars: Vec<_> = parse("λx:A. f x y").free_vars().into_iter().collect();
        assert_eq!(vars, vec!["f", "y"]);
    }

    #[test]
    fn substitution_replaces_free_occurrences() {
        let result = parse("f x").substitute("x", &parse("λy:A. y"));
        assert_eq!(result, parse("f (λy:A. y)"));
    }

    #[test]
    fn substitution_respects_shadowing() {
        let term = parse("λx:A. x");
        assert_eq!(term.substitute("x", &parse("z")), term);
    }

    #[test]
    fn substitution_avoids_capture() {
        // [x ↦ y](λy:A. x)  deve ser  λy1:A. y,  não  λy:A. y
        let result = parse("λy:A. x").substitute("x", &parse("y"));

        assert_eq!(result, parse("λy1:A. y"));
        assert!(result.alpha_eq(&parse("λz:A. y")));
        assert!(!result.alpha_eq(&parse("λy:A. y")));
    }

    #[test]
    fn alpha_equivalence() {
        assert!(parse("λx:A. x").alpha_eq(&parse("λy:A. y")));
        assert!(parse("λx:A. λy:A. x").alpha_eq(&parse("λa:A. λb:A. a")));
        assert!(!parse("λx:A. λy:A. x").alpha_eq(&parse("λa:A. λb:A. b")));
        assert!(!parse("λx:A. x").alpha_eq(&parse("λx:B. x")));
        assert!(!parse("λx:A. z").alpha_eq(&parse("λx:A. w"))); // livres diferentes
    }
}

// EOF