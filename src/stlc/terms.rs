use std::collections::BTreeSet;
use std::fmt;

use super::types::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    /// `x`
    Var(String),
    /// `true`
    True,
    /// `false`
    False,
    /// `if c then t else e`
    If { condition: Box<Term>, then_branch: Box<Term>, else_branch: Box<Term> },
    /// `λx:T. body`
    Lambda { param: String, ty: Type, body: Box<Term> },
    /// `f x`
    App { func: Box<Term>, arg: Box<Term> },
}

impl Term {
    pub fn variable(name: impl Into<String>) -> Self {
        Self::Var(name.into())
    }

    pub fn boolean(value: bool) -> Self {
        if value {
            Self::True
        } else {
            Self::False
        }
    }

    pub fn if_then_else(condition: Term, then_branch: Term, else_branch: Term) -> Self {
        Self::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        }
    }

    pub fn lambda(param: impl Into<String>, ty: Type, body: Term) -> Self {
        Self::Lambda { param: param.into(), ty, body: Box::new(body) }
    }

    pub fn app(func: Term, arg: Term) -> Self {
        Self::App { func: Box::new(func), arg: Box::new(arg) }
    }

    /// Valores: `true`, `false` e as abstrações (TAPL, cap. 9).
    pub fn is_value(&self) -> bool {
        matches!(self, Term::True | Term::False | Term::Lambda { .. })
    }

    /// Variáveis livres.
    pub fn free_vars(&self) -> BTreeSet<String> {
        match self {
            Term::Var(x) => BTreeSet::from([x.clone()]),
            Term::True | Term::False => BTreeSet::new(),
            Term::If { condition, then_branch, else_branch } => {
                let mut vars = condition.free_vars();
                vars.extend(then_branch.free_vars());
                vars.extend(else_branch.free_vars());
                vars
            }
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
            Term::Var(_) | Term::True | Term::False => self.clone(),

            Term::If { condition, then_branch, else_branch } => Term::if_then_else(
                condition.substitute(name, replacement),
                then_branch.substitute(name, replacement),
                else_branch.substitute(name, replacement),
            ),

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
                (Term::True, Term::True) | (Term::False, Term::False) => true,
                (
                    Term::If { condition: c1, then_branch: t1, else_branch: e1 },
                    Term::If { condition: c2, then_branch: t2, else_branch: e2 },
                ) => {
                    go(c1, c2, env_a, env_b) && go(t1, t2, env_a, env_b) && go(e1, e2, env_a, env_b)
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

/// Átomos não precisam de parênteses como argumento de uma aplicação.
fn is_atom(term: &Term) -> bool {
    matches!(term, Term::Var(_) | Term::True | Term::False)
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(x) => write!(f, "{x}"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
            Self::If { condition, then_branch, else_branch } => {
                write!(f, "if {condition} then {then_branch} else {else_branch}")
            }
            Self::Lambda { param, ty, body } => write!(f, "λ{param}:{ty}. {body}"),
            Self::App { func, arg } => {
                // Aplicação é associativa à esquerda: só uma abstração ou um
                // `if` à esquerda precisam de parênteses (os dois se estendem
                // o mais à direita possível).
                match **func {
                    Term::Lambda { .. } | Term::If { .. } => write!(f, "({func})")?,
                    _ => write!(f, "{func}")?,
                }
                write!(f, " ")?;
                if is_atom(arg) {
                    write!(f, "{arg}")
                } else {
                    write!(f, "({arg})")
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::stlc::testing::parse;

    #[test]
    fn free_variables() {
        assert!(parse("λx:A. x").free_vars().is_empty());
        let vars: Vec<_> = parse("λx:A. f x y").free_vars().into_iter().collect();
        assert_eq!(vars, vec!["f", "y"]);
    }

    #[test]
    fn free_variables_of_conditionals() {
        let vars: Vec<_> = parse("if a then b else λx:Bool. x c").free_vars().into_iter().collect();
        assert_eq!(vars, vec!["a", "b", "c"]);
        assert!(parse("if true then false else true").free_vars().is_empty());
    }

    #[test]
    fn values_are_booleans_and_abstractions() {
        assert!(parse("true").is_value());
        assert!(parse("false").is_value());
        assert!(parse("λx:A. x").is_value());
        assert!(!parse("x").is_value());
        assert!(!parse("if true then true else false").is_value());
        assert!(!parse("(λx:A. x) y").is_value());
    }

    #[test]
    fn substitution_replaces_free_occurrences() {
        let result = parse("f x").substitute("x", &parse("λy:A. y"));
        assert_eq!(result, parse("f (λy:A. y)"));
    }

    #[test]
    fn substitution_goes_into_conditionals() {
        let result = parse("if x then x else y").substitute("x", &parse("true"));
        assert_eq!(result, parse("if true then true else y"));
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
        assert!(parse("λx:Bool. if x then x else false").alpha_eq(&parse("λy:Bool. if y then y else false")));
        assert!(!parse("true").alpha_eq(&parse("false")));
    }
}
