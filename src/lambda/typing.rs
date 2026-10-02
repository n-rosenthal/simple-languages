//! Tipagem de `lambda` (STLC): o julgamento `Γ ⊢ t : T`.

use std::fmt;

use crate::common::semantics::{Derivation, Typed, Typing, TypingDerivation};
use crate::common::Context;

use super::terms::Term;
use super::types::Type;

crate::rules! {
    pub enum TypingRule {
        /// x:T ∈ Γ  ⟹  Γ ⊢ x : T
        Var => "T-Var",
        /// Γ, x:T1 ⊢ t : T2  ⟹  Γ ⊢ λx:T1. t : T1 → T2
        Abs => "T-Abs",
        /// Γ ⊢ t1 : T1 → T2,  Γ ⊢ t2 : T1  ⟹  Γ ⊢ t1 t2 : T2
        App => "T-App",
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    UnboundVariable { name: String },
    NotAFunction { found: Type },
    ArgumentMismatch { expected: Type, found: Type },
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnboundVariable { name } => write!(f, "unbound variable `{name}`"),
            Self::NotAFunction { found } => {
                write!(f, "cannot apply a term of type `{found}`; expected a function type")
            }
            Self::ArgumentMismatch { expected, found } => write!(
                f,
                "argument has type `{found}`, but the function expects `{expected}`"
            ),
        }
    }
}

impl std::error::Error for TypeError {}

pub struct LambdaTyping;

impl LambdaTyping {
    /// Deriva `Γ ⊢ t : T` para um Γ qualquer. O contexto é persistente:
    /// estender Γ não altera o contexto do chamador, então não há nada
    /// para restaurar quando um `?` interrompe a checagem.
    pub fn check_in(
        ctx: &Context<Type>,
        term: &Term,
    ) -> Result<TypingDerivation<Self>, TypeError> {
        match term {
            // T-Var
            Term::Var(name) => {
                let ty = ctx
                    .lookup(name)
                    .cloned()
                    .ok_or_else(|| TypeError::UnboundVariable { name: name.clone() })?;

                Ok(Derivation::axiom(
                    Typed::new(ctx.clone(), term.clone(), ty),
                    TypingRule::Var,
                ))
            }

            // T-Abs
            Term::Lambda { param, ty, body } => {
                let inner = ctx.extend(param.clone(), ty.clone());
                let body_derivation = Self::check_in(&inner, body)?;
                let result = Type::arrow(ty.clone(), body_derivation.conclusion.ty.clone());

                Ok(Derivation::node(
                    Typed::new(ctx.clone(), term.clone(), result),
                    TypingRule::Abs,
                    vec![body_derivation],
                ))
            }

            // T-App
            Term::App { func, arg } => {
                let func_derivation = Self::check_in(ctx, func)?;
                let arg_derivation = Self::check_in(ctx, arg)?;

                let (from, to) = match &func_derivation.conclusion.ty {
                    Type::Arrow(from, to) => ((**from).clone(), (**to).clone()),
                    found => return Err(TypeError::NotAFunction { found: found.clone() }),
                };

                if from != arg_derivation.conclusion.ty {
                    return Err(TypeError::ArgumentMismatch {
                        expected: from,
                        found: arg_derivation.conclusion.ty.clone(),
                    });
                }

                Ok(Derivation::node(
                    Typed::new(ctx.clone(), term.clone(), to),
                    TypingRule::App,
                    vec![func_derivation, arg_derivation],
                ))
            }
        }
    }
}

impl Typing for LambdaTyping {
    type Term = Term;
    type Type = Type;
    type Rule = TypingRule;
    type Error = TypeError;

    fn check(term: &Term) -> Result<TypingDerivation<Self>, TypeError> {
        Self::check_in(&Context::empty(), term)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lambda::testing::parse;

    fn ty(source: &str) -> Result<Type, TypeError> {
        LambdaTyping::type_of(&parse(source))
    }

    fn base(name: &str) -> Type {
        Type::base(name)
    }

    #[test]
    fn identity_has_an_arrow_type() {
        assert_eq!(ty("λx:Bool. x"), Ok(Type::arrow(base("Bool"), base("Bool"))));
    }

    #[test]
    fn curried_constant_function() {
        assert_eq!(
            ty("λx:A. λy:B. x"),
            Ok(Type::arrow(base("A"), Type::arrow(base("B"), base("A"))))
        );
    }

    #[test]
    fn application() {
        assert_eq!(ty("(λf:A->A. f) (λy:A. y)"), Ok(Type::arrow(base("A"), base("A"))));
    }

    #[test]
    fn unbound_variable() {
        assert_eq!(ty("z"), Err(TypeError::UnboundVariable { name: "z".into() }));
    }

    #[test]
    fn applying_a_non_function() {
        assert!(matches!(ty("λx:A. x x"), Err(TypeError::NotAFunction { .. })));
    }

    #[test]
    fn argument_mismatch() {
        assert_eq!(
            ty("(λx:A. x) (λy:A. y)"),
            Err(TypeError::ArgumentMismatch {
                expected: base("A"),
                found: Type::arrow(base("A"), base("A")),
            })
        );
    }

    #[test]
    fn inner_binding_shadows_outer() {
        assert_eq!(
            ty("λx:A. λx:B. x"),
            Ok(Type::arrow(base("A"), Type::arrow(base("B"), base("B"))))
        );
    }

    #[test]
    fn context_is_restored_after_abstraction() {
        // (λx:Bool. x) x  → o segundo x é livre
        assert_eq!(
            ty("(λx:Bool. x) x"),
            Err(TypeError::UnboundVariable { name: "x".into() })
        );
    }

    #[test]
    fn the_derivation_is_a_tree() {
        let d = LambdaTyping::check(&parse("λx:A. x")).unwrap();

        assert_eq!(d.size(), 2);
        assert_eq!(d.postorder_rules(), vec![TypingRule::Var, TypingRule::Abs]);
    }

    #[test]
    fn premises_carry_their_context() {
        let d = LambdaTyping::check(&parse("λx:A. x")).unwrap();

        assert_eq!(d.conclusion.to_string(), "⊢ λx:A. x : A->A");
        assert_eq!(d.premises[0].conclusion.to_string(), "x:A ⊢ x : A");
    }

    #[test]
    fn application_has_two_premises() {
        let d = LambdaTyping::check(&parse("(λf:A->A. f) (λy:A. y)")).unwrap();

        assert_eq!(d.premises.len(), 2);
        assert_eq!(d.rule, TypingRule::App);
    }
}
