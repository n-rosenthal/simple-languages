//! `simple-languages/lambda/terms.rs` defines the term types for the lambda calculus language. 
//! 
//! The `Term` type is defined in `simple-languages/lambda/types.rs`.
//! 
//! Author:     n-rosenthal
//! Date:       2026-09-28
//! Version:    0.1.0

use std::fmt;

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
    //  ---
    //  Constructors
    //  ---

    /// Creates a new variable term.
    pub fn variable(name: impl Into<String>) -> Self {
        Self::Var(name.into())
    }

    /// Creates a new lambda term.
    pub fn lambda(
        param: impl Into<String>,
        body: impl Into<Term>,
    ) -> Self {
        Self::Lambda {
            param: param.into(),
            body: Box::new(body.into()),
        }
    }

    /// Creates a new application term.
    pub fn app(lhs: impl Into<Term>, rhs: impl Into<Term>) -> Self {
        Self::App(Box::new(lhs.into()), Box::new(rhs.into()))
    }


    ///  ---
    ///  Accessors
    ///  ---
    /// Returns the name of the variable if the term is a variable, otherwise returns `None`.
    pub fn as_variable(&self) -> Option<&str> {
        if let Self::Var(name) = self {
            Some(name)
        } else {
            None
        }
    }

    /// Returns the parameter and body of the lambda abstraction if the term is a lambda, otherwise returns `None`.
    pub fn as_lambda(&self) -> Option<(&str, &Term)> {
        if let Self::Lambda { param, body } = self {
            Some((param, body))
        } else {
            None
        }
    }

    /// Returns the left-hand side and right-hand side of the application if the term is an application, otherwise returns `None`.
    pub fn as_app(&self) -> Option<(&Term, &Term)> {
        if let Self::App(lhs, rhs) = self {
            Some((lhs, rhs))
        } else {
            None
        }
    }

    // Returns true if the term is a value, which in the case of lambda calculus is any lambda abstraction.
    pub fn is_value(&self) -> bool {
        matches!(self, Self::Lambda { .. })
    }
}

// EOF