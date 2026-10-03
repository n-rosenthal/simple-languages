///!    `src/languages/stlc/terms.rs`
///     Syntax definition for the TERMS of the simply typed lambda calculus (STLC) language. This module defines the structure and representation of terms in STLC.

use std::collections::BTreeSet;
use std::fmt;

use super::types::Type;

/// Terms of the Simply-Typed Lambda Calculus (STLC).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SimplyTypedTerm {
    ///     names
    ///     Literal 'x'
    Literal(String),

    ///     Identifier, variable "x"
    Identifier(String),


    ///     values
    ///     Natural numbers, e.g., 0, 1, 2, ...
    Natural(u64),

    ///     Boolean values, e.g., true, false
    Boolean(bool),


    //      control structures
    ///     if-then-else expression
    Conditional {
        condition: Box<SimplyTypedTerm>,
        then_branch: Box<SimplyTypedTerm>,
        else_branch: Box<SimplyTypedTerm>,
    },

    ///     Lambda abstraction, e.g., λx:T. body
    Lambda {
        param: String,
        ty: Type,
        body: Box<SimplyTypedTerm>,
    },

    ///     Function application, e.g., f x
    Application {
        func: Box<SimplyTypedTerm>,
        arg: Box<SimplyTypedTerm>,
    },
}

impl SimplyTypedTerm {
    pub fn free_vars(&self) -> BTreeSet<String> {
        match self {
            SimplyTypedTerm::Literal(_) => BTreeSet::new(),
            SimplyTypedTerm::Identifier(x) => BTreeSet::from([x.clone()]),
            SimplyTypedTerm::Natural(_) => BTreeSet::new(),
            SimplyTypedTerm::Boolean(_) => BTreeSet::new(),
            SimplyTypedTerm::Conditional { condition, .. } => condition.free_vars(),
            SimplyTypedTerm::Lambda { param, body, .. } => {
                let mut vars = body.free_vars();
                vars.remove(param);
                vars
            }
            SimplyTypedTerm::Application { func, arg, .. } => {
                let mut vars = func.free_vars();
                vars.extend(arg.free_vars());
                vars
            }
        }
    }
}