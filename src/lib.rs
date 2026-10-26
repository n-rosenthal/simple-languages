//! Implementações das linguagens de *Types and Programming Languages* (TAPL).
//  
pub mod common;
pub mod registry;

//  arithmetic expressions
pub mod arith;
pub mod typed_arith;

//  lambda calculus
pub mod lambda;

//  nameless terms (De Bruijn indices)
pub mod nameless_terms;

//  simply typed lambda calculus
pub mod stlc;

