///! `src/languages/stlc/language.rs` contains the implementation of the simply typed lambda calculus (STLC) language. It defines the syntax, typing rules, and evaluation semantics for STLC, providing a foundation for working with typed functional programming languages.

use std::fmt;

use crate::common::language::Language;
use crate::common::{Lexer, Parser, Scanner};

// ... other imports ...


/// Simply-Typed Lambda Calculus (STLC) language implementation
pub struct SimplyTyped;

impl Language for SimplyTyped {
    const NAME: &'static str = "stlc";

    // type Term = Term;
    // type Type = Type;
    // type SyntaxError = SyntaxError;

    // type Scanner = STLCScanner;
    // type Lexer = STLCLexer;
    // type Parser = STLCParser;

    // type Typing = STLCTyping;
    // type BigStep = STLCBigStep;
    // type SmallStep = STLCSmallStep;
    // type Compiler = STLCCompiler;
}