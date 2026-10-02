//! Abstrações para termos e valores semânticos.

use std::fmt;

use crate::common::ToLatex;

/// Um termo de uma linguagem formal.
///
/// O termo concreto continua sendo definido pela linguagem.
pub trait Term:
    Clone
    + PartialEq
    + Eq
    + fmt::Display
    + ToLatex
{
}

/// Um valor de uma linguagem formal.
///
/// Valores são os resultados finais da avaliação.
pub trait Value:
    Clone
    + PartialEq
    + Eq
    + fmt::Display
    + ToLatex
{
}