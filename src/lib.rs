//! Implementações das linguagens de *Types and Programming Languages* (TAPL).

pub mod common;
pub mod lambda;
pub mod registry;

// A migração de `arith` para o backbone ainda não foi feita; reative quando
// `arith` implementar `Language` (ver as instruções de migração).
// pub mod arith;