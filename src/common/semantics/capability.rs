//! Capacidades opcionais.
//!
//! Nem toda linguagem do TAPL define todas as semânticas: o livro dá o
//! big-step só em alguns capítulos, e linguagens como `featherweight-java`
//! ou `type-reconstruction` não fazem sentido na máquina virtual. Cada
//! semântica tem um *marcador* para a ausência ([`NoTyping`](super::NoTyping),
//! [`NoSmallStep`](super::NoSmallStep), [`NoBigStep`](super::NoBigStep) e
//! [`NoCompile`](crate::common::machine_language::NoCompile)) e um flag
//! (`DEFINED`, ou `AVAILABLE` na compilação) que o driver, as leis e a
//! interface consultam para pular o que não existe.

use std::fmt;

/// O erro dos marcadores: a semântica não está definida para a linguagem.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NotDefined;

impl fmt::Display for NotDefined {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("not defined for this language")
    }
}

impl std::error::Error for NotDefined {}
