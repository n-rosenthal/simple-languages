//! Memória (μ): associa locações a valores (TAPL, cap. 13).
//!
//! Uma [`Store`] só cresce: `alloc` devolve uma locação nova e nenhuma é
//! liberada, como no modelo do livro (não há coletor de lixo na
//! semântica). Ler ou escrever numa locação que não existe é um erro
//! [`Dangling`]; na semântica estrutural isso corresponde a um termo
//! travado.
//!
//! O contexto de tipos da memória (Σ) tem a mesma forma: um mapa de
//! locações para tipos, daí [`StoreTyping`].

use std::fmt;
use std::mem;

use crate::common::ToLatex;

/// Uma locação `l0`, `l1`, ... Só a [`Store`] cria locações válidas.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Location(usize);

impl Location {
    pub fn index(self) -> usize {
        self.0
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "l{}", self.0)
    }
}

impl ToLatex for Location {
    fn to_latex(&self) -> String {
        format!("l_{{{}}}", self.0)
    }
}

/// Acesso a uma locação que não está na memória.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Dangling(pub Location);

impl fmt::Display for Dangling {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "dangling location `{}`", self.0)
    }
}

impl std::error::Error for Dangling {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Store<V> {
    cells: Vec<V>,
}

/// Σ: o tipo de cada locação.
pub type StoreTyping<Ty> = Store<Ty>;

impl<V> Store<V> {
    pub fn new() -> Self {
        Self { cells: Vec::new() }
    }

    pub fn len(&self) -> usize {
        self.cells.len()
    }

    pub fn is_empty(&self) -> bool {
        self.cells.is_empty()
    }

    pub fn contains(&self, location: Location) -> bool {
        location.0 < self.cells.len()
    }

    /// `ref v`: aloca uma célula nova e devolve a locação.
    pub fn alloc(&mut self, value: V) -> Location {
        self.cells.push(value);
        Location(self.cells.len() - 1)
    }

    /// `!l`.
    pub fn read(&self, location: Location) -> Result<&V, Dangling> {
        self.cells.get(location.0).ok_or(Dangling(location))
    }

    /// `l := v`. Devolve o valor antigo.
    pub fn write(&mut self, location: Location, value: V) -> Result<V, Dangling> {
        match self.cells.get_mut(location.0) {
            Some(cell) => Ok(mem::replace(cell, value)),
            None => Err(Dangling(location)),
        }
    }

    /// As células em ordem de alocação.
    pub fn iter(&self) -> impl Iterator<Item = (Location, &V)> {
        self.cells.iter().enumerate().map(|(i, v)| (Location(i), v))
    }
}

impl<V> Default for Store<V> {
    fn default() -> Self {
        Self::new()
    }
}

/// `{l0 ↦ 1, l1 ↦ true}`
impl<V: fmt::Display> fmt::Display for Store<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("{")?;
        for (i, (location, value)) in self.iter().enumerate() {
            if i > 0 {
                f.write_str(", ")?;
            }
            write!(f, "{location} ↦ {value}")?;
        }
        f.write_str("}")
    }
}

impl<V: ToLatex> ToLatex for Store<V> {
    fn to_latex(&self) -> String {
        let cells = self
            .iter()
            .map(|(location, value)| {
                format!(r"{} \mapsto {}", location.to_latex(), value.to_latex())
            })
            .collect::<Vec<_>>()
            .join(r",\,");
        format!(r"\{{{cells}\}}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn alloc_returns_fresh_locations() {
        let mut store = Store::new();
        let a = store.alloc(10);
        let b = store.alloc(20);

        assert_ne!(a, b);
        assert_eq!(store.len(), 2);
        assert_eq!(store.read(a), Ok(&10));
        assert_eq!(store.read(b), Ok(&20));
    }

    #[test]
    fn write_returns_the_old_value() {
        let mut store = Store::new();
        let l = store.alloc(1);

        assert_eq!(store.write(l, 2), Ok(1));
        assert_eq!(store.read(l), Ok(&2));
    }

    #[test]
    fn aliasing_through_a_copied_location() {
        let mut store = Store::new();
        let a = store.alloc(0);
        let b = a; // Copy: duas referências para a mesma célula

        store.write(b, 5).unwrap();
        assert_eq!(store.read(a), Ok(&5));
    }

    #[test]
    fn unknown_locations_dangle() {
        let mut store: Store<i32> = Store::new();
        let l = store.alloc(0);
        let mut other: Store<i32> = Store::new(); // memória sem a locação `l`

        assert_eq!(other.read(l), Err(Dangling(l)));
        assert_eq!(other.write(l, 1), Err(Dangling(l)));
        assert!(!other.contains(l));
        assert!(store.contains(l));
    }

    #[test]
    fn clone_is_a_snapshot() {
        let mut store = Store::new();
        let l = store.alloc(1);
        let before = store.clone();

        store.write(l, 2).unwrap();
        assert_eq!(before.read(l), Ok(&1));
        assert_eq!(store.read(l), Ok(&2));
    }

    #[test]
    fn display() {
        let mut store = Store::new();
        assert_eq!(store.to_string(), "{}");
        store.alloc(1);
        store.alloc(2);
        assert_eq!(store.to_string(), "{l0 ↦ 1, l1 ↦ 2}");
    }
}
