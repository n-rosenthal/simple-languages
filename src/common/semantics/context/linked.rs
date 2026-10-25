//! Implementação de contextos como listas encadeadas persistentes.
//!
//! `LinkedContext<B>` representa um contexto como uma lista de bindings
//! compartilhada por meio de `Rc`.
//!
//! A extensão é O(1): apenas um novo nó é criado e o contexto anterior é
//! compartilhado.
//!
//! A cabeça da lista representa o binding mais recente. Isso fornece,
//! naturalmente, a semântica de shadowing e a numeração de De Bruijn.

use std::fmt;
use std::rc::Rc;

use crate::common::latex::ident;
use crate::common::ToLatex;

use super::traits::{Context, IndexedContext};

/// Nó de uma lista encadeada de bindings.
struct Node<B> {
    /// Nome associado ao binding.
    name: String,

    /// Binding associado ao nome.
    binding: B,

    /// Contexto anterior.
    parent: Option<Rc<Node<B>>>,
}

/// Contexto persistente implementado como uma lista encadeada.
///
/// O binding mais recente fica na cabeça da lista.
///
/// # Exemplo
///
/// ```text
/// Γ = x:A, y:B, z:C
///
/// head
///  │
///  ▼
/// z:C
///  │
///  ▼
/// y:B
///  │
///  ▼
/// x:A
///  │
///  ▼
/// ∅
/// ```
///
/// Portanto:
///
/// ```text
/// lookup("z")        → C
/// lookup_index("z")  → 0
/// lookup_index("y")  → 1
/// lookup_index("x")  → 2
/// ```
pub struct LinkedContext<B> {
    /// Binding mais recente.
    head: Option<Rc<Node<B>>>,

    /// Número de bindings.
    len: usize,
}

impl<B> LinkedContext<B> {
    /// Itera sobre os bindings do mais recente para o mais antigo.
    pub fn iter(&self) -> Iter<'_, B> {
        Iter {
            next: self.head.as_deref(),
        }
    }

    /// Retorna todos os bindings do mais antigo para o mais recente.
    ///
    /// Esta ordem é conveniente para exibição de contextos:
    ///
    /// ```text
    /// x:A, y:B, z:C
    /// ```
    pub fn bindings(&self) -> Vec<(&str, &B)> {
        let mut bindings: Vec<_> = self.iter().collect();
        bindings.reverse();
        bindings
    }

}

/// Implementação da interface geral de contexto.
impl<B> Context<B> for LinkedContext<B> {
    fn empty() -> Self {
        Self {
            head: None,
            len: 0,
        }
    }

    fn len(&self) -> usize {
        self.len
    }

    fn extend(&self, name: impl Into<String>, binding: B) -> Self {
        Self {
            head: Some(Rc::new(Node {
                name: name.into(),
                binding,
                parent: self.head.clone(),
            })),
            len: self.len + 1,
        }
    }

    fn lookup(&self, name: &str) -> Option<&B> {
        self.iter()
            .find(|(n, _)| *n == name)
            .map(|(_, binding)| binding)
    }
}

/// Implementação de operações indexadas.
///
/// Como `iter()` começa pelo binding mais recente, sua posição no iterador
/// coincide diretamente com o índice de De Bruijn.
impl<B> IndexedContext<B> for LinkedContext<B> {
    fn lookup_index(&self, name: &str) -> Option<usize> {
        self.iter().position(|(n, _)| n == name)
    }

    fn get(&self, index: usize) -> Option<(&str, &B)> {
        self.iter().nth(index)
    }
}

/// Iterador sobre um `LinkedContext`.
///
/// A ordem é do binding mais recente para o mais antigo.
pub struct Iter<'a, B> {
    next: Option<&'a Node<B>>,
}

impl<'a, B> Iterator for Iter<'a, B> {
    type Item = (&'a str, &'a B);

    fn next(&mut self) -> Option<Self::Item> {
        let node = self.next?;

        self.next = node.parent.as_deref();

        Some((node.name.as_str(), &node.binding))
    }
}

impl<B> Clone for LinkedContext<B> {
    fn clone(&self) -> Self {
        Self {
            head: self.head.clone(),
            len: self.len,
        }
    }
}

impl<B> Default for LinkedContext<B> {
    fn default() -> Self {
        Self::empty()
    }
}

/// Destruição iterativa.
///
/// Uma cadeia muito longa de `Rc` poderia provocar uma sequência recursiva
/// de destruições. A implementação percorre a cadeia explicitamente para
/// evitar crescimento da stack.
impl<B> Drop for LinkedContext<B> {
    fn drop(&mut self) {
        let mut current = self.head.take();

        while let Some(rc) = current {
            match Rc::try_unwrap(rc) {
                Ok(mut node) => {
                    current = node.parent.take();
                }
                Err(_) => {
                    // O restante da cadeia ainda possui outros proprietários.
                    break;
                }
            }
        }
    }
}

impl<B: PartialEq> PartialEq for LinkedContext<B> {
    fn eq(&self, other: &Self) -> bool {
        self.len == other.len && self.iter().eq(other.iter())
    }
}

impl<B: Eq> Eq for LinkedContext<B> {}

impl<B: fmt::Debug> fmt::Debug for LinkedContext<B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.bindings()).finish()
    }
}

impl<B: fmt::Display> fmt::Display for LinkedContext<B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_empty() {
            return f.write_str("∅");
        }

        for (i, (name, binding)) in self.bindings().into_iter().enumerate() {
            if i > 0 {
                f.write_str(", ")?;
            }

            write!(f, "{name}:{binding}")?;
        }

        Ok(())
    }
}

impl<B: ToLatex> ToLatex for LinkedContext<B> {
    fn to_latex(&self) -> String {
        if self.is_empty() {
            return r"\emptyset".to_string();
        }

        self.bindings()
            .into_iter()
            .map(|(name, binding)| {
                format!("{}{{:}}{}", ident(name), binding.to_latex())
            })
            .collect::<Vec<_>>()
            .join(r",\,")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_context() {
        let ctx: LinkedContext<i32> = LinkedContext::empty();

        assert!(ctx.is_empty());
        assert_eq!(ctx.len(), 0);
        assert_eq!(ctx.lookup("x"), None);
        assert_eq!(ctx.to_string(), "∅");
    }

    #[test]
    fn extend_and_lookup() {
        let ctx = LinkedContext::empty()
            .extend("x", 1)
            .extend("y", 2);

        assert_eq!(ctx.len(), 2);
        assert_eq!(ctx.lookup("x"), Some(&1));
        assert_eq!(ctx.lookup("y"), Some(&2));
        assert_eq!(ctx.lookup("z"), None);
    }

    #[test]
    fn shadowing() {
        let ctx = LinkedContext::empty()
            .extend("x", 1)
            .extend("x", 2);

        assert_eq!(ctx.lookup("x"), Some(&2));
        assert_eq!(ctx.len(), 2);
    }

    #[test]
    fn extend_is_persistent() {
        let base = LinkedContext::empty().extend("x", 1);

        let a = base.extend("y", 2);
        let b = base.extend("z", 3);

        assert_eq!(base.len(), 1);

        assert_eq!(a.lookup("x"), Some(&1));
        assert_eq!(a.lookup("y"), Some(&2));
        assert_eq!(a.lookup("z"), None);

        assert_eq!(b.lookup("x"), Some(&1));
        assert_eq!(b.lookup("y"), None);
        assert_eq!(b.lookup("z"), Some(&3));
    }

    #[test]
    fn de_bruijn_indices() {
        let ctx = LinkedContext::empty()
            .extend("x", ())
            .extend("y", ())
            .extend("z", ());

        assert_eq!(ctx.lookup_index("z"), Some(0));
        assert_eq!(ctx.lookup_index("y"), Some(1));
        assert_eq!(ctx.lookup_index("x"), Some(2));
        assert_eq!(ctx.lookup_index("w"), None);
    }

    #[test]
    fn get_by_de_bruijn_index() {
        let ctx = LinkedContext::empty()
            .extend("x", "A")
            .extend("y", "B")
            .extend("z", "C");

        assert_eq!(ctx.get(0), Some(("z", &"C")));
        assert_eq!(ctx.get(1), Some(("y", &"B")));
        assert_eq!(ctx.get(2), Some(("x", &"A")));
        assert_eq!(ctx.get(3), None);
    }

    #[test]
    fn bindings_are_oldest_first() {
        let ctx = LinkedContext::empty()
            .extend("x", 1)
            .extend("y", 2)
            .extend("z", 3);

        assert_eq!(
            ctx.bindings(),
            vec![("x", &1), ("y", &2), ("z", &3)]
        );
    }

    #[test]
    fn iter_is_newest_first() {
        let ctx = LinkedContext::empty()
            .extend("x", 1)
            .extend("y", 2)
            .extend("z", 3);

        let names: Vec<_> = ctx.iter().map(|(name, _)| name).collect();

        assert_eq!(names, vec!["z", "y", "x"]);
    }

    #[test]
    fn fresh_names() {
        let ctx = LinkedContext::empty()
            .extend("x", ())
            .extend("x1", ());

        assert_eq!(ctx.fresh_name("x"), "x2");
        assert_eq!(ctx.fresh_name("y"), "y");
    }

    #[test]
    fn equality_compares_contents() {
        let a = LinkedContext::empty()
            .extend("x", 1)
            .extend("y", 2);

        let b = LinkedContext::empty()
            .extend("x", 1)
            .extend("y", 2);

        let c = LinkedContext::empty()
            .extend("y", 2)
            .extend("x", 1);

        assert_eq!(a, b);
        assert_ne!(a, c);
    }

    #[test]
    fn dropping_deep_context_does_not_overflow() {
        let mut ctx: LinkedContext<()> = LinkedContext::empty();

        for i in 0..200_000 {
            ctx = ctx.extend(format!("x{i}"), ());
        }

        drop(ctx);
    }
}