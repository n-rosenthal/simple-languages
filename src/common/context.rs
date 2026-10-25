//! Contextos (Γ): associam nomes a bindings.
//!
//! O binding `B` depende do uso: um tipo na tipagem (`Γ ⊢ t : T`), um
//! valor num ambiente de avaliação, `()` num contexto só de nomes.
//!
//! O contexto é uma lista encadeada *persistente*: [`Context::extend`]
//! devolve um contexto novo em O(1) e deixa o original intacto. Assim
//! não há `push`/`pop` para restaurar, uma derivação pode guardar o Γ de
//! cada nó, e fechamentos podem compartilhar ambientes.
//!
//! `Rc` torna o tipo `!Send`. Se um dia precisar de threads, troque por
//! `Arc`; a API não muda.

use std::fmt;
use std::rc::Rc;

use crate::common::latex::ident;
use crate::common::ToLatex;

struct Node<B> {
    name: String,
    binding: B,
    parent: Option<Rc<Node<B>>>,
}

pub struct Context<B> {
    head: Option<Rc<Node<B>>>,
    len: usize,
}

impl<B> Context<B> {
    /// O contexto vazio (Γ = ∅).
    pub fn empty() -> Self {
        Self { head: None, len: 0 }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Γ, x:B. O contexto original não muda.
    pub fn extend(&self, name: impl Into<String>, binding: B) -> Self {
        Self {
            head: Some(Rc::new(Node {
                name: name.into(),
                binding,
                parent: self.head.clone(),
            })),
            len: self.len + 1,
        }
    }

    /// Bindings do mais recente ao mais antigo.
    pub fn iter(&self) -> Iter<'_, B> {
        Iter { next: self.head.as_deref() }
    }

    /// Bindings do mais antigo ao mais recente (a ordem em que se lê Γ).
    pub fn bindings(&self) -> Vec<(&str, &B)> {
        let mut all: Vec<_> = self.iter().collect();
        all.reverse();
        all
    }

    /// O binding mais recente de `name` (o mais interno vence).
    pub fn lookup(&self, name: &str) -> Option<&B> {
        self.iter().find(|(n, _)| *n == name).map(|(_, b)| b)
    }

    pub fn contains(&self, name: &str) -> bool {
        self.lookup(name).is_some()
    }

    /// Índice de de Bruijn de `name`: 0 é o binding mais recente.
    pub fn lookup_index(&self, name: &str) -> Option<usize> {
        self.iter().position(|(n, _)| n == name)
    }

    /// O binding de índice `index` (0 é o mais recente).
    pub fn get(&self, index: usize) -> Option<(&str, &B)> {
        self.iter().nth(index)
    }

    /// Um nome que não ocorre no contexto: `base`, ou `base1`, `base2`, ...
    ///
    /// Só olha o contexto, não as variáveis livres de um termo; quem
    /// renomeia deve estender o contexto com as livres antes.
    pub fn fresh_name(&self, base: &str) -> String {
        if !self.contains(base) {
            return base.to_string();
        }
        (1..)
            .map(|n| format!("{base}{n}"))
            .find(|candidate| !self.contains(candidate))
            .expect("infinite iterator")
    }
}

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

impl<B> Clone for Context<B> {
    fn clone(&self) -> Self {
        Self { head: self.head.clone(), len: self.len }
    }
}

impl<B> Default for Context<B> {
    fn default() -> Self {
        Self::empty()
    }
}

/// Destruição iterativa: o `Drop` recursivo padrão de uma lista longa
/// de `Rc` estoura a pilha.
impl<B> Drop for Context<B> {
    fn drop(&mut self) {
        let mut current = self.head.take();
        while let Some(rc) = current {
            match Rc::try_unwrap(rc) {
                Ok(mut node) => current = node.parent.take(),
                Err(_) => break, // o resto da lista é compartilhado
            }
        }
    }
}

impl<B: PartialEq> PartialEq for Context<B> {
    fn eq(&self, other: &Self) -> bool {
        self.len == other.len && self.iter().eq(other.iter())
    }
}

impl<B: Eq> Eq for Context<B> {}

impl<B: fmt::Debug> fmt::Debug for Context<B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.bindings()).finish()
    }
}

/// `x:Bool, y:Nat`; o contexto vazio é `∅`.
impl<B: fmt::Display> fmt::Display for Context<B> {
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

impl<B: ToLatex> ToLatex for Context<B> {
    fn to_latex(&self) -> String {
        if self.is_empty() {
            return r"\emptyset".to_string();
        }
        self.bindings()
            .into_iter()
            .map(|(name, binding)| format!("{}{{:}}{}", ident(name), binding.to_latex()))
            .collect::<Vec<_>>()
            .join(r",\,")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_context() {
        let ctx: Context<i32> = Context::empty();
        assert!(ctx.is_empty());
        assert_eq!(ctx.lookup("x"), None);
        assert_eq!(ctx.to_string(), "∅");
    }

    #[test]
    fn extend_and_lookup() {
        let ctx = Context::empty().extend("x", 1).extend("y", 2);
        assert_eq!(ctx.len(), 2);
        assert_eq!(ctx.lookup("x"), Some(&1));
        assert_eq!(ctx.lookup("y"), Some(&2));
        assert_eq!(ctx.lookup("z"), None);
    }

    #[test]
    fn the_innermost_binding_shadows() {
        let ctx = Context::empty().extend("x", 1).extend("x", 2);
        assert_eq!(ctx.lookup("x"), Some(&2));
        assert_eq!(ctx.lookup_index("x"), Some(0));
        assert_eq!(ctx.len(), 2); // o binding externo continua lá
    }

    #[test]
    fn extend_is_persistent() {
        let base = Context::empty().extend("x", 1);
        let a = base.extend("y", 2);
        let b = base.extend("z", 3);

        assert_eq!(base.len(), 1);
        assert_eq!(a.lookup("z"), None);
        assert_eq!(b.lookup("y"), None);
        assert_eq!(a.lookup("x"), Some(&1)); // o pai é compartilhado
    }

    #[test]
    fn de_bruijn_indices() {
        let ctx = Context::empty().extend("x", ()).extend("y", ()).extend("z", ());
        assert_eq!(ctx.lookup_index("z"), Some(0));
        assert_eq!(ctx.lookup_index("x"), Some(2));
        assert_eq!(ctx.lookup_index("w"), None);
        assert_eq!(ctx.get(1).map(|(n, _)| n), Some("y"));
        assert!(ctx.get(3).is_none());
    }

    #[test]
    fn bindings_are_listed_oldest_first() {
        let ctx = Context::empty().extend("x", 1).extend("y", 2);
        assert_eq!(ctx.bindings(), vec![("x", &1), ("y", &2)]);
    }

    #[test]
    fn display_reads_left_to_right() {
        let ctx = Context::empty().extend("x", "Bool").extend("y", "Nat");
        assert_eq!(ctx.to_string(), "x:Bool, y:Nat");
    }

    #[test]
    fn equality_compares_contents() {
        let a = Context::empty().extend("x", 1).extend("y", 2);
        let b = Context::empty().extend("x", 1).extend("y", 2);
        let c = Context::empty().extend("y", 2).extend("x", 1);
        assert_eq!(a, b);
        assert_ne!(a, c);
    }

    #[test]
    fn fresh_names_avoid_the_context() {
        let ctx = Context::empty().extend("x", ()).extend("x1", ());
        assert_eq!(ctx.fresh_name("x"), "x2");
        assert_eq!(ctx.fresh_name("y"), "y");
    }

    #[test]
    fn dropping_a_deep_context_does_not_overflow_the_stack() {
        let mut ctx: Context<()> = Context::empty();
        for i in 0..200_000 {
            ctx = ctx.extend(format!("x{i}"), ());
        }
        drop(ctx);
    }
}
