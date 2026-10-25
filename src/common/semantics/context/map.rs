//! Implementação de contextos utilizando `HashMap`.
//!
//! `MapContext<B>` representa o contexto como um mapeamento direto de nomes
//! para bindings.
//!
//! Ao contrário de [`LinkedContext`](super::linked::LinkedContext), esta
//! representação não preserva bindings sombreados. Inserir novamente um
//! nome substitui o binding anterior.
//!
//! Portanto, `MapContext` é apropriado quando o contexto deve representar
//! apenas os bindings atualmente visíveis.

use std::collections::HashMap;
use std::fmt;

use crate::common::latex::ident;
use crate::common::ToLatex;

use super::traits::Context;

/// Contexto baseado em `HashMap`.
///
/// Cada nome aparece no máximo uma vez.
///
/// ```text
/// Γ = x:A, y:B
/// ```
///
/// é representado por um mapa:
///
/// ```text
/// x → A
/// y → B
/// ```
pub struct MapContext<B> {
    bindings: HashMap<String, B>,
}

impl<B> MapContext<B> {
    /// Itera sobre os bindings.
    ///
    /// A ordem não é especificada.
    pub fn iter(&self) -> impl Iterator<Item = (&str, &B)> {
        self.bindings
            .iter()
            .map(|(name, binding)| (name.as_str(), binding))
    }

    /// Retorna os bindings como vetor.
    ///
    /// A ordem não é especificada.
    pub fn bindings(&self) -> Vec<(&str, &B)> {
        self.iter().collect()
    }

}

impl<B: Clone> Context<B> for MapContext<B> {
    fn empty() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    fn len(&self) -> usize {
        self.bindings.len()
    }

    fn extend(&self, name: impl Into<String>, binding: B) -> Self {
        let mut bindings = self.bindings.clone();

        bindings.insert(name.into(), binding);

        Self { bindings }
    }

    fn lookup(&self, name: &str) -> Option<&B> {
        self.bindings.get(name)
    }
}

impl<B: Clone> Clone for MapContext<B> {
    fn clone(&self) -> Self {
        Self {
            bindings: self.bindings.clone(),
        }
    }
}

impl<B> Default for MapContext<B> {
    fn default() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }
}

impl<B: PartialEq> PartialEq for MapContext<B> {
    fn eq(&self, other: &Self) -> bool {
        self.bindings == other.bindings
    }
}

impl<B: Eq> Eq for MapContext<B> {}

impl<B: fmt::Debug> fmt::Debug for MapContext<B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(self.bindings.iter())
            .finish()
    }
}

impl<B: fmt::Display> fmt::Display for MapContext<B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.bindings.is_empty() {
            return f.write_str("∅");
        }

        // HashMap não possui ordem semântica. Ordenamos somente para que
        // a representação textual seja determinística.
        let mut bindings: Vec<_> = self.bindings.iter().collect();

        bindings.sort_by(|(a, _), (b, _)| a.cmp(b));

        for (i, (name, binding)) in bindings.into_iter().enumerate() {
            if i > 0 {
                f.write_str(", ")?;
            }

            write!(f, "{name}:{binding}")?;
        }

        Ok(())
    }
}

impl<B: ToLatex> ToLatex for MapContext<B> {
    fn to_latex(&self) -> String {
        if self.bindings.is_empty() {
            return r"\emptyset".to_string();
        }

        let mut bindings: Vec<_> = self.bindings.iter().collect();

        bindings.sort_by(|(a, _), (b, _)| a.cmp(b));

        bindings
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
        let ctx: MapContext<i32> = MapContext::empty();

        assert!(ctx.is_empty());
        assert_eq!(ctx.len(), 0);
        assert_eq!(ctx.lookup("x"), None);
        assert_eq!(ctx.to_string(), "∅");
    }

    #[test]
    fn extend_and_lookup() {
        let ctx = MapContext::empty()
            .extend("x", 1)
            .extend("y", 2);

        assert_eq!(ctx.len(), 2);
        assert_eq!(ctx.lookup("x"), Some(&1));
        assert_eq!(ctx.lookup("y"), Some(&2));
        assert_eq!(ctx.lookup("z"), None);
    }

    #[test]
    fn extending_existing_name_replaces_binding() {
        let ctx = MapContext::empty()
            .extend("x", 1)
            .extend("x", 2);

        assert_eq!(ctx.len(), 1);
        assert_eq!(ctx.lookup("x"), Some(&2));
    }

    #[test]
    fn extend_is_persistent() {
        let base = MapContext::empty()
            .extend("x", 1);

        let a = base.extend("y", 2);
        let b = base.extend("z", 3);

        assert_eq!(base.len(), 1);
        assert_eq!(base.lookup("x"), Some(&1));
        assert_eq!(base.lookup("y"), None);
        assert_eq!(base.lookup("z"), None);

        assert_eq!(a.lookup("x"), Some(&1));
        assert_eq!(a.lookup("y"), Some(&2));
        assert_eq!(a.lookup("z"), None);

        assert_eq!(b.lookup("x"), Some(&1));
        assert_eq!(b.lookup("y"), None);
        assert_eq!(b.lookup("z"), Some(&3));
    }

    #[test]
    fn fresh_names() {
        let ctx = MapContext::empty()
            .extend("x", ())
            .extend("x1", ());

        assert_eq!(ctx.fresh_name("x"), "x2");
        assert_eq!(ctx.fresh_name("y"), "y");
    }

    #[test]
    fn equality_compares_mappings() {
        let a = MapContext::empty()
            .extend("x", 1)
            .extend("y", 2);

        let b = MapContext::empty()
            .extend("y", 2)
            .extend("x", 1);

        assert_eq!(a, b);
    }

    #[test]
    fn display_is_deterministic() {
        let ctx = MapContext::empty()
            .extend("y", "Nat")
            .extend("x", "Bool");

        assert_eq!(ctx.to_string(), "x:Bool, y:Nat");
    }
}