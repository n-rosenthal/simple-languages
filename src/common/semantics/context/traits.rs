//! Interfaces abstratas para contextos semânticos.
//!
//! Um contexto associa nomes a bindings de tipo `B`. O significado de `B`
//! depende do uso:
//!
//! - `Context<Type>` para contextos de tipagem;
//! - `Context<Value>` para ambientes de avaliação;
//! - `Context<Term>` para ambientes que associam nomes a termos;
//! - `Context<()>` para contextos que registram apenas a presença de nomes.
//!
//! A interface não impõe uma representação concreta. Implementações podem
//! utilizar listas persistentes, mapas, vetores ou outras estruturas.

/// Interface geral para contextos semânticos.
///
/// Um contexto associa identificadores a bindings de tipo `B`.
///
/// A operação [`Context::extend`] é persistente: recebe `&self` e devolve
/// um novo contexto, preservando o contexto original.
///
/// # Shadowing
///
/// Quando um nome é inserido mais de uma vez, `lookup` deve retornar o
/// binding mais recentemente introduzido para esse nome.
pub trait Context<B>: Clone {
    /// Cria o contexto vazio `∅`.
    fn empty() -> Self;

    /// Retorna o número de bindings representados pelo contexto.
    fn len(&self) -> usize;

    /// Estende o contexto com um novo binding.
    ///
    /// A operação não modifica `self`; retorna um novo contexto.
    fn extend(&self, name: impl Into<String>, binding: B) -> Self;

    /// Procura o binding visível associado a `name`.
    ///
    /// Se o nome não estiver presente, retorna `None`.
    ///
    /// Se houver shadowing, deve ser retornado o binding mais recente.
    fn lookup(&self, name: &str) -> Option<&B>;

    /// Verifica se `name` está presente no contexto.
    fn contains(&self, name: &str) -> bool {
        self.lookup(name).is_some()
    }

    /// Verifica se o contexto está vazio.
    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Gera um nome que não ocorre no contexto: `x`, `x1`, `x2`, ...
    fn fresh_name(&self, base: &str) -> String {
        if !self.contains(base) {
            return base.to_string();
        }

        (1..)
            .map(|n| format!("{base}{n}"))
            .find(|candidate| !self.contains(candidate))
            .expect("infinite iterator")
    }
}

/// Extensão de [`Context`] para contextos que possuem uma ordem explícita
/// entre seus bindings.
///
/// A ordem permite associar um nome a um índice de De Bruijn:
///
/// ```text
/// Γ = x:A, y:B, z:C
///
/// z ↦ 0
/// y ↦ 1
/// x ↦ 2
/// ```
///
/// O índice `0` corresponde sempre ao binding mais recente.
///
/// Nem toda implementação de [`Context`] precisa fornecer essa operação.
/// Por exemplo, um `HashMap` não possui uma ordem de bindings naturalmente
/// significativa.
pub trait IndexedContext<B>: Context<B> {
    /// Retorna o índice de De Bruijn correspondente a `name`.
    ///
    /// O binding mais recente possui índice `0`.
    fn lookup_index(&self, name: &str) -> Option<usize>;

    /// Retorna o binding localizado no índice de De Bruijn `index`.
    ///
    /// `0` corresponde ao binding mais recente.
    fn get(&self, index: usize) -> Option<(&str, &B)>;
}