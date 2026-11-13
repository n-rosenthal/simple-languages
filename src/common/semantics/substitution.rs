use std::marker::PhantomData;

/// Substituição de uma variável identificada por nome.
///
/// É uma operação sobre termos, não uma declaração de suporte
/// a definições globais.
pub trait NamedSubstitutionTerm: Clone {
    fn substitute_name(&self, name: &str, replacement: &Self) -> Self;
}

/// Capacidade de expandir definições globais.
///
/// O parâmetro `Term` é o tipo de termo da linguagem.
pub trait DefinitionSupport<Term> {
    const SUPPORTED: bool;

    fn expand(term: &Term, name: &str, value: &Term) -> Term;
}

/// Implementação para linguagens que não suportam definições.
pub struct NoDefinitions;

/// Implementação para linguagens que suportam definições nominais.
pub struct NamedDefinitions;

impl<Term> DefinitionSupport<Term> for NoDefinitions
where
    Term: Clone,
{
    const SUPPORTED: bool = false;

    fn expand(term: &Term, _name: &str, _value: &Term) -> Term {
        term.clone()
    }
}

impl<Term> DefinitionSupport<Term> for NamedDefinitions
where
    Term: NamedSubstitutionTerm,
{
    const SUPPORTED: bool = true;

    fn expand(term: &Term, name: &str, value: &Term) -> Term {
        term.substitute_name(name, value)
    }
}

/// Interface para substituição por índices de De Bruijn.
pub trait DeBruijnSubstitution {
    type Term;

    fn substitute_index(
        term: &Self::Term,
        index: usize,
        replacement: &Self::Term,
    ) -> Option<Self::Term>;

    fn substitute_top(
        body: &Self::Term,
        argument: &Self::Term,
    ) -> Option<Self::Term>;
}