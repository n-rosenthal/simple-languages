//! Propriedades que relacionam semânticas diferentes.
//!
//! Cada função devolve `true` quando a propriedade vale para o termo
//! dado. Testes por linguagem (ou testes de propriedades com `proptest`)
//! as chamam com termos de exemplo.

use super::big_step::BigStep;
use super::typing::Typing;

/// Segurança, na forma big-step: se `t` é bem tipado, a avaliação
/// produz um valor (não falha por operandos incompatíveis).
///
/// Vale vacuamente (`true`) para termos mal tipados.
///
/// Observação: em big-step, uma falha de avaliação pode ser travamento
/// ou outro erro do avaliador (estouro, por exemplo). A propriedade só é
/// significativa se `B::Error` representar apenas travamento.
pub fn well_typed_evaluates<T, B>(term: &T::Term) -> bool
where
    T: Typing,
    B: BigStep<Term = T::Term>,
{
    !T::is_well_typed(term) || B::evaluate(term).is_ok()
}
