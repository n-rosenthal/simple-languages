// arith/small_step.rs
use crate::common::{SmallStepEvaluator, Step};

use super::evaluator::EvaluationError;
use super::terms::{BinaryOp, Term};
use super::values::Value;

/// Regras de avaliação small-step de `arith`. Nomenclatura seguindo
/// a convenção usual: sufixo numérico para regras de congruência
/// (qual sub-termo está sendo reduzido), sem sufixo para regras de
/// computação (quando os operandos já são valores).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SmallStepRule {
    /// t1 → t1'  ⟹  t1 op t2 → t1' op t2
    BinaryLeft,
    /// v1 valor, t2 → t2'  ⟹  v1 op t2 → v1 op t2'
    BinaryRight,
    /// v1, v2 valores  ⟹  v1 op v2 → resultado
    BinaryCompute,
    /// t1 → t1'  ⟹  if t1 then t2 else t3 → if t1' then t2 else t3
    IfCongruence,
    /// if true then t2 else t3 → t2
    IfTrue,
    /// if false then t2 else t3 → t3
    IfFalse,
}

// arith/small_step.rs (continuação)

pub struct ArithSmallStep;

impl ArithSmallStep {
    /// Aplica um operador binário quando os dois lados já são
    /// valores de superfície, produzindo o termo-resultado.
    /// Reaproveita `Value`/a lógica de `evaluate_binary` do
    /// avaliador big-step via conversão pontual — evita duplicar a
    /// tabela de casos de operadores em dois lugares.
    fn compute_binary(op: BinaryOp, lhs: &Term, rhs: &Term) -> Result<Term, EvaluationError> {
        let lhs_value = Self::term_to_value(lhs);
        let rhs_value = Self::term_to_value(rhs);

        // reaproveita a mesma tabela de casos do big-step
        let (result, _rule) =
            super::evaluator::ArithEvaluator::evaluate_binary(op, lhs_value, rhs_value)?;

        Ok(Self::value_to_term(result))
    }

    fn term_to_value(term: &Term) -> Value {
        match term {
            Term::Integer(n) => Value::Integer(*n),
            Term::Boolean(b) => Value::Boolean(*b),
            // seguro: só chamado quando `is_value(term)` já foi checado
            _ => unreachable!("term_to_value chamado sobre termo que não é valor"),
        }
    }

    fn value_to_term(value: Value) -> Term {
        match value {
            Value::Integer(n) => Term::Integer(n),
            Value::Boolean(b) => Term::Boolean(b),
        }
    }
}

impl SmallStepEvaluator for ArithSmallStep {
    type Term = Term;
    type Rule = SmallStepRule;
    type Error = EvaluationError;

    fn is_value(term: &Term) -> bool {
        matches!(term, Term::Integer(_) | Term::Boolean(_))
    }

    fn step(term: &Term) -> Result<Option<Step<SmallStepRule, Term>>, EvaluationError> {
        match term {
            // valores não têm passo — evaluate_trace() os reconhece
            // via is_value, não precisa apontar isso aqui.
            Term::Integer(_) | Term::Boolean(_) => Ok(None),

            Term::Binary { op, lhs, rhs } => {
                if !Self::is_value(lhs) {
                    // E-Bin1: reduz o lado esquerdo primeiro
                    if let Some(inner) = Self::step(lhs)? {
                        let next = Term::binary(*op, inner.to.clone(), (**rhs).clone());
                        return Ok(Some(Step::new(SmallStepRule::BinaryLeft, term.clone(), next)));
                    }
                    // lhs não é valor e não tem passo: travado;
                    // deixa evaluate_trace() detectar via is_value.
                    return Ok(None);
                }

                if !Self::is_value(rhs) {
                    // E-Bin2: esquerda já é valor, reduz a direita
                    if let Some(inner) = Self::step(rhs)? {
                        let next = Term::binary(*op, (**lhs).clone(), inner.to.clone());
                        return Ok(Some(Step::new(SmallStepRule::BinaryRight, term.clone(), next)));
                    }
                    return Ok(None);
                }

                // E-BinConst: ambos são valores, computa
                let result = Self::compute_binary(*op, lhs, rhs)?;
                Ok(Some(Step::new(SmallStepRule::BinaryCompute, term.clone(), result)))
            }

            Term::If { condition, then_branch, else_branch } => match condition.as_ref() {
                Term::Boolean(true) => {
                    // E-IfTrue
                    Ok(Some(Step::new(
                        SmallStepRule::IfTrue,
                        term.clone(),
                        (**then_branch).clone(),
                    )))
                }
                Term::Boolean(false) => {
                    // E-IfFalse
                    Ok(Some(Step::new(
                        SmallStepRule::IfFalse,
                        term.clone(),
                        (**else_branch).clone(),
                    )))
                }
                _ => {
                    // E-If: reduz a condição
                    match Self::step(condition)? {
                        Some(inner) => {
                            let next = Term::if_then_else(
                                inner.to.clone(),
                                (**then_branch).clone(),
                                (**else_branch).clone(),
                            );
                            Ok(Some(Step::new(SmallStepRule::IfCongruence, term.clone(), next)))
                        }
                        None => Ok(None), // condição travada (ex.: `1` como condição)
                    }
                }
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::SmallStepEvaluator;

    #[test]
    fn single_step_addition() {
        let term = Term::binary(BinaryOp::Add, Term::integer(1), Term::integer(2));
        let step = ArithSmallStep::step(&term).unwrap().unwrap();
        assert_eq!(step.rule, SmallStepRule::BinaryCompute);
        assert_eq!(step.to, Term::integer(3));
    }

    #[test]
    fn trace_reduces_nested_addition_left_to_right() {
        // (1 + 2) + (3 + 4)
        let term = Term::binary(
            BinaryOp::Add,
            Term::binary(BinaryOp::Add, Term::integer(1), Term::integer(2)),
            Term::binary(BinaryOp::Add, Term::integer(3), Term::integer(4)),
        );

        let trace = ArithSmallStep::evaluate_trace(&term).unwrap();

        assert!(!trace.is_stuck);
        assert_eq!(trace.final_term, Term::integer(10));
        // (1+2)+(3+4) → 3+(3+4) → 3+7 → 10  :  3 passos
        assert_eq!(trace.steps.len(), 3);
        assert_eq!(trace.steps[0].rule, SmallStepRule::BinaryLeft);
        assert_eq!(trace.steps[1].rule, SmallStepRule::BinaryRight);
        assert_eq!(trace.steps[2].rule, SmallStepRule::BinaryCompute);
    }

    #[test]
    fn if_true_reduces_directly() {
        let term = Term::if_then_else(Term::boolean(true), Term::integer(10), Term::integer(20));
        let trace = ArithSmallStep::evaluate_trace(&term).unwrap();
        assert_eq!(trace.final_term, Term::integer(10));
        assert_eq!(trace.steps.len(), 1);
        assert_eq!(trace.steps[0].rule, SmallStepRule::IfTrue);
    }

    #[test]
    fn stuck_term_is_reported_not_erred() {
        // true + 1  — trava (não existe regra pra Bool + Int)
        let term = Term::binary(BinaryOp::Add, Term::boolean(true), Term::integer(1));
        let trace = ArithSmallStep::evaluate_trace(&term).unwrap();
        assert!(trace.is_stuck);
        assert_eq!(trace.final_term, term);
        assert!(trace.steps.is_empty());
    }
}