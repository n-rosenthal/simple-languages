//! Semântica estrutural de `arith-extensions` (TAPL, cap. 3), esquerda para a direita.
//!
//! Um termo em que nenhuma regra se aplica e que não é valor (`true + 1`,
//! `if 1 then ...`) está *travado*: é um resultado normal, não um erro.

use crate::common::semantics::{Step, Transition};

use super::terms::Term;
use super::values::{apply, Value};

crate::rules! {
    pub enum SmallStepRule {
        /// t1 → t1'  ⟹  t1 op t2 → t1' op t2
        BinaryLeft => "E-Bin1" {
            [r"t_1 \to t_1'"] => r"t_1 \oplus t_2 \to t_1' \oplus t_2"
        },
        /// v1 valor, t2 → t2'  ⟹  v1 op t2 → v1 op t2'
        BinaryRight => "E-Bin2" {
            [r"t_2 \to t_2'"] => r"v_1 \oplus t_2 \to v_1 \oplus t_2'"
        },
        /// v1, v2 valores  ⟹  v1 op v2 → resultado
        BinaryCompute => "E-BinConst" {
            [r"v = v_1 \oplus v_2"] => r"v_1 \oplus v_2 \to v"
        },
        /// t1 → t1'  ⟹  if t1 then t2 else t3 → if t1' then t2 else t3
        IfCongruence => "E-If" {
            [r"t_1 \to t_1'"]
                => r"\mathsf{if}\ t_1\ \mathsf{then}\ t_2\ \mathsf{else}\ t_3 \to \mathsf{if}\ t_1'\ \mathsf{then}\ t_2\ \mathsf{else}\ t_3"
        },
        /// if true then t2 else t3 → t2
        IfTrue => "E-IfTrue" {
            [] => r"\mathsf{if}\ \mathsf{true}\ \mathsf{then}\ t_2\ \mathsf{else}\ t_3 \to t_2"
        },
        /// if false then t2 else t3 → t3
        IfFalse => "E-IfFalse" {
            [] => r"\mathsf{if}\ \mathsf{false}\ \mathsf{then}\ t_2\ \mathsf{else}\ t_3 \to t_3"
        },

        //  Peano arithmetic
        /// t → t'  ⟹  succ(t) → succ(t')
        SuccCongruence => "E-Succ" {
            [r"t \to t'"] => r"\mathsf{succ}(t) \to \mathsf{succ}(t')"
        },

        /// t → t'  ⟹  pred(t) → pred(t')
        PredCongruence => "E-Pred" {
            [r"t \to t'"] => r"\mathsf{pred}(t) \to \mathsf{pred}(t')"
        },

        /// t → t'  ⟹  iszero(t) → iszero(t')
        IsZeroCongruence => "E-IsZero" {
            [r"t \to t'"] => r"\mathsf{iszero}(t) \to \mathsf{iszero}(t')"
        },

        /// pred(0) → 0
        PredZero => "E-PredZero" {
            [] => r"\mathsf{pred}(0) \to 0"
        },

        /// pred(succ(nv)) → nv
        PredSucc => "E-PredSucc" {
            [] => r"\mathsf{pred}(\mathsf{succ}(nv)) \to nv"
        },

        /// iszero(0) → true
        IsZeroZero => "E-IsZeroZero" {
            [] => r"\mathsf{iszero}(0) \to \mathsf{true}"
        },

        /// iszero(succ(nv)) → false
        IsZeroSucc => "E-IsZeroSucc" {
            [] => r"\mathsf{iszero}(\mathsf{succ}(nv)) \to \mathsf{false}"
        },

    }
}

pub struct ArithSmallStep;

fn value_of(term: &Term) -> Option<Value> {
    match term {
        Term::Integer(n) => Some(Value::Integer(*n)),
        Term::Boolean(b) => Some(Value::Boolean(*b)),

        Term::Zero => Some(Value::Natural(0)),

        Term::Succ(n) => {
            let inner = value_of(n)?;

            match inner {
                Value::Natural(k) => Some(Value::Natural(k + 1)),
                _ => None,
            }
        }

        // pred e iszero são operadores, não valores.
        Term::Pred(_) | Term::IsZero(_) => None,

        _ => None,
    }
}

impl ArithSmallStep {
    /// A regra mais externa e o termo seguinte, sem montar a `Transition`.
    ///
    /// As regras de congruência recorrem a subtermos; se cada nível montasse
    /// uma `Transition` (clonando o termo), um passo custaria O(n²) numa
    /// cadeia `1 + 1 + ... + 1`.
    fn reduce(term: &Term) -> Option<(SmallStepRule, Term)> {
        match term {
            // Valores
            Term::Integer(_)
            | Term::Boolean(_)
            | Term::Zero => None,

            // E-Succ:
            //
            // t → t'
            // ----------------
            // succ t → succ t'
            //
            // `succ nv` é um valor; portanto, só precisamos reduzir
            // seu argumento quando ele ainda não é valor.
            Term::Succ(inner) => {
                if inner.is_value() {
                    None
                } else {
                    let (_, next_inner) = Self::reduce(inner)?;
                    Some((
                        SmallStepRule::SuccCongruence,
                        Term::succ(next_inner),
                    ))
                }
            }

            // pred 0 → 0
            //
            // pred (succ nv) → nv
            //
            // Caso o argumento ainda não seja valor, primeiro reduzimos
            // esse argumento.
            Term::Pred(inner) => {
                if !inner.is_value() {
                    let (_, next_inner) = Self::reduce(inner)?;

                    return Some((
                        SmallStepRule::PredCongruence,
                        Term::pred(next_inner),
                    ));
                }

                match inner.as_ref() {
                    Term::Zero => Some((
                        SmallStepRule::PredZero,
                        Term::zero(),
                    )),

                    Term::Succ(nv) if nv.is_value() => Some((
                        SmallStepRule::PredSucc,
                        (**nv).clone(),
                    )),

                    // Um pred de algo que não é um natural valor trava.
                    _ => None,
                }
            }

            // iszero 0 → true
            //
            // iszero (succ nv) → false
            //
            // Caso o argumento ainda não seja valor, reduzimos primeiro.
            Term::IsZero(inner) => {
                if !inner.is_value() {
                    let (_, next_inner) = Self::reduce(inner)?;

                    return Some((
                        SmallStepRule::IsZeroCongruence,
                        Term::is_zero(next_inner),
                    ));
                }

                match inner.as_ref() {
                    Term::Zero => Some((
                        SmallStepRule::IsZeroZero,
                        Term::boolean(true),
                    )),

                    Term::Succ(nv) if nv.is_value() => Some((
                        SmallStepRule::IsZeroSucc,
                        Term::boolean(false),
                    )),

                    // iszero de algo que não é um natural valor trava.
                    _ => None,
                }
            }

            Term::Binary { op, lhs, rhs } => {
                // E-Bin1: reduz o lado esquerdo primeiro.
                if !lhs.is_value() {
                    let (_, inner) = Self::reduce(lhs)?;
                    let next = Term::binary(
                        *op,
                        inner,
                        (**rhs).clone(),
                    );

                    return Some((
                        SmallStepRule::BinaryLeft,
                        next,
                    ));
                }

                // E-Bin2: esquerda é valor; reduz a direita.
                if !rhs.is_value() {
                    let (_, inner) = Self::reduce(rhs)?;
                    let next = Term::binary(
                        *op,
                        (**lhs).clone(),
                        inner,
                    );

                    return Some((
                        SmallStepRule::BinaryRight,
                        next,
                    ));
                }

                // E-BinConst: ambos são valores.
                //
                // Se os operandos forem incompatíveis, não existe regra:
                // o termo fica travado.
                let result = apply(
                    *op,
                    value_of(lhs)?,
                    value_of(rhs)?,
                )?;

                Some((
                    SmallStepRule::BinaryCompute,
                    result.into(),
                ))
            }

            Term::If {
                condition,
                then_branch,
                else_branch,
            } => match condition.as_ref() {
                // E-IfTrue
                Term::Boolean(true) => Some((
                    SmallStepRule::IfTrue,
                    (**then_branch).clone(),
                )),

                // E-IfFalse
                Term::Boolean(false) => Some((
                    SmallStepRule::IfFalse,
                    (**else_branch).clone(),
                )),

                // Uma condição inteira não é booleana e trava.
                Term::Integer(_) => None,

                // E-If
                _ => {
                    let (_, inner) = Self::reduce(condition)?;

                    let next = Term::if_then_else(
                        inner,
                        (**then_branch).clone(),
                        (**else_branch).clone(),
                    );

                    Some((
                        SmallStepRule::IfCongruence,
                        next,
                    ))
                }
            },
        }
    }
}

impl Step for ArithSmallStep {
    type State = Term;
    type Rule = SmallStepRule;

    fn is_final(term: &Term) -> bool {
        term.is_value()
    }

    fn step(term: &Term) -> Option<Transition<SmallStepRule, Term>> {
        let (rule, next) = Self::reduce(term)?;
        Some(Transition::new(rule, term.clone(), next))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arith_extensions::terms::BinaryOp;
    use crate::common::semantics::run;

    fn nat(n: u64) -> Term {
        Term::natural(n)
    }

    fn int(n: i64) -> Term {
        Term::integer(n)
    }

    fn bool(b: bool) -> Term {
        Term::boolean(b)
    }


    fn add(l: Term, r: Term) -> Term {
        Term::binary(BinaryOp::Add, l, r)
    }

    #[test]
    fn single_step_addition() {
        let step = ArithSmallStep::step(&add(int(1), int(2))).unwrap();
        assert_eq!(step.rule, SmallStepRule::BinaryCompute);
        assert_eq!(step.to, int(3));
    }

    #[test]
    fn trace_reduces_nested_addition_left_to_right() {
        // (1 + 2) + (3 + 4) → 3 + (3 + 4) → 3 + 7 → 10
        let term = add(add(int(1), int(2)), add(int(3), int(4)));
        let trace = run::<ArithSmallStep>(term);

        assert!(trace.is_final());
        assert_eq!(trace.final_state, int(10));
        assert_eq!(
            trace.rules(),
            vec![
                SmallStepRule::BinaryLeft,
                SmallStepRule::BinaryRight,
                SmallStepRule::BinaryCompute
            ]
        );
    }

    #[test]
    fn if_true_reduces_directly() {
        let term = Term::if_then_else(Term::boolean(true), int(10), int(20));
        let trace = run::<ArithSmallStep>(term);

        assert_eq!(trace.final_state, int(10));
        assert_eq!(trace.rules(), vec![SmallStepRule::IfTrue]);
    }

    #[test]
    fn the_condition_is_reduced_first() {
        let term = Term::if_then_else(
            Term::binary(BinaryOp::LessThan, int(1), int(2)),
            int(10),
            int(20),
        );
        let trace = run::<ArithSmallStep>(term);

        assert_eq!(
            trace.rules(),
            vec![SmallStepRule::IfCongruence, SmallStepRule::IfTrue]
        );
        assert_eq!(trace.final_state, int(10));
    }

    #[test]
    fn stuck_term_is_reported_not_erred() {
        // true + 1: não existe regra para Boolean + Integer
        let term = add(Term::boolean(true), int(1));
        let trace = run::<ArithSmallStep>(term.clone());

        assert!(trace.is_stuck());
        assert_eq!(trace.final_state, term);
        assert!(trace.is_empty());
    }

    #[test]
    fn a_stuck_term_is_not_a_value() {
        let term = add(Term::boolean(true), int(1));
        assert!(!ArithSmallStep::is_final(&term));
        assert!(ArithSmallStep::step(&term).is_none());
    }

    #[test]
    fn stuckness_propagates_from_subterms() {
        let term = add(add(Term::boolean(true), int(1)), int(2));
        assert!(run::<ArithSmallStep>(term).is_stuck());
    }

    #[test]
    fn gets_stuck_after_making_progress() {
        // (1 + 2) + true → 3 + true
        let trace = run::<ArithSmallStep>(add(add(int(1), int(2)), Term::boolean(true)));

        assert!(trace.is_stuck());
        assert_eq!(trace.len(), 1);
        assert_eq!(trace.final_state, add(int(3), Term::boolean(true)));
    }

    #[test]
    fn non_boolean_condition_is_stuck() {
        let term = Term::if_then_else(int(1), int(2), int(3));
        assert!(run::<ArithSmallStep>(term).is_stuck());
    }

    #[test]
    fn overflow_wraps_so_well_typed_terms_never_stick() {
        let trace = run::<ArithSmallStep>(add(int(i64::MAX), int(1)));

        assert!(trace.is_final());
        assert_eq!(trace.final_state, int(i64::MIN));
    }

    #[test]
    fn pred_zero_reduces_to_zero() {
        let term = Term::pred(Term::zero());

        let step = ArithSmallStep::step(&term).unwrap();

        assert_eq!(step.rule, SmallStepRule::PredZero);
        assert_eq!(step.to, Term::zero());
    }

    #[test]
    fn pred_succ_reduces_to_inner_natural() {
        let term = Term::pred(Term::succ(nat(3)));

        let step = ArithSmallStep::step(&term).unwrap();

        assert_eq!(step.rule, SmallStepRule::PredSucc);
        assert_eq!(step.to, nat(3));
    }

    #[test]
    fn pred_reduces_argument_first() {
        let term = Term::pred(Term::succ(Term::pred(Term::zero())));

        let trace = run::<ArithSmallStep>(term);

        assert!(trace.is_final());
        assert_eq!(trace.final_state, Term::zero());

        assert_eq!(
            trace.rules(),
            vec![
                SmallStepRule::PredCongruence,
                SmallStepRule::PredSucc,
            ]
        );
    }

    #[test]
    fn iszero_zero_reduces_to_true() {
        let term = Term::is_zero(Term::zero());

        let step = ArithSmallStep::step(&term).unwrap();

        assert_eq!(step.rule, SmallStepRule::IsZeroZero);
        assert_eq!(step.to, Term::boolean(true));
    }

    #[test]
    fn iszero_succ_reduces_to_false() {
        let term = Term::is_zero(Term::succ(nat(3)));

        let step = ArithSmallStep::step(&term).unwrap();

        assert_eq!(step.rule, SmallStepRule::IsZeroSucc);
        assert_eq!(step.to, Term::boolean(false));
    }

    #[test]
    fn succ_reduces_argument_first() {
        let term = Term::succ(Term::pred(Term::zero()));

        let trace = run::<ArithSmallStep>(term);

        assert!(trace.is_final());
        assert_eq!(trace.final_state, Term::succ(Term::zero()));

        assert_eq!(
            trace.rules(),
            vec![
                SmallStepRule::SuccCongruence,
            ]
        );
    }
}
