use simple_languages::arith::evaluator::ArithEvaluator;
use simple_languages::arith::{BinaryOp, Term};
use simple_languages::common::Evaluator;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // (2 * 3) + (4 - 5)
    let term = Term::binary(
        BinaryOp::Add,
        Term::binary(BinaryOp::Mul, Term::integer(2), Term::integer(3)),
        Term::binary(BinaryOp::Sub, Term::integer(4), Term::integer(5)),
    );

    let derivation = ArithEvaluator::evaluate(&term)?;

    println!("term:  {}", term);
    println!("value: {}", derivation.conclusion.value);
    println!("rules (postorder): {:#?}", derivation.postorder_rules());
    println!("latex (último passo): {}", derivation.to_latex_step());
    println!("latex (árvore completa): {}", derivation.to_latex_tree());

    Ok(())
}