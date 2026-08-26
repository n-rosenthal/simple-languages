use simple_languages::common::Evaluator;
use simple_languages::arith::Term;
use simple_languages::arith::evaluator::ArithEvaluator;

fn main() {
    let term = Term::binary(
        simple_languages::arith::BinaryOp::Add,
        Term::integer(2),
        Term::integer(3),
    );

    // Chamada correta: função associada, não método
    let (value, rules) = ArithEvaluator::evaluate(&term).unwrap();

    println!("Value: {}", value);
    println!("Rules: {:#?}", rules);
}