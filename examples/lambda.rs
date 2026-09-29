use simple_languages::common::{Lexer, Scanner};
use simple_languages::lambda::lexer::LambdaLexer;
use simple_languages::lambda::scanner::LambdaScanner;
use simple_languages::lambda::{Term, Type};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Lexer only (works as soon as scanner + mod.rs exist)
    let source = "(λf:Bool->Bool. λx:Bool. f x) (λy:Bool. y)";
    let lines = LambdaScanner::scan(source)?;
    let tokens = LambdaLexer::analyze(&lines)?;

    println!("source: {source}");
    for t in &tokens {
        println!("{:?} {:?} @ {}..{}", t.kind, t.lexeme, t.span.start, t.span.end);
    }

    // 2. Built by hand, mirroring the arith main: λx:Bool. x
    let term = Term::Lambda { param: "x".to_string(), body: Box::new(Term::Var("x".to_string())) };
    println!("term: {term}");

    // 3. Once the parser and checker exist:
    // let parsed = LambdaParser::parse(&tokens)?;
    // let ty = LambdaTypeChecker::check(&parsed)?;
    // println!("type: {ty}");
    // let derivation = LambdaEvaluator::evaluate(&parsed)?;
    // println!("value: {}", derivation.conclusion.value);
    // println!("latex: {}", derivation.to_latex_tree());

    Ok(())
}