use simple_languages::common::machine_language::{Compile, Vm};
use simple_languages::common::semantics::{run, BigStep, Machine, Typing};
use simple_languages::common::{Lexer, Parser, Scanner};
use simple_languages::lambda::{
    LambdaBigStep, LambdaCompiler, LambdaLexer, LambdaParser, LambdaScanner, LambdaSmallStep,
    LambdaTyping,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = "(λf:A->A. λx:A. f x) (λy:A. y)";

    let lines = LambdaScanner::scan(source)?;
    let tokens = LambdaLexer::analyze(&lines)?;
    let term = LambdaParser::parse(&tokens)?;

    let typing = LambdaTyping::check(&term)?;
    println!("term:  {term}");
    println!("type:  {}", typing.conclusion.ty);
    println!("typing rules (postorder): {:?}\n", typing.postorder_rules());

    println!("small-step:");
    print!("{}", run::<LambdaSmallStep>(term.clone()).to_text());

    let big = LambdaBigStep::evaluate(&term)?;
    println!("\nbig-step:\n{}", big.to_text());
    println!("latex:\n{}\n", big.to_latex_tree());

    let program = LambdaCompiler::compile(&term)?;
    println!("machine code:\n{program}");

    let execution = Vm::execute(&program);
    println!("machine: {} ({} steps)", execution.trace.outcome, execution.trace.len());

    Ok(())
}