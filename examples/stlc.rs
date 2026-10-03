use simple_languages::common::machine_language::{Compile, Vm};
use simple_languages::common::semantics::{run, BigStep, Machine, Typing};
use simple_languages::common::{Lexer, Parser, Scanner};
use simple_languages::stlc::{
    StlcBigStep, StlcCompiler, StlcLexer, StlcParser, StlcScanner, StlcSmallStep,
    StlcTyping,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = "(λf:A->A. λx:A. f x) (λy:A. y)";

    let lines = StlcScanner::scan(source)?;
    let tokens = StlcLexer::analyze(&lines)?;
    let term = StlcParser::parse(&tokens)?;

    let typing = StlcTyping::check(&term)?;
    println!("term:  {term}");
    println!("type:  {}", typing.conclusion.ty);
    println!("typing rules (postorder): {:?}\n", typing.postorder_rules());

    println!("small-step:");
    print!("{}", run::<StlcSmallStep>(term.clone()).to_text());

    let big = StlcBigStep::evaluate(&term)?;
    println!("\nbig-step:\n{}", big.to_text());
    println!("latex:\n{}\n", big.to_latex_tree());

    let program = StlcCompiler::compile(&term)?;
    println!("machine code:\n{program}");

    let execution = Vm::execute(&program);
    println!("machine: {} ({} steps)", execution.trace.outcome, execution.trace.len());

    Ok(())
}
