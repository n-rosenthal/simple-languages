use simple_languages::common::machine_language::{Compile, Vm};
use simple_languages::common::semantics::{run, BigStep, Machine, Typing};
use simple_languages::common::{Lexer, Parser, Scanner};
use simple_languages::stlc::language::Stlc;
use simple_languages::stlc::lexer::StlcLexer;
use simple_languages::stlc::parser::StlcParser;
use simple_languages::stlc::scanner::StlcScanner;
use std::io::{stdin, stdout, Write};

fn main() {
    let mut stdin = stdin();
    let mut stdout = stdout();
    let mut buffer = String::new();

    loop {
        buffer.clear();
        stdout.write(b"> ").unwrap();
        stdout.flush().unwrap();
        stdin.read_line(&mut buffer).unwrap();

        if buffer.is_empty() {
            break;
        }

        let mut scanner = StlcScanner::new(&buffer);
        let mut lexer = StlcLexer::new(&mut scanner);
        let mut parser = StlcParser::new(&mut lexer);
        let ast = parser.parse().unwrap();

        let machine = Vm::new();
        let typing = Typing::new();
        let big_step = BigStep::new();
        let compile = Compile::new();
        let semantics = run(&ast, &machine, &typing, &big_step, &compile);

        println!("{}", semantics);
    }
}