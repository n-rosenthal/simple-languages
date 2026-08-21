//! S/src/lib.rs
//! Biblioteca principal do interpretador S, com suporte a WASM.
#![allow(non_snake_case)]

// Declaração de todos os módulos do projeto
pub mod Scanner;
pub mod SMachine;
pub mod Compiler;

use wasm_bindgen::prelude::*;

// Estado persistente da sessão REPL (ambiente de valores, ambiente de tipos e memória).
// Como estamos em WASM (single-thread), um RefCell é seguro.
thread_local! {
    static STATE: std::cell::RefCell<(Scanner::Evaluator::ValueEnv, Scanner::TypeChecker::TypeEnv, Scanner::Evaluator::Store)> =
        std::cell::RefCell::new((std::collections::HashMap::new(), std::collections::HashMap::new(), Vec::new()));
}

/// Avalia uma linha de entrada no REPL (com estado persistente).
/// Retorna a representação textual do resultado ou mensagem de erro.
#[wasm_bindgen]
pub fn eval_line(input: &str) -> String {
    use Scanner::Evaluator::eval_in;
    use Scanner::Lexer::Lexer;
    use Scanner::Parser::Parser;
    use Scanner::TypeChecker::type_of_with_env;
    use Scanner::Types::SourceFile;

    let source = SourceFile {
        path: std::path::PathBuf::from("<web>"),
        content: input.to_string(),
        lines: vec![input.to_string()],
    };

    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer);

    let term = match parser.parse() {
        Ok(t) => t,
        Err(err) => return format!("erro sintático: {err}"),
    };

    let result = STATE.with(|state| {
        // Obter uma referência mutável para a tupla (não mover)
        let mut state_ref = state.borrow_mut();
        let (ref mut env, ref mut type_env, ref mut store) = &mut *state_ref;

        // Verifica tipos com o ambiente persistente
        if let Err(err) = type_of_with_env(&term, type_env) {
            return format!("erro de tipo: {err}");
        }

        // Avalia
        let value = eval_in(&term, env, store);

        // Se for um let, atualiza o type_env
        if let Scanner::Types::Term::Let { name, declared_type, .. } = &term {
            type_env.insert(name.clone(), declared_type.clone());
        }

        // O estado já foi atualizado in-place, não precisamos reatribuir.

        // Formata a saída
        if matches!(term, Scanner::Types::Term::Let { .. }) {
            match value {
                Scanner::Evaluator::Value::Unit => "ok".to_string(),
                _ => format!("{term} = {value}"),
            }
        } else {
            format!("{term} = {value}")
        }
    });

    result
}

/// Reinicia a sessão (limpa ambientes e memória).
#[wasm_bindgen]
pub fn reset_session() {
    STATE.with(|state| {
        *state.borrow_mut() = (
            std::collections::HashMap::new(),
            std::collections::HashMap::new(),
            Vec::new(),
        );
    });
}

/// Retorna a versão do interpretador.
#[wasm_bindgen]
pub fn version() -> String {
    "0.1.0".to_string()
}

/// Verifica se a entrada está incompleta (para suporte a múltiplas linhas).
#[wasm_bindgen]
pub fn is_incomplete(input: &str) -> bool {
    use Scanner::Lexer::Lexer;
    use Scanner::Parser::Parser;
    use Scanner::Types::SourceFile;

    let source = SourceFile {
        path: std::path::PathBuf::from("<web>"),
        content: input.to_string(),
        lines: vec![input.to_string()],
    };
    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer);
    // Se o parser retornar UnexpectedEof, a expressão está incompleta.
    matches!(parser.parse(), Err(Scanner::Parser::ParseError::UnexpectedEof))
}

/// Compila `input` para bytecode da SMachine e retorna a listagem.
#[wasm_bindgen]
pub fn compile_bytecode(input: &str) -> String {
    use Compiler::compile;
    use SMachine::Instruction::disassemble;
    use Scanner::Lexer::Lexer;
    use Scanner::Parser::Parser;
    use Scanner::TypeChecker::type_of;
    use Scanner::Types::SourceFile;

    let source = SourceFile {
        path: std::path::PathBuf::from("<web>"),
        content: input.to_string(),
        lines: vec![input.to_string()],
    };

    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer);

    let term = match parser.parse() {
        Ok(t) => t,
        Err(err) => return format!("erro sintático: {err}"),
    };

    if let Err(err) = type_of(&term) {
        return format!("erro de tipo: {err}");
    }

    match compile(&term) {
        Ok(program) => disassemble(&program),
        Err(err) => format!("erro de compilação: {err}"),
    }
}

/// Compila e roda `input` na SMachine, retornando o valor final.
#[wasm_bindgen]
pub fn run_via_machine(input: &str) -> String {
    use Compiler::compile;
    use SMachine::VM::Machine;
    use Scanner::Lexer::Lexer;
    use Scanner::Parser::Parser;
    use Scanner::TypeChecker::type_of;
    use Scanner::Types::SourceFile;

    let source = SourceFile {
        path: std::path::PathBuf::from("<web>"),
        content: input.to_string(),
        lines: vec![input.to_string()],
    };

    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer);

    let term = match parser.parse() {
        Ok(t) => t,
        Err(err) => return format!("erro sintático: {err}"),
    };

    if let Err(err) = type_of(&term) {
        return format!("erro de tipo: {err}");
    }

    let program = match compile(&term) {
        Ok(p) => p,
        Err(err) => return format!("erro de compilação: {err}"),
    };

    match Machine::new().run(&program) {
        Ok(value) => value.to_string(),
        Err(err) => format!("erro de execução (SMachine): {err}"),
    }
}

/// Retorna uma representação textual da AST (árvore sintática) do termo.
#[wasm_bindgen]
pub fn parse_ast(input: &str) -> String {
    use Scanner::Lexer::Lexer;
    use Scanner::Parser::Parser;
    use Scanner::Types::SourceFile;

    let source = SourceFile {
        path: std::path::PathBuf::from("<web>"),
        content: input.to_string(),
        lines: vec![input.to_string()],
    };

    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer);

    match parser.parse() {
        Ok(term) => format!("{:#?}", term),
        Err(err) => format!("erro sintático: {err}"),
    }
}

/// Retorna o tipo inferido e o valor avaliado do termo (usando o avaliador em árvore).
#[wasm_bindgen]
pub fn analyze(input: &str) -> String {
    use Scanner::Evaluator::eval_in;
    use Scanner::Lexer::Lexer;
    use Scanner::Parser::Parser;
    use Scanner::TypeChecker::type_of;
    use Scanner::Types::SourceFile;

    let source = SourceFile {
        path: std::path::PathBuf::from("<web>"),
        content: input.to_string(),
        lines: vec![input.to_string()],
    };

    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer);

    let term = match parser.parse() {
        Ok(t) => t,
        Err(err) => return format!("erro sintático: {err}"),
    };

    let ty = match type_of(&term) {
        Ok(t) => t.to_string(),
        Err(err) => return format!("erro de tipo: {err}"),
    };

    let mut env = std::collections::HashMap::new();
    let mut store = Vec::new();
    let value = eval_in(&term, &mut env, &mut store);

    format!("Tipo: {}\nValor: {}", ty, value)
}
