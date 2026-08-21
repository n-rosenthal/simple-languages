//! S/src/Compiler.rs
//!
//! Compila um `Term` (AST de `S`, já validado pelo type-checker) para
//! um programa da SMachine (`Vec<Instruction>`).
//!
//! author:  n-rosenthal
//! date:    2026-08-19
//! version: 0.1

#![allow(non_snake_case)]

use crate::SMachine::Instruction::Instruction;
use crate::Scanner::Types::{BinaryOperator, Term, UnaryOperator};

/// Ambiente de compilação: associa nomes de variáveis ligadas por
/// `let` ao índice absoluto da pilha onde seu valor mora, e mantém a
/// profundidade atual da pilha (quantos valores existem no momento
/// em que este ambiente é usado para compilar um sub-termo).
///
/// A escolha de guardar índices *absolutos* (em vez de deslocamentos
/// relativos ao topo) é o que torna a composição de sub-expressões
/// simples: cálculos intermediários empilhados temporariamente acima
/// de uma variável nunca invalidam o índice dela — só um `Slide`
/// deliberado (emitido ao final do `let` correspondente) o faz.
#[derive(Debug, Clone)]
struct CompileEnv {
    bindings: Vec<(String, usize)>,
    depth: usize,
}

impl CompileEnv {
    fn new() -> Self {
        CompileEnv { bindings: Vec::new(), depth: 0 }
    }

    /// Retorna um ambiente igual, mas com a profundidade atualizada
    /// para `new_depth` (usado depois de compilar um sub-termo cujo
    /// efeito na pilha já é conhecido estaticamente).
    fn at_depth(&self, new_depth: usize) -> Self {
        CompileEnv { bindings: self.bindings.clone(), depth: new_depth }
    }

    /// Retorna um ambiente com uma nova ligação `name -> idx`, onde
    /// `idx` é o índice absoluto (igual à profundidade atual antes de
    /// empilhar o valor ligado). A nova profundidade é `idx + 1`.
    fn with_binding(&self, name: String, idx: usize) -> Self {
        let mut bindings = self.bindings.clone();
        bindings.push((name, idx));
        CompileEnv { bindings, depth: idx + 1 }
    }

    /// Busca o índice absoluto de `name`, procurando da ligação mais
    /// recente para a mais antiga (respeitando sombreamento: um `let`
    /// mais interno com o mesmo nome esconde o externo).
    fn lookup(&self, name: &str) -> Option<usize> {
        self.bindings.iter().rev().find(|(n, _)| n == name).map(|(_, idx)| *idx)
    }
}

/// Erros possíveis durante a compilação.
///
/// Na prática, se o `Term` já passou pelo type-checker (`type_of`),
/// só `UnboundVariable` deveria ser realmente alcançável aqui — e só
/// se o compilador for chamado sobre uma `Term` que o type-checker
/// não validou. Mesmo assim, preferimos `Result` a `panic!`, para que
/// erros de uso incorreto do compilador sejam depuráveis.
#[derive(Debug, PartialEq)]
pub enum CompileError {
    UnboundVariable { name: String },
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::UnboundVariable { name } => {
                write!(f, "variável '{name}' não resolvida durante a compilação")
            }
        }
    }
}

impl std::error::Error for CompileError {}

/// Insere `code` (cujos saltos internos são endereços *locais*,
/// 0-indexados a partir do próprio início) dentro de `dest`, somando
/// a cada `Jump`/`JumpIfFalse` de `code` o deslocamento
/// `dest.len()` — o ponto onde `code` vai começar dentro de `dest`.
///
/// Toda chamada recursiva de `compile_in` devolve um fragmento
/// autocontido nesse sentido; é responsabilidade de quem monta
/// fragmentos maiores (como `Conditional`) usar esta função em vez de
/// `Vec::extend` diretamente, sempre que o fragmento inserido contém
/// saltos.
fn splice(dest: &mut Vec<Instruction>, code: Vec<Instruction>) {
    let offset = dest.len();
    for instr in code {
        let relocated = match instr {
            Instruction::Jump(target) => Instruction::Jump(target + offset),
            Instruction::JumpIfFalse(target) => Instruction::JumpIfFalse(target + offset),
            other => other,
        };
        dest.push(relocated);
    }
}

fn binary_instruction(operator: BinaryOperator) -> Instruction {
    match operator {
        BinaryOperator::Add => Instruction::Add,
        BinaryOperator::Sub => Instruction::Sub,
        BinaryOperator::Eq => Instruction::Eq,
        BinaryOperator::Neq => Instruction::Neq,
        BinaryOperator::Lt => Instruction::Lt,
        BinaryOperator::Leq => Instruction::Leq,
        BinaryOperator::Gt => Instruction::Gt,
        BinaryOperator::Geq => Instruction::Geq,
    }
}

/// Ponto de entrada público: compila um `Term` fechado (sem variáveis
/// livres) para um programa da SMachine.
pub fn compile(term: &Term) -> Result<Vec<Instruction>, CompileError> {
    compile_in(term, &CompileEnv::new())
}

/// Compila `term` sob `env`, produzindo um fragmento autocontido cujo
/// efeito líquido na pilha é sempre "empilha exatamente um valor" —
/// não importa quantas instruções internas ele use, o fragmento nunca
/// deixa a pilha com profundidade diferente de `env.depth + 1` ao
/// final. Essa invariante é o que permite compor sub-termos por
/// concatenação simples (via `splice`) sem reconciliar profundidades
/// manualmente em cada ponto de chamada.
fn compile_in(term: &Term, env: &CompileEnv) -> Result<Vec<Instruction>, CompileError> {
    match term {
        Term::LiteralInteger { value, .. } => Ok(vec![Instruction::PushInt(*value)]),
        Term::LiteralBoolean { value, .. } => Ok(vec![Instruction::PushBool(*value)]),
        Term::Unit { .. } => Ok(vec![Instruction::PushUnit]),

        Term::Variable { name, .. } => {
            let idx = env
                .lookup(name)
                .ok_or_else(|| CompileError::UnboundVariable { name: name.clone() })?;
            Ok(vec![Instruction::Load(idx)])
        }

        Term::UnaryOperation { operator, operand, .. } => {
            let mut code = compile_in(operand, env)?;
            match operator {
                UnaryOperator::Not => code.push(Instruction::Not),
            }
            Ok(code)
        }

        Term::BinaryOperation { operator, left, right, .. } => {
            let mut code = compile_in(left, env)?;
            let env_after_left = env.at_depth(env.depth + 1);
            let right_code = compile_in(right, &env_after_left)?;
            splice(&mut code, right_code);
            code.push(binary_instruction(*operator));
            Ok(code)
        }

        Term::Conditional { condition, then_branch, else_branch, .. } => {
            let cond_code = compile_in(condition, env)?;
            // Condição consumida (pop) pelo JumpIfFalse; then/else
            // compilados no mesmo `env` de entrada (a condição já
            // não está mais na pilha nesse ponto).
            let then_code = compile_in(then_branch, env)?;
            let else_code = compile_in(else_branch, env)?;

            let mut code = cond_code;
            let jump_if_false_pos = code.len();
            code.push(Instruction::JumpIfFalse(0)); // placeholder

            splice(&mut code, then_code);
            let jump_pos = code.len();
            code.push(Instruction::Jump(0)); // placeholder

            let else_start = code.len();
            splice(&mut code, else_code);
            let end = code.len();

            code[jump_if_false_pos] = Instruction::JumpIfFalse(else_start);
            code[jump_pos] = Instruction::Jump(end);

            Ok(code)
        }

        Term::Let { name, value, body, .. } => {
            let mut code = compile_in(value, env)?;
            let bind_idx = env.depth; // valor de `value` aterrissa exatamente aqui
            let env_body = env.with_binding(name.clone(), bind_idx);
            let body_code = compile_in(body, &env_body)?;
            splice(&mut code, body_code);
            code.push(Instruction::Slide(1));
            Ok(code)
        }

        Term::Ref { inner, .. } => {
            let mut code = compile_in(inner, env)?;
            code.push(Instruction::Alloc);
            Ok(code)
        }

        Term::Deref { inner, .. } => {
            let mut code = compile_in(inner, env)?;
            code.push(Instruction::Deref);
            Ok(code)
        }

        Term::Assign { target, value, .. } => {
            let mut code = compile_in(target, env)?;
            let env_after_target = env.at_depth(env.depth + 1);
            let value_code = compile_in(value, &env_after_target)?;
            splice(&mut code, value_code);
            code.push(Instruction::StoreMem);
            Ok(code)
        }

        Term::Sequence { left, right, .. } => {
            let mut code = compile_in(left, env)?;
            code.push(Instruction::Pop);
            let right_code = compile_in(right, env)?;
            splice(&mut code, right_code);
            Ok(code)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::SMachine::VM::Machine;
    use crate::Scanner::Evaluator::{eval_in, Store as EvalStore, ValueEnv};
    use crate::Scanner::Lexer::Lexer;
    use crate::Scanner::Parser::Parser;
    use crate::Scanner::TypeChecker::type_of;
    use crate::Scanner::Types::SourceFile;

    /// Roda `source` pelos dois caminhos — o avaliador em árvore
    /// (`Scanner::Evaluator::eval_in`, já validado em toda a
    /// implementação anterior de `S`) e a SMachine via este
    /// compilador — e devolve as representações textuais dos dois
    /// resultados, para comparação nos testes.
    fn run_both(source: &str) -> (String, String) {
        let src = SourceFile {
            path: "<test>".into(),
            content: source.to_string(),
            lines: source.lines().map(str::to_string).collect(),
        };

        let lexer = Lexer::new(&src);
        let mut parser = Parser::new(lexer);
        let term = parser.parse().expect("deveria parsear");

        type_of(&term).expect("deveria tipar");

        // avaliador em árvore
        let mut env = ValueEnv::new();
        let mut store = EvalStore::new();
        let tree_result = eval_in(&term, &mut env, &mut store).to_string();

        // SMachine
        let program = compile(&term).expect("deveria compilar");
        let machine_result = Machine::new()
            .run(&program)
            .expect("programa da SMachine deveria rodar sem erro")
            .to_string();

        (tree_result, machine_result)
    }

    fn assert_matches(source: &str) {
        let (tree, machine) = run_both(source);
        assert_eq!(
            tree, machine,
            "avaliador em árvore e SMachine divergiram para: {source}\narvore={tree}, machine={machine}"
        );
    }

    #[test]
    fn aritmetica_simples() {
        assert_matches("1 + 2");
        assert_matches("10 - 3");
        assert_matches("(1 + 2) - (3 - 4)");
    }

    #[test]
    fn comparacoes_e_booleanos() {
        assert_matches("1 < 2");
        assert_matches("5 >= 5");
        assert_matches("true == false");
        assert_matches("not (1 < 2)");
        assert_matches("1 <> 2");
    }

    #[test]
    fn let_simples_e_aninhado() {
        assert_matches("let x: Integer = 10 in x + 1");
        assert_matches("let x: Integer = 1 in let y: Integer = 2 in x + y");
        assert_matches("let x: Integer = 1 in let x: Boolean = true in x");
    }

    #[test]
    fn let_em_ambos_operandos_de_uma_soma() {
        // exercita especificamente o caso em que compilar o operando
        // esquerdo deixa um valor temporário na pilha antes de
        // compilar o direito, que por sua vez introduz seu próprio
        // `let` — validando que o esquema de índice absoluto não se
        // confunde com esse "ruído" intermediário.
        assert_matches("(let x: Integer = 1 in x + 1) + (let y: Integer = 2 in y + 1)");
    }

    #[test]
    fn condicional() {
        assert_matches("if true then 1 else 2");
        assert_matches("if 1 < 2 then 10 else 20");
        assert_matches("if not (1 == 1) then 1 else 2");
    }

    #[test]
    fn condicional_dentro_de_let() {
        assert_matches("let x: Integer = 5 in if x < 10 then x else 0");
    }

    #[test]
    fn memoria_ref_deref_assign() {
        assert_matches("let r: Ref Integer = ref 1 in !r");
        assert_matches("let r: Ref Integer = ref 1 in r := 99 ; !r");
    }

    #[test]
    fn duas_referencias_independentes() {
        assert_matches(
            "let a: Ref Integer = ref 1 in let b: Ref Integer = ref 1 in a := 100 ; !b",
        );
    }

    #[test]
    fn programa_combinado_let_if_ref_sequence() {
        assert_matches(
            "let r: Ref Integer = ref 0 in \
             let step: Integer = 5 in \
             (if step < 10 then r := step else r := 0) ; \
             !r + 1",
        );
    }

    #[test]
    fn compilacao_de_variavel_livre_falha_com_erro_dedicado() {
        // constrói manualmente um Term com uma variável livre, já
        // que o parser normal não produz isso sem um `let` anterior.
        use crate::Scanner::Types::Span;
        let term = Term::Variable { name: "fantasma".to_string(), span: Span::new(1, 1) };
        assert_eq!(
            compile(&term),
            Err(CompileError::UnboundVariable { name: "fantasma".to_string() })
        );
    }
}
