//! S/src/SMachine/VM.rs
//!
//! A SMachine em si: estado (pilha de dados + heap) e o ciclo
//! fetch-decode-execute que interpreta um programa (`&[Instruction]`).
//!
//! author:  n-rosenthal
//! date:    2026-08-19
//! version: 0.1

use crate::SMachine::Instruction::{Instruction, Value};

/// Erros possíveis durante a execução de um programa.
///
/// Um programa bem-formado, produzido por um compilador correto (ver
/// `compiling-s-to-smachine.org`), nunca deveria disparar nenhum
/// destes — eles existem para tornar bugs do compilador visíveis em
/// vez de causar pânico ou comportamento indefinido silencioso.
#[derive(Debug, PartialEq)]
pub enum MachineError {
    /// Tentativa de executar uma operação com a pilha vazia ou com
    /// menos operandos do que a instrução exige.
    StackUnderflow { at: usize },
    /// Uma instrução recebeu um valor do tipo errado (ex.: `Add`
    /// sobre um `Boolean`, `Deref` sobre algo que não é `Location`).
    TypeMismatch { at: usize, expected: &'static str },
    /// `Load`/`Deref`/`StoreMem` referenciou um índice fora dos limites.
    OutOfBounds { at: usize },
    /// O programa terminou (chegou ao fim do vetor de instruções)
    /// sem executar `Halt` e sem deixar exatamente um valor na pilha.
    MalformedResult,
}

impl std::fmt::Display for MachineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MachineError::StackUnderflow { at } => {
                write!(f, "pilha vazia demais para a instrução em {at}")
            }
            MachineError::TypeMismatch { at, expected } => {
                write!(f, "tipo inesperado na instrução em {at}: esperava {expected}")
            }
            MachineError::OutOfBounds { at } => {
                write!(f, "índice fora dos limites na instrução em {at}")
            }
            MachineError::MalformedResult => {
                write!(f, "programa terminou sem deixar exatamente um resultado na pilha")
            }
        }
    }
}

impl std::error::Error for MachineError {}

/// Estado de execução da SMachine.
#[derive(Debug, Default)]
pub struct Machine {
    pub stack: Vec<Value>,
    pub heap: Vec<Value>,
}

impl Machine {
    pub fn new() -> Self {
        Machine { stack: Vec::new(), heap: Vec::new() }
    }

    fn pop(&mut self, at: usize) -> Result<Value, MachineError> {
        self.stack.pop().ok_or(MachineError::StackUnderflow { at })
    }

    fn pop_integer(&mut self, at: usize) -> Result<i64, MachineError> {
        match self.pop(at)? {
            Value::Integer(n) => Ok(n),
            _ => Err(MachineError::TypeMismatch { at, expected: "Integer" }),
        }
    }

    fn pop_boolean(&mut self, at: usize) -> Result<bool, MachineError> {
        match self.pop(at)? {
            Value::Boolean(b) => Ok(b),
            _ => Err(MachineError::TypeMismatch { at, expected: "Boolean" }),
        }
    }

    fn pop_location(&mut self, at: usize) -> Result<usize, MachineError> {
        match self.pop(at)? {
            Value::Location(loc) => Ok(loc),
            _ => Err(MachineError::TypeMismatch { at, expected: "Location" }),
        }
    }

    /// Executa `program` do início ao fim (ou até `Halt`) e retorna o
    /// valor final deixado no topo da pilha.
    pub fn run(&mut self, program: &[Instruction]) -> Result<Value, MachineError> {
        let mut pc: usize = 0;

        while pc < program.len() {
            match &program[pc] {
                Instruction::Halt => break,

                Instruction::PushInt(n) => {
                    self.stack.push(Value::Integer(*n));
                    pc += 1;
                }
                Instruction::PushBool(b) => {
                    self.stack.push(Value::Boolean(*b));
                    pc += 1;
                }
                Instruction::PushUnit => {
                    self.stack.push(Value::Unit);
                    pc += 1;
                }

                Instruction::Add => {
                    let b = self.pop_integer(pc)?;
                    let a = self.pop_integer(pc)?;
                    self.stack.push(Value::Integer(a + b));
                    pc += 1;
                }
                Instruction::Sub => {
                    let b = self.pop_integer(pc)?;
                    let a = self.pop_integer(pc)?;
                    self.stack.push(Value::Integer(a - b));
                    pc += 1;
                }
                Instruction::Lt => {
                    let b = self.pop_integer(pc)?;
                    let a = self.pop_integer(pc)?;
                    self.stack.push(Value::Boolean(a < b));
                    pc += 1;
                }
                Instruction::Leq => {
                    let b = self.pop_integer(pc)?;
                    let a = self.pop_integer(pc)?;
                    self.stack.push(Value::Boolean(a <= b));
                    pc += 1;
                }
                Instruction::Gt => {
                    let b = self.pop_integer(pc)?;
                    let a = self.pop_integer(pc)?;
                    self.stack.push(Value::Boolean(a > b));
                    pc += 1;
                }
                Instruction::Geq => {
                    let b = self.pop_integer(pc)?;
                    let a = self.pop_integer(pc)?;
                    self.stack.push(Value::Boolean(a >= b));
                    pc += 1;
                }
                Instruction::Eq => {
                    let b = self.pop(pc)?;
                    let a = self.pop(pc)?;
                    self.stack.push(Value::Boolean(a == b));
                    pc += 1;
                }
                Instruction::Neq => {
                    let b = self.pop(pc)?;
                    let a = self.pop(pc)?;
                    self.stack.push(Value::Boolean(a != b));
                    pc += 1;
                }
                Instruction::Not => {
                    let a = self.pop_boolean(pc)?;
                    self.stack.push(Value::Boolean(!a));
                    pc += 1;
                }

                Instruction::Load(idx) => {
                    let value = self
                        .stack
                        .get(*idx)
                        .cloned()
                        .ok_or(MachineError::OutOfBounds { at: pc })?;
                    self.stack.push(value);
                    pc += 1;
                }

                Instruction::Jump(target) => {
                    pc = *target;
                }
                Instruction::JumpIfFalse(target) => {
                    let cond = self.pop_boolean(pc)?;
                    pc = if cond { pc + 1 } else { *target };
                }

                Instruction::Alloc => {
                    let value = self.pop(pc)?;
                    self.heap.push(value);
                    self.stack.push(Value::Location(self.heap.len() - 1));
                    pc += 1;
                }
                Instruction::Deref => {
                    let loc = self.pop_location(pc)?;
                    let value = self
                        .heap
                        .get(loc)
                        .cloned()
                        .ok_or(MachineError::OutOfBounds { at: pc })?;
                    self.stack.push(value);
                    pc += 1;
                }
                Instruction::StoreMem => {
                    let value = self.pop(pc)?;
                    let loc = self.pop_location(pc)?;
                    if loc >= self.heap.len() {
                        return Err(MachineError::OutOfBounds { at: pc });
                    }
                    self.heap[loc] = value;
                    self.stack.push(Value::Unit);
                    pc += 1;
                }

                Instruction::Pop => {
                    self.pop(pc)?;
                    pc += 1;
                }
                Instruction::Slide(n) => {
                    let top = self.pop(pc)?;
                    if self.stack.len() < *n {
                        return Err(MachineError::StackUnderflow { at: pc });
                    }
                    let new_len = self.stack.len() - n;
                    self.stack.truncate(new_len);
                    self.stack.push(top);
                    pc += 1;
                }
            }
        }

        if self.stack.len() != 1 {
            return Err(MachineError::MalformedResult);
        }

        Ok(self.stack[0].clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::SMachine::Instruction::Instruction::*;

    fn run(program: Vec<crate::SMachine::Instruction::Instruction>) -> Value {
        Machine::new().run(&program).expect("programa deveria rodar sem erro")
    }

    #[test]
    fn soma_simples() {
        assert_eq!(run(vec![PushInt(1), PushInt(2), Add]), Value::Integer(3));
    }

    #[test]
    fn comparacao() {
        assert_eq!(run(vec![PushInt(1), PushInt(2), Lt]), Value::Boolean(true));
    }

    #[test]
    fn load_variavel_por_indice_absoluto() {
        // equivalente a: let x = 10 in x + 1
        // stack ao entrar no corpo: [Integer(10)]  (índice 0 = x)
        let program = vec![
            PushInt(10),   // idx 0: x
            Load(0),       // empilha cópia de x
            PushInt(1),
            Add,
            Slide(1),      // remove o slot de x, mantendo o resultado
        ];
        assert_eq!(run(program), Value::Integer(11));
    }

    #[test]
    fn desvio_condicional_ramo_verdadeiro() {
        // if true then 1 else 2
        let program = vec![
            PushBool(true),
            JumpIfFalse(4),
            PushInt(1),
            Jump(5),
            PushInt(2),
        ];
        assert_eq!(run(program), Value::Integer(1));
    }

    #[test]
    fn desvio_condicional_ramo_falso() {
        let program = vec![
            PushBool(false),
            JumpIfFalse(4),
            PushInt(1),
            Jump(5),
            PushInt(2),
        ];
        assert_eq!(run(program), Value::Integer(2));
    }

    #[test]
    fn alloc_e_deref_encadeados() {
        // ref 42, depois !ref  =>  42
        let result = run(vec![PushInt(42), Alloc, Deref]);
        assert_eq!(result, Value::Integer(42));
    }

    #[test]
    fn store_mem_sobrescreve_heap_e_leitura_subsequente_ve_o_novo_valor() {
        // let r = ref 1 in (r := 99 ; !r)
        //
        // idx 0: r (uma Location, resultado de Alloc)
        // corpo: Load(0) duas vezes — uma para o alvo do StoreMem,
        // outra (depois) para o Deref final.
        let program = vec![
            PushInt(1),   // idx 0 (temporário, antes do Alloc)
            Alloc,        // idx 0 agora é a Location (heap[0] = 1)
            // --- corpo do let, r está em stack[0] ---
            Load(0),      // empilha Location (alvo de :=)
            PushInt(99),  // valor a escrever
            StoreMem,     // heap[0] = 99; empilha Unit
            Pop,          // descarta o Unit da atribuição (lado esquerdo do ';')
            Load(0),      // empilha Location de novo (para o !r)
            Deref,        // lê heap[0]
            Slide(1),     // remove o slot de r (stack[0]), mantém o resultado
        ];
        let mut m = Machine::new();
        let result = m.run(&program).expect("programa deveria rodar sem erro");
        assert_eq!(result, Value::Integer(99));
        assert_eq!(m.heap, vec![Value::Integer(99)]);
    }

    #[test]
    fn stack_underflow_e_reportado_como_erro_nao_panico() {
        let mut m = Machine::new();
        let result = m.run(&[Add]);
        assert!(matches!(result, Err(MachineError::StackUnderflow { .. })));
    }

    #[test]
    fn tipo_incompativel_e_reportado_como_erro() {
        let mut m = Machine::new();
        let result = m.run(&[PushBool(true), PushInt(1), Add]);
        assert!(matches!(result, Err(MachineError::TypeMismatch { .. })));
    }
}
