//! S/src/SMachine/Instruction.rs
//!
//! Conjunto de instruções da SMachine: uma máquina de pilha simples,
//! com memória endereçável separada (heap) para suportar referências.
//!
//! author:  n-rosenthal
//! date:    2026-08-19
//! version: 0.1

/// Um valor manipulado pela SMachine em tempo de execução.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Unit,
    /// Endereço de uma posição na `heap` da máquina.
    Location(usize),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Unit => write!(f, "unit"),
            Value::Location(loc) => write!(f, "<loc {loc}>"),
        }
    }
}

/// Uma instrução da SMachine.
///
/// Convenção de endereçamento de variáveis: `Load(i)` lê o valor no
/// índice absoluto `i` da pilha de dados (contado a partir da base,
/// posição 0), não relativo ao topo. Essa escolha evita que
/// referências a variáveis fiquem erradas quando código intermediário
/// empilha valores temporários acima delas — o índice absoluto de
/// uma variável nunca muda desde o momento em que ela é ligada até o
/// `Slide` que remove seu escopo.
///
/// Convenção de saltos: `Jump`/`JumpIfFalse` guardam índices absolutos
/// de instrução (posição no vetor do programa), calculados pelo
/// compilador após montar o programa inteiro.
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // --- constantes ---
    PushInt(i64),
    PushBool(bool),
    PushUnit,

    // --- aritmética e comparação (todas: pop 2, push 1) ---
    Add,
    Sub,
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,

    // --- lógica (pop 1, push 1) ---
    Not,

    // --- variáveis ---
    /// Empilha uma cópia do valor no índice absoluto `i` da pilha.
    Load(usize),

    // --- controle de fluxo ---
    /// Salta incondicionalmente para a instrução de índice absoluto dado.
    Jump(usize),
    /// Desempilha um `Boolean`; se falso, salta. Se verdadeiro, segue
    /// para a próxima instrução.
    JumpIfFalse(usize),

    // --- memória (heap) ---
    /// Desempilha um valor, aloca na heap, empilha `Location` do
    /// endereço recém-criado.
    Alloc,
    /// Desempilha uma `Location`, empilha uma cópia do valor
    /// armazenado naquele endereço da heap.
    Deref,
    /// Desempilha um valor `v` e, em seguida, uma `Location(loc)`;
    /// escreve `v` em `heap[loc]`; empilha `Unit`.
    StoreMem,

    // --- manipulação de pilha ---
    /// Descarta o valor no topo da pilha.
    Pop,
    /// Remove `n` valores que estão logo abaixo do topo da pilha,
    /// mantendo o valor do topo. Usado para encerrar o escopo de uma
    /// variável `let` depois que seu corpo já foi avaliado: o valor
    /// ligado (que ocupava um slot abaixo do resultado do corpo) é
    /// descartado, e só o resultado do corpo permanece.
    Slide(usize),

    /// Encerra a execução.
    Halt,
}

impl std::fmt::Display for Instruction {
    /// Formatação mnemônica de uma instrução isolada, sem endereço —
    /// usada por `disassemble` para montar a listagem completa, mas
    /// também útil sozinha (ex.: depurando uma instrução específica).
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::PushInt(n) => write!(f, "PushInt {n}"),
            Instruction::PushBool(b) => write!(f, "PushBool {b}"),
            Instruction::PushUnit => write!(f, "PushUnit"),
            Instruction::Add => write!(f, "Add"),
            Instruction::Sub => write!(f, "Sub"),
            Instruction::Eq => write!(f, "Eq"),
            Instruction::Neq => write!(f, "Neq"),
            Instruction::Lt => write!(f, "Lt"),
            Instruction::Leq => write!(f, "Leq"),
            Instruction::Gt => write!(f, "Gt"),
            Instruction::Geq => write!(f, "Geq"),
            Instruction::Not => write!(f, "Not"),
            Instruction::Load(idx) => write!(f, "Load {idx}"),
            Instruction::Jump(target) => write!(f, "Jump {target}"),
            Instruction::JumpIfFalse(target) => write!(f, "JumpIfFalse {target}"),
            Instruction::Alloc => write!(f, "Alloc"),
            Instruction::Deref => write!(f, "Deref"),
            Instruction::StoreMem => write!(f, "StoreMem"),
            Instruction::Pop => write!(f, "Pop"),
            Instruction::Slide(n) => write!(f, "Slide {n}"),
            Instruction::Halt => write!(f, "Halt"),
        }
    }
}

/// Formata um programa inteiro como uma listagem legível, uma
/// instrução por linha, prefixada pelo seu índice absoluto — o mesmo
/// índice que `Jump`/`JumpIfFalse` referenciam. Pensada para exibição
/// (terminal, frontend web), não para ser reanalisada de volta em
/// instruções — não é um formato de serialização.
///
/// ```text
/// 0000  PushInt 1
/// 0001  PushInt 2
/// 0002  Add
/// ```
pub fn disassemble(program: &[Instruction]) -> String {
    program
        .iter()
        .enumerate()
        .map(|(idx, instr)| format!("{idx:04}  {instr}"))
        .collect::<Vec<_>>()
        .join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn display_de_instrucoes_individuais() {
        assert_eq!(Instruction::PushInt(42).to_string(), "PushInt 42");
        assert_eq!(Instruction::Load(3).to_string(), "Load 3");
        assert_eq!(Instruction::Add.to_string(), "Add");
    }

    #[test]
    fn disassemble_numera_cada_linha_pelo_indice_absoluto() {
        let program = vec![
            Instruction::PushInt(1),
            Instruction::PushInt(2),
            Instruction::Add,
        ];
        assert_eq!(
            disassemble(&program),
            "0000  PushInt 1\n0001  PushInt 2\n0002  Add"
        );
    }
}
