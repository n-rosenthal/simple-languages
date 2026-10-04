//! A linguagem de máquina compartilhada: um alvo único de compilação
//! para todas as linguagens.
//!
//! O alvo é *não-tipado* (os tipos são apagados na compilação) e usa
//! índices de de Bruijn: `Access(0)` é a variável mais recente. A
//! máquina é de ambiente e *call-by-value*, com uma pilha de valores e
//! uma pilha de quadros (frames) de retorno.
//!
//! Convenção de chamada: `Apply` espera `[.., função, argumento]`
//! (argumento no topo). Depois da chamada o resultado fica na pilha no
//! lugar de ambos. Compilar `f x` é, portanto, `f; x; Apply`.
//!
//! Um estado travado (operandos de tipo errado, pilha vazia, variável
//! inexistente, locação solta) é um resultado normal: `step` devolve
//! `None`, como na semântica estrutural.
//!
//! Cada linguagem implementa [`Compile`]; a lei
//! [`compilation_is_correct`] confere o resultado contra a semântica
//! natural.

use std::fmt;
use std::rc::Rc;

use crate::common::semantics::{BigStep, Machine, Step, Transition};
use crate::common::store::{Location, Store};

// =============================================================================
// Código
// =============================================================================

/// Uma sequência de instruções, compartilhada entre closures e quadros.
pub type Code = Rc<[Instr]>;

pub fn code(instructions: Vec<Instr>) -> Code {
    Rc::from(instructions)
}

/// Operadores primitivos estritos: desempilham dois valores e empilham um.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Prim {
    Add,
    Sub,
    Mul,
    Lt,
    Eq,
    And,
    Or,
}

impl Prim {
    pub fn name(self) -> &'static str {
        match self {
            Prim::Add => "add",
            Prim::Sub => "sub",
            Prim::Mul => "mul",
            Prim::Lt => "lt",
            Prim::Eq => "eq",
            Prim::And => "and",
            Prim::Or => "or",
        }
    }

    /// `None` quando os operandos não servem ao operador (o estado trava).
    /// A aritmética dá a volta em 64 bits, como em `arith`: o livro usa
    /// inteiros sem limite, e estouro travar quebraria o teorema de
    /// progresso.
    fn apply(self, lhs: Value, rhs: Value) -> Option<Value> {
        match (self, lhs, rhs) {
            (Prim::Add, Value::Int(a), Value::Int(b)) => Some(Value::Int(a.wrapping_add(b))),
            (Prim::Sub, Value::Int(a), Value::Int(b)) => Some(Value::Int(a.wrapping_sub(b))),
            (Prim::Mul, Value::Int(a), Value::Int(b)) => Some(Value::Int(a.wrapping_mul(b))),
            (Prim::Lt, Value::Int(a), Value::Int(b)) => Some(Value::Bool(a < b)),
            (Prim::Eq, Value::Int(a), Value::Int(b)) => Some(Value::Bool(a == b)),
            (Prim::Eq, Value::Bool(a), Value::Bool(b)) => Some(Value::Bool(a == b)),
            (Prim::And, Value::Bool(a), Value::Bool(b)) => Some(Value::Bool(a && b)),
            (Prim::Or, Value::Bool(a), Value::Bool(b)) => Some(Value::Bool(a || b)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instr {
    /// Constantes.
    Int(i64),
    Bool(bool),
    Unit,
    /// `[.., a, b]  ⟹  [.., a ⊕ b]`
    Prim(Prim),
    /// Empilha a variável de índice `n` (0 é a mais recente).
    Access(usize),
    /// Empilha uma closure: o código e o ambiente atual.
    Closure(Code),
    /// `[.., f, v]`: chama `f` com `v`.
    Apply,
    /// Desempilha um booleano e executa um dos dois ramos, depois continua.
    Branch(Code, Code),
    /// `ref v`: aloca uma célula e empilha a locação.
    Ref,
    /// `!l`.
    Deref,
    /// `l := v`; empilha `unit`.
    Assign,
}

impl Instr {
    /// A instrução em uma linha (sem o código aninhado).
    pub fn mnemonic(&self) -> String {
        match self {
            Instr::Int(n) => format!("int {n}"),
            Instr::Bool(b) => format!("bool {b}"),
            Instr::Unit => "unit".into(),
            Instr::Prim(p) => p.name().into(),
            Instr::Access(i) => format!("access {i}"),
            Instr::Closure(_) => "closure".into(),
            Instr::Apply => "apply".into(),
            Instr::Branch(..) => "branch".into(),
            Instr::Ref => "ref".into(),
            Instr::Deref => "deref".into(),
            Instr::Assign => "assign".into(),
        }
    }
}

/// Um programa: o ponto de entrada da máquina.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub code: Code,
}

impl Program {
    pub fn new(instructions: Vec<Instr>) -> Self {
        Self { code: code(instructions) }
    }
}

fn write_code(instructions: &[Instr], depth: usize, out: &mut String) {
    let pad = "  ".repeat(depth);
    for instr in instructions {
        out.push_str(&format!("{pad}{}\n", instr.mnemonic()));
        match instr {
            Instr::Closure(body) => write_code(body, depth + 1, out),
            Instr::Branch(then_code, else_code) => {
                write_code(then_code, depth + 1, out);
                out.push_str(&format!("{pad}else\n"));
                write_code(else_code, depth + 1, out);
            }
            _ => {}
        }
    }
}

/// O desassemblado: uma instrução por linha, código aninhado recuado.
impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = String::new();
        write_code(&self.code, 0, &mut out);
        f.write_str(&out)
    }
}

// =============================================================================
// Valores e ambiente
// =============================================================================

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Unit,
    Closure(Closure),
    Loc(Location),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Closure {
    pub code: Code,
    pub env: Env,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Unit => f.write_str("()"),
            Value::Closure(_) => f.write_str("<closure>"),
            Value::Loc(l) => write!(f, "{l}"),
        }
    }
}

/// O ambiente: lista encadeada persistente, sem nomes. O índice 0 é o
/// binding mais recente. `extend` é O(1) e não altera o original, então
/// closures compartilham ambientes sem copiar.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Env(Option<Rc<EnvNode>>);

#[derive(Debug, PartialEq, Eq)]
struct EnvNode {
    value: Value,
    parent: Env,
}

impl Env {
    pub fn empty() -> Self {
        Env(None)
    }

    pub fn extend(&self, value: Value) -> Env {
        Env(Some(Rc::new(EnvNode { value, parent: self.clone() })))
    }

    pub fn get(&self, index: usize) -> Option<&Value> {
        let mut node = self.0.as_deref()?;
        for _ in 0..index {
            node = node.parent.0.as_deref()?;
        }
        Some(&node.value)
    }
}

// =============================================================================
// A máquina
// =============================================================================

/// Retorno pendente de uma chamada ou de um ramo.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Frame {
    pub code: Code,
    pub pc: usize,
    pub env: Env,
}

/// A pilha de quadros: uma lista encadeada *persistente*. Empilhar e
/// desempilhar são O(1) e não alteram a pilha original, então clonar uma
/// [`Config`] (que o trace faz a cada passo) não copia os quadros. Com um
/// `Vec`, o trace de um termo divergente usaria memória quadrática.
#[derive(Clone)]
pub struct Frames(Option<Rc<FrameNode>>);

struct FrameNode {
    frame: Frame,
    parent: Frames,
}

impl Frames {
    pub fn empty() -> Self {
        Frames(None)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_none()
    }

    pub fn push(&self, frame: Frame) -> Frames {
        Frames(Some(Rc::new(FrameNode { frame, parent: self.clone() })))
    }

    /// O quadro do topo e o resto da pilha.
    pub fn pop(&self) -> Option<(Frame, Frames)> {
        self.0
            .as_ref()
            .map(|node| (node.frame.clone(), node.parent.clone()))
    }

    pub fn len(&self) -> usize {
        self.iter().count()
    }

    fn iter(&self) -> impl Iterator<Item = &Frame> {
        let mut next = self.0.as_deref();
        std::iter::from_fn(move || {
            let node = next?;
            next = node.parent.0.as_deref();
            Some(&node.frame)
        })
    }
}

impl Default for Frames {
    fn default() -> Self {
        Self::empty()
    }
}

impl PartialEq for Frames {
    fn eq(&self, other: &Self) -> bool {
        match (&self.0, &other.0) {
            (None, None) => true,
            (Some(a), Some(b)) if Rc::ptr_eq(a, b) => true, // cauda compartilhada
            _ => self.iter().eq(other.iter()),
        }
    }
}

impl Eq for Frames {}

impl std::fmt::Debug for Frames {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

/// Destruição iterativa: o `Drop` recursivo padrão de uma lista longa de
/// `Rc` estoura a pilha (um laço de 10 000 chamadas basta).
impl Drop for Frames {
    fn drop(&mut self) {
        let mut current = self.0.take();
        while let Some(rc) = current {
            match Rc::try_unwrap(rc) {
                Ok(mut node) => current = node.parent.0.take(),
                Err(_) => break, // o resto da lista é compartilhado
            }
        }
    }
}

/// Uma configuração: `⟨código, pc, pilha, ambiente, quadros, memória⟩`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Config {
    pub code: Code,
    pub pc: usize,
    pub stack: Vec<Value>,
    pub env: Env,
    pub frames: Frames,
    pub store: Store<Value>,
}

impl Config {
    /// Salva a continuação atual e passa a executar `code` em `env`.
    fn call(&mut self, code: Code, env: Env) {
        self.frames = self.frames.push(Frame {
            code: self.code.clone(),
            pc: self.pc,
            env: self.env.clone(),
        });
        self.code = code;
        self.pc = 0;
        self.env = env;
    }
}

/// `⟨próxima instrução | pilha⟩`.
impl fmt::Display for Config {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let next = match self.code.get(self.pc) {
            Some(instr) => instr.mnemonic(),
            None if self.frames.is_empty() => "halt".to_string(),
            None => "return".to_string(),
        };
        let stack = self
            .stack
            .iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "⟨{next} | [{stack}]⟩")
    }
}

crate::rules! {
    /// Regras da máquina. Chamadas e retornos são regras próprias. Um estado
    /// é `⟨código, pilha, ambiente, quadros⟩`; `k` é o resto do código.
    pub enum MachineRule {
        Const => "M-Const" {
            [] => r"\langle \mathsf{const}\ c :: k,\ s,\ e,\ f \rangle \to \langle k,\ c :: s,\ e,\ f \rangle"
        },
        Prim => "M-Prim" {
            [] => r"\langle \mathsf{prim}\ \oplus :: k,\ v_2 :: v_1 :: s,\ e,\ f \rangle \to \langle k,\ (v_1 \oplus v_2) :: s,\ e,\ f \rangle"
        },
        Access => "M-Access" {
            [] => r"\langle \mathsf{access}\ n :: k,\ s,\ e,\ f \rangle \to \langle k,\ e(n) :: s,\ e,\ f \rangle"
        },
        Closure => "M-Closure" {
            [] => r"\langle \mathsf{closure}\ c' :: k,\ s,\ e,\ f \rangle \to \langle k,\ \langle c', e \rangle :: s,\ e,\ f \rangle"
        },
        Apply => "M-Apply" {
            [] => r"\langle \mathsf{apply} :: k,\ v :: \langle c', e' \rangle :: s,\ e,\ f \rangle \to \langle c',\ s,\ v :: e',\ (k, e) :: f \rangle"
        },
        Return => "M-Return" {
            [] => r"\langle [\,],\ s,\ e,\ (k, e') :: f \rangle \to \langle k,\ s,\ e',\ f \rangle"
        },
        Branch => "M-Branch" {
            [] => r"\langle \mathsf{branch}(c_1, c_2) :: k,\ \mathsf{true} :: s,\ e,\ f \rangle \to \langle c_1,\ s,\ e,\ (k, e) :: f \rangle"
        },
        Ref => "M-Ref" {
            [] => r"\langle \mathsf{ref} :: k,\ v :: s,\ e,\ f,\ \mu \rangle \to \langle k,\ l :: s,\ e,\ f,\ \mu[l \mapsto v] \rangle"
        },
        Deref => "M-Deref" {
            [] => r"\langle \mathsf{deref} :: k,\ l :: s,\ e,\ f,\ \mu \rangle \to \langle k,\ \mu(l) :: s,\ e,\ f,\ \mu \rangle"
        },
        Assign => "M-Assign" {
            [] => r"\langle \mathsf{assign} :: k,\ v :: l :: s,\ e,\ f,\ \mu \rangle \to \langle k,\ \mathsf{unit} :: s,\ e,\ f,\ \mu[l \mapsto v] \rangle"
        },
    }
}

/// A máquina virtual.
pub struct Vm;

impl Step for Vm {
    type State = Config;
    type Rule = MachineRule;

    /// Terminou: o código acabou e não há retornos pendentes.
    fn is_final(config: &Config) -> bool {
        config.pc >= config.code.len() && config.frames.is_empty()
    }

    fn step(config: &Config) -> Option<Transition<MachineRule, Config>> {
        // Cada passo trabalha sobre uma cópia: se uma pré-condição falhar
        // no meio (`?`), o estado original fica intacto.
        let mut next = config.clone();

        let rule = match config.code.get(config.pc) {
            // Fim do código: volta ao chamador.
            None => {
                let (frame, rest) = next.frames.pop()?;
                next.frames = rest;
                next.code = frame.code;
                next.pc = frame.pc;
                next.env = frame.env;
                MachineRule::Return
            }

            Some(instr) => {
                next.pc += 1;

                match instr {
                    Instr::Int(n) => {
                        next.stack.push(Value::Int(*n));
                        MachineRule::Const
                    }
                    Instr::Bool(b) => {
                        next.stack.push(Value::Bool(*b));
                        MachineRule::Const
                    }
                    Instr::Unit => {
                        next.stack.push(Value::Unit);
                        MachineRule::Const
                    }

                    Instr::Prim(op) => {
                        let rhs = next.stack.pop()?;
                        let lhs = next.stack.pop()?;
                        next.stack.push(op.apply(lhs, rhs)?);
                        MachineRule::Prim
                    }

                    Instr::Access(index) => {
                        let value = next.env.get(*index)?.clone();
                        next.stack.push(value);
                        MachineRule::Access
                    }

                    Instr::Closure(body) => {
                        let closure = Closure {
                            code: body.clone(),
                            env: next.env.clone(),
                        };
                        next.stack.push(Value::Closure(closure));
                        MachineRule::Closure
                    }

                    Instr::Apply => {
                        let argument = next.stack.pop()?;
                        let Value::Closure(callee) = next.stack.pop()? else {
                            return None; // aplicar um não-função: travado
                        };
                        next.call(callee.code, callee.env.extend(argument));
                        MachineRule::Apply
                    }

                    Instr::Branch(then_code, else_code) => {
                        let Value::Bool(condition) = next.stack.pop()? else {
                            return None; // condição não booleana: travado
                        };
                        let chosen = if condition { then_code } else { else_code };
                        let env = next.env.clone();
                        next.call(chosen.clone(), env);
                        MachineRule::Branch
                    }

                    Instr::Ref => {
                        let value = next.stack.pop()?;
                        let location = next.store.alloc(value);
                        next.stack.push(Value::Loc(location));
                        MachineRule::Ref
                    }

                    Instr::Deref => {
                        let Value::Loc(location) = next.stack.pop()? else {
                            return None;
                        };
                        let value = next.store.read(location).ok()?.clone();
                        next.stack.push(value);
                        MachineRule::Deref
                    }

                    Instr::Assign => {
                        let value = next.stack.pop()?;
                        let Value::Loc(location) = next.stack.pop()? else {
                            return None;
                        };
                        next.store.write(location, value).ok()?;
                        next.stack.push(Value::Unit);
                        MachineRule::Assign
                    }
                }
            }
        };

        Some(Transition::new(rule, config.clone(), next))
    }
}

impl Machine for Vm {
    type Term = Program;
    type Value = Value;

    fn load(program: &Program) -> Config {
        Config {
            code: program.code.clone(),
            pc: 0,
            stack: Vec::new(),
            env: Env::empty(),
            frames: Frames::empty(),
            store: Store::new(),
        }
    }

    /// Só há valor numa configuração final com exatamente um valor na
    /// pilha; sobras indicam um erro do compilador.
    fn unload(config: &Config) -> Option<Value> {
        if !Self::is_final(config) {
            return None;
        }
        match config.stack.as_slice() {
            [only] => Some(only.clone()),
            _ => None,
        }
    }
}

// =============================================================================
// Compilação
// =============================================================================

/// O que uma linguagem implementa para ser executada no [`Vm`]:
/// a tradução (com os tipos apagados) e a relação entre valores.
pub trait Compile {
    type Source;
    /// O tipo de valor da semântica natural da linguagem.
    type Value;
    type Error: std::error::Error;

    fn compile(term: &Self::Source) -> Result<Program, Self::Error>;

    /// O valor da máquina representa o valor da linguagem? Para closures,
    /// basta que ambos sejam funções: não há igualdade extensional.
    fn corresponds(machine: &Value, source: &Self::Value) -> bool;
}

/// Correção da compilação: se `t ⇓ v`, a máquina termina com um valor
/// que corresponde a `v`; se `t` não tem derivação (travou ou deu erro),
/// a máquina não produz valor.
///
/// Só vale para termos que terminam (big-step não responde para os que
/// divergem) e supõe que o erro de `B` represente só travamento.
pub fn compilation_is_correct<C, B>(term: &C::Source) -> bool
where
    C: Compile,
    B: BigStep<Term = C::Source, Value = C::Value>,
{
    let expected = B::value_of(term);
    let actual = C::compile(term)
        .ok()
        .and_then(|program| Vm::execute(&program).value);

    match (expected, actual) {
        (Ok(source), Some(machine)) => C::corresponds(&machine, &source),
        (Err(_), None) => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::semantics::run;
    use crate::common::semantics::toy::{self, ToyBigStep};

    fn value_of(instructions: Vec<Instr>) -> Option<Value> {
        Vm::execute(&Program::new(instructions)).value
    }

    fn is_stuck(instructions: Vec<Instr>) -> bool {
        run::<Vm>(Vm::load(&Program::new(instructions))).is_stuck()
    }

    #[test]
    fn constants_and_primitives() {
        let run = Vm::execute(&Program::new(vec![
            Instr::Int(1),
            Instr::Int(2),
            Instr::Prim(Prim::Add),
        ]));

        assert_eq!(run.value, Some(Value::Int(3)));
        assert_eq!(
            run.trace.rules(),
            vec![MachineRule::Const, MachineRule::Const, MachineRule::Prim]
        );
    }

    #[test]
    fn identity_application() {
        // (λx. x) 5
        let run = Vm::execute(&Program::new(vec![
            Instr::Closure(code(vec![Instr::Access(0)])),
            Instr::Int(5),
            Instr::Apply,
        ]));

        assert_eq!(run.value, Some(Value::Int(5)));
        assert_eq!(
            run.trace.rules(),
            vec![
                MachineRule::Closure,
                MachineRule::Const,
                MachineRule::Apply,
                MachineRule::Access,
                MachineRule::Return,
            ]
        );
    }

    #[test]
    fn closures_capture_their_environment() {
        // (λx. λy. x) 1 2
        let outer = code(vec![Instr::Closure(code(vec![Instr::Access(1)]))]);

        assert_eq!(
            value_of(vec![
                Instr::Closure(outer),
                Instr::Int(1),
                Instr::Apply,
                Instr::Int(2),
                Instr::Apply,
            ]),
            Some(Value::Int(1))
        );
    }

    #[test]
    fn branch_selects_by_condition() {
        let branch = |condition| {
            value_of(vec![
                Instr::Bool(condition),
                Instr::Branch(code(vec![Instr::Int(1)]), code(vec![Instr::Int(2)])),
            ])
        };

        assert_eq!(branch(true), Some(Value::Int(1)));
        assert_eq!(branch(false), Some(Value::Int(2)));
    }

    #[test]
    fn references_have_a_store() {
        // let x = ref 1 in (x := 2; !x)
        //   = (λx. (λ_. !x) (x := 2)) (ref 1)
        let inner = code(vec![Instr::Access(1), Instr::Deref]);
        let body = code(vec![
            Instr::Closure(inner),
            Instr::Access(0),
            Instr::Int(2),
            Instr::Assign,
            Instr::Apply,
        ]);

        let execution = Vm::execute(&Program::new(vec![
            Instr::Closure(body),
            Instr::Int(1),
            Instr::Ref,
            Instr::Apply,
        ]));

        assert_eq!(execution.value, Some(Value::Int(2)));

        let store = &execution.trace.final_state.store;
        assert_eq!(store.len(), 1);
        assert_eq!(store.iter().next().map(|(_, v)| v), Some(&Value::Int(2)));
    }

    #[test]
    fn ill_formed_programs_get_stuck() {
        assert!(is_stuck(vec![Instr::Prim(Prim::Add)])); // pilha vazia
        assert!(is_stuck(vec![Instr::Int(1), Instr::Bool(true), Instr::Prim(Prim::Add)]));
        assert!(is_stuck(vec![Instr::Int(1), Instr::Int(2), Instr::Apply])); // não é função
        assert!(is_stuck(vec![Instr::Access(0)])); // variável solta
        assert!(is_stuck(vec![
            Instr::Int(1),
            Instr::Branch(code(vec![]), code(vec![])), // condição não booleana
        ]));
        assert!(is_stuck(vec![Instr::Int(1), Instr::Deref]));
        assert!(is_stuck(vec![Instr::Unit, Instr::Int(1), Instr::Assign]));
    }

    #[test]
    fn omega_runs_out_of_fuel() {
        // (λx. x x) (λx. x x)
        let w = code(vec![Instr::Access(0), Instr::Access(0), Instr::Apply]);
        let program = Program::new(vec![
            Instr::Closure(w.clone()),
            Instr::Closure(w),
            Instr::Apply,
        ]);

        let execution = Vm::execute_with_fuel(&program, 200);

        assert!(execution.trace.is_out_of_fuel());
        assert_eq!(execution.value, None);
    }

    #[test]
    fn unload_needs_a_final_configuration_with_one_value() {
        let program = Program::new(vec![Instr::Int(1), Instr::Int(2)]);

        // inicial: não é final
        assert_eq!(Vm::unload(&Vm::load(&program)), None);
        // final, mas com duas sobras na pilha
        assert_eq!(Vm::execute(&program).value, None);
    }

    #[test]
    fn frames_are_persistent_and_drop_iteratively() {
        let frame = |pc| Frame { code: code(vec![]), pc, env: Env::empty() };

        let base = Frames::empty().push(frame(1));
        let a = base.push(frame(2));
        let b = base.push(frame(3));

        assert_eq!(base.len(), 1);
        assert_eq!(a.pop().map(|(f, _)| f.pc), Some(2));
        assert_eq!(b.pop().map(|(f, rest)| (f.pc, rest.len())), Some((3, 1)));
        assert!(a != b);
        assert!(a == a.clone());

        // uma pilha muito funda não pode estourar a pilha ao ser destruída
        let mut deep = Frames::empty();
        for i in 0..200_000 {
            deep = deep.push(frame(i));
        }
        assert_eq!(deep.len(), 200_000);
        drop(deep);
    }

    #[test]
    fn omega_traces_stay_cheap() {
        // 10 000 passos de um termo divergente: com `Vec` de quadros isto
        // levava segundos e centenas de MB.
        let w = code(vec![Instr::Access(0), Instr::Access(0), Instr::Apply]);
        let program = Program::new(vec![
            Instr::Closure(w.clone()),
            Instr::Closure(w),
            Instr::Apply,
        ]);

        let started = std::time::Instant::now();
        let execution = Vm::execute_with_fuel(&program, 10_000);

        assert!(execution.trace.is_out_of_fuel());
        assert!(started.elapsed().as_secs() < 5);
    }

    #[test]
    fn disassembly_indents_nested_code() {
        let program = Program::new(vec![
            Instr::Closure(code(vec![Instr::Access(0)])),
            Instr::Int(5),
            Instr::Apply,
        ]);

        assert_eq!(program.to_string(), "closure\n  access 0\nint 5\napply\n");
    }

    #[test]
    fn configurations_render_for_traces() {
        let config = Vm::load(&Program::new(vec![Instr::Int(1)]));
        assert_eq!(config.to_string(), "⟨int 1 | []⟩");
    }

    // --- a lei, com a linguagem de brinquedo ----------------------------------

    struct ToyCompiler;

    impl Compile for ToyCompiler {
        type Source = toy::Term;
        type Value = toy::Value;
        type Error = std::convert::Infallible;

        fn compile(term: &toy::Term) -> Result<Program, Self::Error> {
            fn go(term: &toy::Term, out: &mut Vec<Instr>) {
                match term {
                    toy::Term::Num(n) => out.push(Instr::Int(*n)),
                    toy::Term::Bool(b) => out.push(Instr::Bool(*b)),
                    toy::Term::Add(l, r) => {
                        go(l, out);
                        go(r, out);
                        out.push(Instr::Prim(Prim::Add));
                    }
                }
            }

            let mut out = Vec::new();
            go(term, &mut out);
            Ok(Program::new(out))
        }

        fn corresponds(machine: &Value, source: &toy::Value) -> bool {
            match (machine, source) {
                (Value::Int(a), toy::Value::Num(b)) => a == b,
                (Value::Bool(a), toy::Value::Bool(b)) => a == b,
                _ => false,
            }
        }
    }

    #[test]
    fn compiling_the_toy_language_is_correct() {
        use toy::{add, boolean, num};

        let samples = vec![
            num(1),
            boolean(true),
            add(num(1), num(2)),
            add(add(num(1), num(2)), add(num(3), num(4))),
            add(num(1), boolean(true)),
            add(boolean(false), num(1)),
            add(add(num(1), num(2)), boolean(true)),
        ];

        for term in &samples {
            assert!(
                compilation_is_correct::<ToyCompiler, ToyBigStep>(term),
                "failed on {term}"
            );
        }
    }
}
