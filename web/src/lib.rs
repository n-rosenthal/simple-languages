//! Cola entre o interpretador (`simple_languages`) e o JavaScript.
//!
//! Toda a lógica vive no crate principal ([`registry::session`] e o trait
//! [`Interpreter`]); aqui só há conversões de tipos. As funções puras ficam
//! fora das `impl` de `#[wasm_bindgen]` para poderem ser testadas no alvo
//! nativo, onde os tipos de `JsValue` não funcionam.

use std::str::FromStr;

use simple_languages::common::document::Block;
use simple_languages::common::driver::Command;
use simple_languages::common::interpreter::{Interpreter, Reply as CoreReply};
use simple_languages::common::language::Example;
use simple_languages::registry;
use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
pub fn start() {
    // Sem isto, um pânico vira um `unreachable` mudo; com isto, aparece no console.
    console_error_panic_hook::set_once();
}

/// Os nomes das linguagens disponíveis.
#[wasm_bindgen]
pub fn languages() -> Vec<String> {
    registry::names().into_iter().map(String::from).collect()
}

/// Os nomes dos modos (`full`, `type`, `small`, ...).
#[wasm_bindgen]
pub fn modes() -> Vec<String> {
    Command::ALL.iter().map(|c| c.name().to_string()).collect()
}

/// A resposta a uma linha digitada.
#[wasm_bindgen]
pub struct Reply {
    kind: String,
    text: String,
    blocks: Vec<String>,
    quit: bool,
}

#[wasm_bindgen]
impl Reply {
    /// `"output"`, `"error"` ou `"info"`.
    #[wasm_bindgen(getter)]
    pub fn kind(&self) -> String {
        self.kind.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn text(&self) -> String {
        self.text.clone()
    }

    /// A resposta em blocos, achatada: `[tipo, texto, web, tex, ...]`, quatro
    /// strings por bloco. O tipo é `"heading"`, `"text"`, `"error"` ou `"math"`;
    /// `web` é o LaTeX para o KaTeX e `tex`, o para `pdflatex` (só em `"math"`).
    /// Vazio nas respostas que são só uma mensagem: use então `text`.
    #[wasm_bindgen(getter)]
    pub fn blocks(&self) -> Vec<String> {
        self.blocks.clone()
    }

    /// O usuário digitou `:quit`.
    #[wasm_bindgen(getter)]
    pub fn quit(&self) -> bool {
        self.quit
    }
}

impl From<CoreReply> for Reply {
    fn from(reply: CoreReply) -> Self {
        Reply {
            kind: reply.kind.name().to_string(),
            text: reply.text,
            blocks: flatten_blocks(&reply.blocks),
            quit: reply.quit,
        }
    }
}

/// Uma sessão interativa de uma linguagem.
#[wasm_bindgen]
pub struct Playground {
    inner: Box<dyn Interpreter>,
}

impl Playground {
    /// Abre uma sessão; `None` se a linguagem não existe.
    pub fn open(language: &str) -> Option<Playground> {
        registry::session(language).map(|inner| Playground { inner })
    }

    pub fn change_mode(&mut self, name: &str) -> Result<(), String> {
        self.inner.set_mode(Command::from_str(name)?);
        Ok(())
    }
}

#[wasm_bindgen]
impl Playground {
    #[wasm_bindgen(constructor)]
    pub fn new(language: &str) -> Result<Playground, JsError> {
        Playground::open(language)
            .ok_or_else(|| JsError::new(&format!("unknown language `{language}`")))
    }

    pub fn language(&self) -> String {
        self.inner.language().to_string()
    }

    pub fn description(&self) -> String {
        self.inner.description().to_string()
    }

    #[wasm_bindgen(js_name = supportsDefinitions)]
    pub fn supports_definitions(&self) -> bool {
        self.inner.supports_definitions()
    }

    pub fn mode(&self) -> String {
        self.inner.mode().name().to_string()
    }

    #[wasm_bindgen(js_name = setMode)]
    pub fn set_mode(&mut self, name: &str) -> Result<(), JsError> {
        self.change_mode(name).map_err(|message| JsError::new(&message))
    }

    pub fn fuel(&self) -> usize {
        self.inner.fuel()
    }

    /// Limita o valor ao intervalo aceito e devolve o valor efetivo.
    #[wasm_bindgen(js_name = setFuel)]
    pub fn set_fuel(&mut self, fuel: usize) -> usize {
        self.inner.set_fuel(fuel)
    }

    /// Executa uma linha: um termo, uma definição ou um comando `:...`.
    pub fn submit(&mut self, line: &str) -> Reply {
        self.inner.submit(line).into()
    }

    /// Os exemplos, achatados: `[título, fonte, título, fonte, ...]`.
    pub fn examples(&self) -> Vec<String> {
        flatten_examples(self.inner.examples())
    }

    /// A gramática, em blocos achatados (ver `Reply.blocks`).
    pub fn syntax(&self) -> Vec<String> {
        flatten_blocks(&self.inner.syntax())
    }

    /// As regras (tipagem, semânticas, máquina), em blocos achatados.
    pub fn rules(&self) -> Vec<String> {
        flatten_blocks(&self.inner.rules())
    }

    /// As definições, achatadas: `[nome, termo, nome, termo, ...]`.
    pub fn definitions(&self) -> Vec<String> {
        flatten_pairs(self.inner.definitions())
    }
}

fn flatten_blocks(blocks: &[Block]) -> Vec<String> {
    blocks
        .iter()
        .flat_map(|b| {
            [b.kind().to_string(), b.text().to_string(), b.web().to_string(), b.tex().to_string()]
        })
        .collect()
}

fn flatten_examples(examples: &[Example]) -> Vec<String> {
    examples
        .iter()
        .flat_map(|e| [e.title.to_string(), e.source.to_string()])
        .collect()
}

fn flatten_pairs(pairs: Vec<(String, String)>) -> Vec<String> {
    pairs.into_iter().flat_map(|(a, b)| [a, b]).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lists_the_registered_languages_and_modes() {
        assert_eq!(languages(), vec!["arith", "stlc"]);
        assert_eq!(modes().first().map(String::as_str), Some("parse"));
        assert!(modes().contains(&"machine".to_string()));
    }

    #[test]
    fn opens_known_languages_only() {
        assert!(Playground::open("stlc").is_some());
        assert!(Playground::open("nope").is_none());
    }

    #[test]
    fn a_session_round_trip() {
        let mut pg = Playground::open("stlc").unwrap();

        assert_eq!(pg.language(), "stlc");
        assert!(pg.supports_definitions());
        assert_eq!(pg.mode(), "full");

        pg.change_mode("type").unwrap();
        assert_eq!(pg.mode(), "type");
        assert!(pg.change_mode("nope").is_err());

        let reply = pg.submit("λx:A. x");
        assert_eq!(reply.kind(), "output");
        assert!(reply.text().starts_with("type: A->A"));
        assert!(!reply.quit());

        let reply = pg.submit("λx. x");
        assert_eq!(reply.kind(), "error");

        assert!(pg.submit(":quit").quit());
    }

    #[test]
    fn fuel_is_clamped_and_returned() {
        let mut pg = Playground::open("arith").unwrap();
        assert_eq!(pg.set_fuel(0), 1);
        assert_eq!(pg.set_fuel(50), 50);
        assert_eq!(pg.fuel(), 50);
        assert_eq!(pg.set_fuel(usize::MAX), simple_languages::common::interpreter::MAX_FUEL);
    }

    #[test]
    fn examples_and_definitions_are_flattened() {
        let mut pg = Playground::open("stlc").unwrap();

        let examples = pg.examples();
        assert!(!examples.is_empty() && examples.len() % 2 == 0);
        assert_eq!(examples[0], "Identidade");
        assert_eq!(examples[1], "λx:A. x");

        assert!(pg.definitions().is_empty());
        pg.submit("id = λx:A. x");
        assert_eq!(pg.definitions(), vec!["id".to_string(), "λx:A. x".to_string()]);
    }

    #[test]
    fn every_registered_example_runs() {
        for language in languages() {
            let mut pg = Playground::open(&language).unwrap();
            let examples = pg.examples();
            for pair in examples.chunks(2) {
                let reply = pg.submit(&pair[1]);
                assert_ne!(reply.kind(), "error", "{language}: {}", pair[0]);
            }
        }
    }
}
