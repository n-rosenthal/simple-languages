//! Documentos de saída: uma resposta do interpretador como uma lista de blocos.
//!
//! O terminal imprime o `text` de cada bloco; a página web renderiza os blocos
//! `Math` com o KaTeX (`web`) e oferece o LaTeX para `pdflatex` (`tex`).

/// Um pedaço da resposta do interpretador.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Block {
    /// Um título de seção (`type`, `small-step`, ...).
    Heading(String),
    /// Texto pré-formatado (código de máquina, traces da máquina, leis).
    Text(String),
    /// Um erro de um estágio (`type error: ...`).
    Error(String),
    /// Uma fórmula, com três visões.
    Math {
        /// A versão em texto puro (o que o terminal imprime).
        text: String,
        /// LaTeX para o KaTeX, que não tem `mathpartir` nem `\textsc`.
        web: String,
        /// LaTeX para `pdflatex` (`mathpartir`).
        tex: String,
    },
}

impl Block {
    pub fn math(text: impl Into<String>, web: impl Into<String>, tex: impl Into<String>) -> Self {
        Block::Math { text: text.into(), web: web.into(), tex: tex.into() }
    }

    /// `"heading"`, `"text"`, `"error"` ou `"math"`.
    pub fn kind(&self) -> &'static str {
        match self {
            Block::Heading(_) => "heading",
            Block::Text(_) => "text",
            Block::Error(_) => "error",
            Block::Math { .. } => "math",
        }
    }

    pub fn text(&self) -> &str {
        match self {
            Block::Heading(text) | Block::Text(text) | Block::Error(text) => text,
            Block::Math { text, .. } => text,
        }
    }

    /// O LaTeX para o KaTeX; vazio fora dos blocos `Math`.
    pub fn web(&self) -> &str {
        match self {
            Block::Math { web, .. } => web,
            _ => "",
        }
    }

    /// O LaTeX para `pdflatex`; vazio fora dos blocos `Math`.
    pub fn tex(&self) -> &str {
        match self {
            Block::Math { tex, .. } => tex,
            _ => "",
        }
    }
}

/// Os blocos como texto, no formato do terminal: cada título abre uma seção
/// (`== título ==`), e uma linha em branco separa as seções.
pub fn blocks_to_text(blocks: &[Block]) -> String {
    let mut out = String::new();
    let mut in_section = false;

    for block in blocks {
        match block {
            Block::Heading(title) => {
                if in_section {
                    out.push('\n');
                }
                in_section = true;
                out.push_str(&format!("== {title} ==\n"));
            }
            other => {
                out.push_str(other.text());
                if !out.ends_with('\n') {
                    out.push('\n');
                }
            }
        }
    }

    if in_section {
        out.push('\n');
    }
    out
}
