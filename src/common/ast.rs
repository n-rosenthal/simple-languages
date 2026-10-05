//! `src/common/ast.rs` - Representação genérica de uma árvore sintática.
//!
//! A AST concreta de cada linguagem é convertida para [`AstNode`].
//! O módulo não conhece nenhuma linguagem específica.

use crate::common::source::Span;

/// Um nó de uma árvore sintática.
///
/// [`AstNode`] é uma representação intermediária e independente da
/// linguagem concreta. Cada linguagem implementa [`ToAst`] para converter
/// seu próprio `Term` em uma árvore genérica.
///
/// A árvore pode ser usada para:
///
/// - visualização textual;
/// - visualização gráfica na aplicação web;
/// - serialização para JSON;
/// - inspeção/debugging da estrutura sintática;
/// - associação entre nós da AST e posições no código-fonte.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstNode {
    /// Nome estrutural do nó.
    ///
    /// Exemplos:
    ///
    /// - `Binary`
    /// - `Integer`
    /// - `Boolean`
    /// - `If`
    /// - `Lambda`
    /// - `Application`
    pub kind: String,

    /// Texto curto usado para identificar o nó.
    ///
    /// Exemplos:
    ///
    /// - `+`
    /// - `42`
    /// - `true`
    /// - `if`
    /// - `x`
    pub label: Option<String>,

    /// Filhos do nó.
    pub children: Vec<AstNode>,

    /// Localização correspondente no source original, quando disponível.
    pub span: Option<Span>,
}

impl AstNode {
    /// Cria um nó sem label e sem filhos.
    pub fn new(kind: impl Into<String>) -> Self {
        Self {
            kind: kind.into(),
            label: None,
            children: Vec::new(),
            span: None,
        }
    }

    /// Cria um nó com label e sem filhos.
    pub fn labeled(
        kind: impl Into<String>,
        label: impl Into<String>,
    ) -> Self {
        Self {
            kind: kind.into(),
            label: Some(label.into()),
            children: Vec::new(),
            span: None,
        }
    }

    /// Cria um nó com filhos.
    pub fn with_children(
        kind: impl Into<String>,
        children: impl IntoIterator<Item = AstNode>,
    ) -> Self {
        Self {
            kind: kind.into(),
            label: None,
            children: children.into_iter().collect(),
            span: None,
        }
    }

    /// Cria um nó com label e filhos.
    pub fn labeled_with_children(
        kind: impl Into<String>,
        label: impl Into<String>,
        children: impl IntoIterator<Item = AstNode>,
    ) -> Self {
        Self {
            kind: kind.into(),
            label: Some(label.into()),
            children: children.into_iter().collect(),
            span: None,
        }
    }

    /// Define a posição do nó no source.
    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    /// Define a posição do nó no source, quando disponível.
    pub fn with_optional_span(mut self, span: Option<Span>) -> Self {
        self.span = span;
        self
    }

    /// Renderiza a árvore em uma representação textual.
    ///
    /// Exemplo:
    ///
    /// ```text
    /// Binary [+]
    /// ├── Integer [1]
    /// └── Binary [*]
    ///     ├── Integer [2]
    ///     └── Integer [3]
    /// ```
    pub fn render(&self) -> String {
        let mut out = String::new();

        self.render_into(&mut out, "", true);

        out
    }

    fn render_into(
        &self,
        out: &mut String,
        prefix: &str,
        last: bool,
    ) {
        out.push_str(prefix);

        if !prefix.is_empty() {
            out.push_str(if last {
                "└── "
            } else {
                "├── "
            });
        }

        out.push_str(&self.display_label());
        out.push('\n');

        let child_prefix = if prefix.is_empty() {
            String::new()
        } else if last {
            format!("{prefix}    ")
        } else {
            format!("{prefix}│   ")
        };

        for (index, child) in self.children.iter().enumerate() {
            child.render_into(
                out,
                &child_prefix,
                index + 1 == self.children.len(),
            );
        }
    }

    fn display_label(&self) -> String {
        match &self.label {
            Some(label) => {
                format!("{} [{}]", self.kind, label)
            }
            None => self.kind.clone(),
        }
    }
}

/// Converte uma estrutura sintática concreta para a AST genérica.
///
/// Cada linguagem implementa esse trait para seu próprio `Term`.
///
/// Por exemplo:
///
/// ```ignore
/// impl ToAst for Term {
///     fn to_ast(&self) -> AstNode {
///         // ...
///     }
/// }
/// ```
///
/// Assim, qualquer termo pode ser convertido com:
///
/// ```ignore
/// let ast = term.to_ast();
/// ```
pub trait ToAst {
    /// Converte este valor para um [`AstNode`].
    fn to_ast(&self) -> AstNode;
}

/// Permite chamar [`ToAst::to_ast`] através de uma referência.
impl<T: ToAst + ?Sized> ToAst for &T {
    fn to_ast(&self) -> AstNode {
        (**self).to_ast()
    }
}

/// Permite chamar [`ToAst::to_ast`] através de um `Box`.
impl<T: ToAst + ?Sized> ToAst for Box<T> {
    fn to_ast(&self) -> AstNode {
        (**self).to_ast()
    }
}