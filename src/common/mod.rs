//! O backbone compartilhado por todas as linguagens.

pub mod source;
pub mod frontend;
pub mod latex;
pub mod context;
pub mod store;
pub mod semantics;
pub mod machine_language;
pub mod language;
pub mod driver;
pub mod interpreter;
pub mod document;

pub use context::Context;
pub use document::Block;
pub use latex::ToLatex;
pub use source::{Lexer, Parser, Scanner, SourceLine, Span};
pub use store::{Dangling, Location, Store, StoreTyping};
