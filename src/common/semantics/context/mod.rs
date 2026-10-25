pub mod traits;
pub mod linked;
pub mod map;

pub use traits::{Context, IndexedContext};
pub use linked::LinkedContext;
pub use map::MapContext;

/// Contexto padrão para tipagem e ambientes: persistente, com shadowing
/// e índices de De Bruijn. Use este alias onde antes havia o `Context<B>`
/// concreto (por exemplo em `Typed`), para trocar a representação num só lugar.
pub type DefaultContext<B> = LinkedContext<B>;
