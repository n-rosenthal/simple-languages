///!    Tipos de `arith-extensions`: inteiros e booleanos.

use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    Integer,
    Boolean,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Type::Integer => "Integer",
            Type::Boolean => "Boolean",
        })
    }
}
