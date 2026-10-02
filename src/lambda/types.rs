use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// Um tipo base, como `Bool` ou `Nat`.
    Base(String),
    /// Tipo função `A -> B`.
    Arrow(Box<Type>, Box<Type>),
}

impl Type {
    pub fn base(name: impl Into<String>) -> Self {
        Self::Base(name.into())
    }

    pub fn arrow(from: Type, to: Type) -> Self {
        Self::Arrow(Box::new(from), Box::new(to))
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Base(name) => write!(f, "{name}"),
            // `->` é associativo à direita: só o lado esquerdo precisa de parênteses.
            Self::Arrow(from, to) => match **from {
                Type::Arrow(..) => write!(f, "({from})->{to}"),
                _ => write!(f, "{from}->{to}"),
            },
        }
    }
}
