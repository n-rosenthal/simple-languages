use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// O tipo dos booleanos, `Bool`.
    Bool,
    /// Um tipo base abstrato, como `A` ou `B`: não tem constantes.
    Base(String),
    /// Tipo função `A -> B`.
    Arrow(Box<Type>, Box<Type>),
}

impl Type {
    /// `Bool` é o tipo dos booleanos; qualquer outro nome é um tipo base
    /// abstrato (TAPL, cap. 9.1: um conjunto `B` de tipos base).
    pub fn base(name: impl Into<String>) -> Self {
        let name = name.into();
        if name == "Bool" {
            Self::Bool
        } else {
            Self::Base(name)
        }
    }

    pub fn arrow(from: Type, to: Type) -> Self {
        Self::Arrow(Box::new(from), Box::new(to))
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool => write!(f, "Bool"),
            Self::Base(name) => write!(f, "{name}"),
            // `->` é associativo à direita: só o lado esquerdo precisa de parênteses.
            Self::Arrow(from, to) => match **from {
                Type::Arrow(..) => write!(f, "({from})->{to}"),
                _ => write!(f, "{from}->{to}"),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bool_is_special_and_other_names_are_abstract() {
        assert_eq!(Type::base("Bool"), Type::Bool);
        assert_eq!(Type::base("A"), Type::Base("A".into()));
    }

    #[test]
    fn display_parenthesizes_only_the_left_of_an_arrow() {
        let a = || Type::base("A");
        assert_eq!(Type::arrow(a(), Type::arrow(a(), a())).to_string(), "A->A->A");
        assert_eq!(Type::arrow(Type::arrow(a(), a()), a()).to_string(), "(A->A)->A");
        assert_eq!(Type::arrow(Type::Bool, Type::Bool).to_string(), "Bool->Bool");
    }
}
