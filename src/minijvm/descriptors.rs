use itertools::Itertools;
use nom::Finish;

use crate::mappings;
use super::*;

#[derive(Clone, PartialEq, Eq, derive_more::IsVariant, bincode::Encode, bincode::Decode)]
pub enum TypeDescriptorKind {
    /// B
    Byte,
    /// C
    Char,
    /// D
    Double,
    /// F
    Float,
    /// I
    Int,
    /// J
    Long,
    /// S
    Short,
    /// Z
    Boolean,
    /// V
    Void,
    /// Ljava/lang/String; -> FieldDescriptor::Object("java.lang.string")
    Object(IdentPath),
}

impl TypeDescriptorKind {
    pub fn parse(content: &str) -> nom::IResult<&str, Self> {
        use nom::{
            branch::alt,
            character::{ char, none_of },
            combinator::recognize,
            multi::{ many0_count },
            sequence::{ delimited },
            Parser as _
        };

        alt((
            char('B').map(|_| TypeDescriptorKind::Byte),
            char('C').map(|_| TypeDescriptorKind::Char),
            char('D').map(|_| TypeDescriptorKind::Double),
            char('F').map(|_| TypeDescriptorKind::Float),
            char('I').map(|_| TypeDescriptorKind::Int),
            char('J').map(|_| TypeDescriptorKind::Long),
            char('S').map(|_| TypeDescriptorKind::Short),
            char('Z').map(|_| TypeDescriptorKind::Boolean),
            char('V').map(|_| TypeDescriptorKind::Void),
            delimited(
                char('L'),
                recognize(many0_count(none_of(";"))),
                char(';'),
            ).map(|o: &str| TypeDescriptorKind::Object(IdentPath::new(o.replace("/", ".")))),
        )).parse(content)
    }

    pub fn to_mapped(&self, mappings: &mappings::Mappings) -> Option<Self> {
        Some(match self {
            Self::Object(o) => {
                Self::Object(mappings.map_class(&**o)?.name.clone())
            },
            other => other.clone(),
        })
    }
}

impl std::fmt::Debug for TypeDescriptorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeDescriptorKind::Byte => write!(f, "B"),
            TypeDescriptorKind::Char => write!(f, "C"),
            TypeDescriptorKind::Double => write!(f, "D"),
            TypeDescriptorKind::Float => write!(f, "F"),
            TypeDescriptorKind::Int => write!(f, "I"),
            TypeDescriptorKind::Long => write!(f, "J"),
            TypeDescriptorKind::Short => write!(f, "S"),
            TypeDescriptorKind::Boolean => write!(f, "Z"),
            TypeDescriptorKind::Void => write!(f, "V"),
            TypeDescriptorKind::Object(o) => write!(f, "L{};", o.replace(".", "/")),
        }
    }
}

impl std::fmt::Display for TypeDescriptorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeDescriptorKind::Byte => write!(f, "byte"),
            TypeDescriptorKind::Char => write!(f, "char"),
            TypeDescriptorKind::Double => write!(f, "double"),
            TypeDescriptorKind::Float => write!(f, "float"),
            TypeDescriptorKind::Int => write!(f, "int"),
            TypeDescriptorKind::Long => write!(f, "long"),
            TypeDescriptorKind::Short => write!(f, "short"),
            TypeDescriptorKind::Boolean => write!(f, "boolean"),
            TypeDescriptorKind::Void => write!(f, "void"),
            TypeDescriptorKind::Object(o) => write!(f, "{o}"),
        }
    }
}

#[derive(Clone, PartialEq, Eq, bincode::Encode, bincode::Decode)]
pub struct TypeDescriptor {
    pub ty: TypeDescriptorKind,
    pub array_depth: usize,
}

impl TypeDescriptor {
    pub fn parse(content: &str) -> nom::IResult<&str, Self> {
        use nom::{ character::char, multi::many0_count, Parser as _ };

        (many0_count(char('[')), TypeDescriptorKind::parse)
            .map(|(array_depth, ty)| Self { array_depth, ty }).parse(content)
    }

    pub fn parse_complete(content: &str) -> anyhow::Result<Self> {
        use nom::{ Parser as _, combinator::complete };

        Ok(complete(Self::parse).parse(content).finish().map(|(_, v)| v).map_err(|e| e.cloned())?)
    }

    pub fn to_mapped(&self, mappings: &mappings::Mappings) -> Self {
        Self {
            ty: self.ty.to_mapped(mappings).unwrap_or_else(|| self.ty.clone()),
            array_depth: self.array_depth,
        }
    }

    pub fn simple_class_name(&self) -> Option<&IdentPath> {
        match self {
            Self { array_depth: 0, ty: TypeDescriptorKind::Object(h) } => Some(h),
            _ => None,
        }
    }
}

impl std::fmt::Debug for TypeDescriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.array_depth {
            write!(f, "[")?;
        }
        write!(f, "{:?}", self.ty)?;
        Ok(())
    }
}

impl std::fmt::Display for TypeDescriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ty)?;
        for _ in 0..self.array_depth {
            write!(f, "[]")?;
        }
        Ok(())
    }
}

#[derive(Clone, PartialEq, Eq, bincode::Encode, bincode::Decode)]
pub struct MethodDescriptor {
    pub return_type: TypeDescriptor,
    pub args: Vec<TypeDescriptor>,
}

impl MethodDescriptor {
    pub fn parse(content: &str) -> nom::IResult<&str, Self> {
        use nom::{
            character::char,
            sequence::delimited,
            multi::many0,
            Parser as _,
        };

        (delimited(char('('), many0(TypeDescriptor::parse), char(')')), TypeDescriptor::parse)
            .map(|(args, return_type)| Self { args, return_type }).parse(content)
    }

    pub fn parse_complete(content: &str) -> anyhow::Result<Self> {
        use nom::{ Parser as _, combinator::complete };

        Ok(complete(Self::parse).parse(content).finish().map(|(_, v)| v).map_err(|e| e.cloned())?)
    }

    pub fn to_mapped(&self, mappings: &mappings::Mappings) -> Self {
        Self {
            return_type: self.return_type.to_mapped(mappings),
            args: self.args.iter().map(|ty| ty.to_mapped(mappings)).collect(),
        }
    }

    pub fn format_with_name(&self, name: &str) -> String {
        format!("{} {name}({})", self.return_type, self.args.iter()
            .map(ToString::to_string)
            .intersperse(",".to_string())
            .collect::<String>()
        )
    }
}

impl std::fmt::Debug for MethodDescriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for arg in self.args.iter() {
            write!(f, "{arg:?}")?;
        }
        write!(f, ")")?;
        write!(f, "{:?}", self.return_type)?;
        Ok(())
    }
}

impl std::fmt::Display for MethodDescriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} (", self.return_type)?;
        let mut arg_iter = self.args.iter();
        if let Some(first) = arg_iter.next() {
            write!(f, "{first}")?;
        }
        for arg in arg_iter {
            write!(f, ",{arg}")?;
        }
        write!(f, ")")?;
        Ok(())
    }
}
