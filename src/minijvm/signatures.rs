//! Based on [https://docs.oracle.com/javase/specs/jvms/se22/html/jvms-4.html#jvms-4.7.9.1]

use std::{fmt::Display, str::FromStr};

use itertools::Itertools;
use nom::{
    branch::alt,
    character::complete::{ char, none_of },
    combinator::{ opt, recognize },
    multi::{ many0, many1, many1_count },
    sequence::{ delimited, preceded, terminated },
    Finish as _, Parser as _
};

use crate::mappings;

#[derive(derive_more::Debug, Clone, PartialEq, Eq, derive_more::Display, bincode::Encode, bincode::Decode)]
#[debug("Identifier({_0:?})")]
pub struct Identifier(String);

impl Identifier {
    const FORBIDDEN_CHARS: &str = ".;[/<>:";

    pub fn new(value: impl Into<String>) -> Self {
        let str = value.into();
        debug_assert!(!str.chars().any(|c| Self::FORBIDDEN_CHARS.contains(c)), "Invalid identifier");
        Self(str)
    }

    pub fn parse(content: &str) -> nom::IResult<&str, Self> {
        recognize(many1_count(none_of(Self::FORBIDDEN_CHARS)))
            .map(String::from).map(Self)
            .parse(content)
    }
}

impl From<String> for Identifier {
    fn from(value: String) -> Self {
        Self::new(value)
    }
}

impl<'a> From<&'a str> for Identifier {
    fn from(value: &'a str) -> Self {
        Self::new(value)
    }
}

impl std::ops::Deref for Identifier {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, derive_more::IsVariant, derive_more::Display, derive_more::From, bincode::Encode, bincode::Decode)]
pub enum JavaTypeSignature {
    Reference(ReferenceTypeSignature),
    Base(BaseTypeSignature),
}

impl JavaTypeSignature {
    pub fn parse(content: &str) -> nom::IResult<&str, Self> {
        alt((
            ReferenceTypeSignature::parse.map(Into::into),
            BaseTypeSignature::parse.map(Into::into),
        )).parse(content)
    }

    pub fn to_mapped(&self, mappings: &mappings::Mappings) -> Self {
        match self {
            Self::Reference(x) => Self::Reference(x.to_mapped(mappings)),
            Self::Base(x)      => Self::Base     (x.clone()),
        }
    }
}

impl FromStr for JavaTypeSignature {
    type Err = nom::error::Error<String>;

    /// Should be used instead of [`JavaTypeSignature::parse`] will always consume
    /// the whole input
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use nom::{ Parser as _, combinator::complete };

        Ok(complete(Self::parse).parse(s).finish().map(|(_, v)| v).map_err(|e| e.cloned())?)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, bincode::Encode, bincode::Decode)]
pub struct ClassSignature {
    pub type_parameters: Vec<TypeParameter>,
    pub superclass: ClassTypeSignature,
    pub superinterfaces: Vec<ClassTypeSignature>,
}

impl ClassSignature {
    pub fn parse(content: &str) -> nom::IResult<&str, Self> {
        (
            opt(delimited(char('<'), many1(TypeParameter::parse), char('>')))
                .map(Option::unwrap_or_default),
            ClassTypeSignature::parse,
            many0(ClassTypeSignature::parse),
        ).map(|(type_parameters, superclass, superinterface)| Self {
            type_parameters,
            superclass,
            superinterfaces: superinterface,
        }).parse(content)
    }

    pub fn to_mapped(&self, mappings: &mappings::Mappings) -> Self {
        Self {
            type_parameters: self.type_parameters.iter()
                .map(|tp| tp.to_mapped(mappings)).collect(),
            superclass: self.superclass.to_mapped(mappings),
            superinterfaces: self.superinterfaces.iter()
                .map(|tp| tp.to_mapped(mappings)).collect(),
        }
    }
}

impl FromStr for ClassSignature {
    type Err = nom::error::Error<String>;

    /// Should be used instead of [`ClassSignature::parse`] will always consume
    /// the whole input
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use nom::{ Parser as _, combinator::complete };

        Ok(complete(Self::parse).parse(s).finish().map(|(_, v)| v).map_err(|e| e.cloned())?)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, bincode::Encode, bincode::Decode)]
pub struct MethodSignature {
    pub type_parameters: Vec<TypeParameter>,
    pub arguments: Vec<JavaTypeSignature>,
    pub result: MethodResult,
    pub throws_signature: Vec<ThrowsSignature>,
}

impl MethodSignature {
    pub fn parse(content: &str) -> nom::IResult<&str, Self> {
        (
            opt(delimited(char('<'), many1(TypeParameter::parse), char('>')))
                .map(Option::unwrap_or_default),
            delimited(char('('), many0(JavaTypeSignature::parse), char(')')),
            MethodResult::parse,
            many0(ThrowsSignature::parse),
        ).map(|(type_parameters, arguments, result, throws_signature)| Self {
            type_parameters, arguments, result, throws_signature,
        }).parse(content)
    }

    pub fn to_mapped(&self, mappings: &mappings::Mappings) -> Self {
        Self {
            type_parameters: self.type_parameters.iter()
                .map(|x| x.to_mapped(mappings)).collect(),
            arguments: self.arguments.iter()
                .map(|x| x.to_mapped(mappings)).collect(),
            result: self.result.to_mapped(mappings),
            throws_signature: self.throws_signature.iter()
                .map(|x| x.to_mapped(mappings)).collect(),
        }
    }
}

impl FromStr for MethodSignature {
    type Err = nom::error::Error<String>;

    /// Should be used instead of [`MethodSignature::parse`] will always consume
    /// the whole input
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use nom::{ Parser as _, combinator::complete };

        Ok(complete(Self::parse).parse(s).finish().map(|(_, v)| v).map_err(|e| e.cloned())?)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, derive_more::IsVariant, derive_more::From, bincode::Encode, bincode::Decode)]
pub enum MethodResult {
    Type(JavaTypeSignature),
    Void,
}

impl MethodResult {
    pub fn parse(content: &str) -> nom::IResult<&str, Self> {
        alt((
            JavaTypeSignature::parse.map(MethodResult::Type),
            char('V').map(|_| MethodResult::Void),
        )).parse(content)
    }

    pub fn to_mapped(&self, mappings: &mappings::Mappings) -> Self {
        match self {
            Self::Type(x) => Self::Type(x.to_mapped(mappings)),
            Self::Void => Self::Void,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, derive_more::IsVariant, derive_more::From, bincode::Encode, bincode::Decode)]
pub enum ThrowsSignature {
    Class(ClassTypeSignature),
    TypeVariable(TypeVariableSignature),
}

impl ThrowsSignature {
    pub fn parse(content: &str) -> nom::IResult<&str, Self> {
        preceded(char('^'), alt((
            ClassTypeSignature::parse.map(|x| Self::Class(x)),
            TypeVariableSignature::parse.map(|x| Self::TypeVariable(x)),
        ))).parse(content)
    }

    pub fn to_mapped(&self, mappings: &mappings::Mappings) -> Self {
        match self {
            Self::Class(x) => Self::Class(x.to_mapped(mappings)),
            Self::TypeVariable(x) => Self::TypeVariable(x.clone()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, bincode::Encode, bincode::Decode)]
pub struct TypeParameter {
    pub name: Identifier,
    pub class_bound: Option<ReferenceTypeSignature>,
    pub interface_bounds: Vec<ReferenceTypeSignature>,
}

impl TypeParameter {
    pub fn parse(content: &str) -> nom::IResult<&str, Self> {
        (
            Identifier::parse,
            preceded(char(':'), opt(ReferenceTypeSignature::parse)),
            many0(preceded(char(':'), ReferenceTypeSignature::parse)),
        ).map(|(name, class_bound, interface_bounds)| Self {
            name, class_bound, interface_bounds
        }).parse(content)
    }

    pub fn to_mapped(&self, mappings: &mappings::Mappings) -> Self {
        Self {
            name: self.name.clone(),
            class_bound: self.class_bound.as_ref().map(|x| x.to_mapped(mappings)),
            interface_bounds: self.interface_bounds.iter()
                .map(|x| x.to_mapped(mappings)).collect(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, derive_more::IsVariant, bincode::Encode, bincode::Decode)]
pub enum BaseTypeSignature {
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
}

impl BaseTypeSignature {
    pub fn parse(content: &str) -> nom::IResult<&str, Self> {
        alt((
            char('B').map(|_| Self::Byte),
            char('C').map(|_| Self::Char),
            char('D').map(|_| Self::Double),
            char('F').map(|_| Self::Float),
            char('I').map(|_| Self::Int),
            char('J').map(|_| Self::Long),
            char('S').map(|_| Self::Short),
            char('Z').map(|_| Self::Boolean),
        )).parse(content)
    }
}

impl std::fmt::Debug for BaseTypeSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Byte => write!(f, "B"),
            Self::Char => write!(f, "C"),
            Self::Double => write!(f, "D"),
            Self::Float => write!(f, "F"),
            Self::Int => write!(f, "I"),
            Self::Long => write!(f, "J"),
            Self::Short => write!(f, "S"),
            Self::Boolean => write!(f, "Z"),
        }
    }
}

impl std::fmt::Display for BaseTypeSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Byte => write!(f, "byte"),
            Self::Char => write!(f, "char"),
            Self::Double => write!(f, "double"),
            Self::Float => write!(f, "float"),
            Self::Int => write!(f, "int"),
            Self::Long => write!(f, "long"),
            Self::Short => write!(f, "short"),
            Self::Boolean => write!(f, "boolean"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, derive_more::Display, derive_more::IsVariant, derive_more::From, bincode::Encode, bincode::Decode)]
pub enum ReferenceTypeSignature {
    Class(ClassTypeSignature),
    TypeVariable(TypeVariableSignature),
    Array(ArrayTypeSignature),
}

impl ReferenceTypeSignature {
    pub fn parse(content: &str) -> nom::IResult<&str, Self> {
        alt((
            ClassTypeSignature::parse.map(Into::into),
            TypeVariableSignature::parse.map(Into::into),
            ArrayTypeSignature::parse.map(Into::into),
        )).parse(content)
    }

    pub fn to_mapped(&self, mappings: &mappings::Mappings) -> Self {
        match self {
            Self::Class(x)        => Self::Class       (x.to_mapped(mappings)),
            Self::TypeVariable(x) => Self::TypeVariable(x.clone()),
            Self::Array(x)        => Self::Array       (x.to_mapped(mappings)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, derive_more::Display, derive_more::IsVariant, bincode::Encode, bincode::Decode)]
pub enum WildcardIndicator {
    #[display("+")]
    Plus,
    #[display("-")]
    Minus,
}

impl WildcardIndicator {
    pub fn parse(content: &str) -> nom::IResult<&str, Self> {
        alt((
            char('+').map(|_| Self::Plus),
            char('-').map(|_| Self::Minus),
        )).parse(content)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, derive_more::Display, bincode::Encode, bincode::Decode)]
pub enum TypeArgument {
    #[display("*")]
    Wildcard,
    #[display("{}{ty}", wildcard_indicator.as_ref().map(ToString::to_string).unwrap_or_default())]
    Type {
        wildcard_indicator: Option<WildcardIndicator>,
        ty: ReferenceTypeSignature,
    },
}

impl TypeArgument {
    pub fn parse(content: &str) -> nom::IResult<&str, Self> {
        alt((
            char('*').map(|_| Self::Wildcard),
            (
                opt(WildcardIndicator::parse),
                ReferenceTypeSignature::parse,
            ).map(|(wildcard_indicator, ty)| Self::Type {
                wildcard_indicator, ty,
            })
        )).parse(content)
    }

    pub fn to_mapped(&self, mappings: &mappings::Mappings) -> Self {
        match self {
            Self::Wildcard => Self::Wildcard,
            Self::Type { wildcard_indicator, ty } => Self::Type {
                wildcard_indicator: wildcard_indicator.clone(),
                ty: ty.to_mapped(mappings),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, bincode::Encode, bincode::Decode)]
pub struct SimpleClassTypeSignature {
    pub name: Identifier,
    pub type_arguments: Vec<TypeArgument>,
}

impl SimpleClassTypeSignature {
    pub fn parse(content: &str) -> nom::IResult<&str, Self> {
        (
            Identifier::parse,
            opt(delimited(char('<'), many1(TypeArgument::parse), char('>')))
                .map(Option::unwrap_or_default),
        ).map(|(name, type_arguments)| Self {
            name,
            type_arguments,
        }).parse(content)
    }

    pub fn to_mapped(&self, mappings: &mappings::Mappings) -> Self {
        Self {
            name: self.name.clone(),
            type_arguments: self.type_arguments.iter()
                .map(|ta| ta.to_mapped(mappings))
                .collect(),
        }
    }
}

impl Display for SimpleClassTypeSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if !self.type_arguments.is_empty() {
            write!(f, "<")?;
            for arg in &self.type_arguments {
                write!(f, "{arg}")?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, bincode::Encode, bincode::Decode)]
pub struct ClassTypeSignature {
    pub package: Vec<Identifier>,
    pub class: SimpleClassTypeSignature,
    pub suffix: Vec<SimpleClassTypeSignature>,
}

impl ClassTypeSignature {
    pub fn class_name(&self) -> String {
        self.package.iter()
            .map(|a| &**a).chain([&*self.class.name])
            .intersperse(".").collect::<String>()
    }

    pub fn parse(content: &str) -> nom::IResult<&str, Self> {
        delimited(
            char('L'),
            (
                many0(terminated(Identifier::parse, char('/'))),
                SimpleClassTypeSignature::parse,
                many0(preceded(char('.'), SimpleClassTypeSignature::parse))
            ).map(|(package, class, suffix)| Self {
                package, class, suffix,
            }),
            char(';'),
        ).parse(content)
    }

    pub fn to_mapped(&self, mappings: &mappings::Mappings) -> Self {
        let class_name = self.class_name();

        let mapped_name_with_package = mappings.map_class(&class_name)
            .map(|class| &*class.name)
            .unwrap_or(class_name.as_str());

        let mut mapped_package = mapped_name_with_package.split('.')
            .map(Identifier::new)
            .collect_vec();
        let mapped_name = mapped_package.pop()
            .expect("Can't be empty");

        // TODO: Mapping this is a bit annoying
        Self {
            package: mapped_package,
            class: SimpleClassTypeSignature {
                name: mapped_name,
                ..self.class.to_mapped(mappings)
            },
            suffix: self.suffix.clone(),
        }
    }
}

impl Display for ClassTypeSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "L")?;
        for p in &self.package {
            write!(f, "{p}/")?;
        }
        write!(f, "{}", self.class)?;
        for s in &self.suffix {
            write!(f, ".{s}")?;
        }
        write!(f, ";")?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, derive_more::Display, bincode::Encode, bincode::Decode)]
#[display("{name}")]
pub struct TypeVariableSignature {
    pub name: Identifier,
}

impl TypeVariableSignature {
    pub fn parse(content: &str) -> nom::IResult<&str, Self> {
        delimited(
            char('T'),
            Identifier::parse,
            char(';')
        ).map(|name| Self { name }).parse(content)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, derive_more::Display, bincode::Encode, bincode::Decode)]
#[display("[{ty}")]
pub struct ArrayTypeSignature {
    pub ty: Box<JavaTypeSignature>,
}

impl ArrayTypeSignature {
    pub fn parse(content: &str) -> nom::IResult<&str, Self> {
        preceded(
            char('['),
            JavaTypeSignature::parse.map(Box::new),
        ).map(|ty| Self { ty }).parse(content)
    }

    pub fn to_mapped(&self, mappings: &mappings::Mappings) -> Self {
        Self {
            ty: Box::new(self.ty.to_mapped(mappings)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identifier() {
        let (rest, id) = Identifier::parse("MyClass;").unwrap();
        assert_eq!(&*id, "MyClass");
        assert_eq!(rest, ";");
    }

    #[test]
    fn test_base_type_signature() {
        assert_eq!(BaseTypeSignature::parse("I").unwrap().1, BaseTypeSignature::Int);
        assert_eq!(BaseTypeSignature::parse("Z").unwrap().1, BaseTypeSignature::Boolean);
        assert_eq!(BaseTypeSignature::parse("J").unwrap().1, BaseTypeSignature::Long);
        assert_eq!(BaseTypeSignature::parse("D").unwrap().1, BaseTypeSignature::Double);
    }

    #[test]
    fn test_class_type_signature_simple() {
        let (_, sig) = ClassTypeSignature::parse("Ljava/lang/String;").unwrap();
        assert_eq!(sig.package.len(), 2);
        assert_eq!(&*sig.class.name, "String");
        assert_eq!(sig.class_name(), "java.lang.String");
    }

    #[test]
    fn test_class_type_signature_generic() {
        let (_, sig) = ClassTypeSignature::parse("Ljava/util/List<Ljava/lang/String;>;").unwrap();
        assert_eq!(sig.class_name(), "java.util.List");
        assert_eq!(sig.class.type_arguments.len(), 1);
    }

    #[test]
    fn test_class_type_signature_nested() {
        let (_, sig) = ClassTypeSignature::parse("Ljava/util/Map<TK;TV;>.Entry<TK;TV;>;").unwrap();
        assert_eq!(sig.class_name(), "java.util.Map");
        assert_eq!(sig.suffix.len(), 1);
        assert_eq!(&*sig.suffix[0].name, "Entry");
    }

    #[test]
    fn test_array_type_signature() {
        let (_, sig) = ArrayTypeSignature::parse("[I").unwrap();
        assert!(sig.ty.is_base());

        let (_, sig) = ArrayTypeSignature::parse("[[Ljava/lang/String;").unwrap();
        assert!(sig.ty.is_reference());
    }

    #[test]
    fn test_wildcard_type_argument() {
        let (_, arg) = TypeArgument::parse("*").unwrap();
        assert!(matches!(arg, TypeArgument::Wildcard));

        let (_, arg) = TypeArgument::parse("+Ljava/lang/Number;").unwrap();
        assert!(matches!(arg, TypeArgument::Type { wildcard_indicator: Some(WildcardIndicator::Plus), .. }));

        let (_, arg) = TypeArgument::parse("-Ljava/lang/Object;").unwrap();
        assert!(matches!(arg, TypeArgument::Type { wildcard_indicator: Some(WildcardIndicator::Minus), .. }));
    }

    #[test]
    fn test_method_signature_simple() {
        let sig: MethodSignature = "(II)I".parse().unwrap();
        assert_eq!(sig.arguments.len(), 2);
        assert!(sig.result.is_type());
        assert!(sig.type_parameters.is_empty());
    }

    #[test]
    fn test_method_signature_void() {
        let sig: MethodSignature = "()V".parse().unwrap();
        assert!(sig.arguments.is_empty());
        assert!(sig.result.is_void());
    }

    #[test]
    fn test_method_signature_generic() {
        let sig: MethodSignature = "<T:Ljava/lang/Object;>(TT;)TT;".parse().unwrap();
        assert_eq!(sig.type_parameters.len(), 1);
        assert_eq!(&*sig.type_parameters[0].name, "T");
    }

    #[test]
    fn test_method_signature_throws() {
        let sig: MethodSignature = "()V^Ljava/io/IOException;".parse().unwrap();
        assert_eq!(sig.throws_signature.len(), 1);
        assert!(sig.throws_signature[0].is_class());
    }

    #[test]
    fn test_class_signature() {
        let sig: ClassSignature = "Ljava/lang/Object;".parse().unwrap();
        assert!(sig.type_parameters.is_empty());
        assert_eq!(sig.superclass.class_name(), "java.lang.Object");
    }

    #[test]
    fn test_class_signature_generic() {
        let sig: ClassSignature = "<T:Ljava/lang/Object;>Ljava/lang/Object;Ljava/lang/Comparable<TT;>;".parse().unwrap();
        assert_eq!(sig.type_parameters.len(), 1);
        assert_eq!(sig.superinterfaces.len(), 1);
    }

    #[test]
    fn test_type_parameter_multiple_bounds() {
        let (_, tp) = TypeParameter::parse("T:Ljava/lang/Object;:Ljava/io/Serializable;").unwrap();
        assert_eq!(&*tp.name, "T");
        assert!(tp.class_bound.is_some());
        assert_eq!(tp.interface_bounds.len(), 1);
    }

    #[test]
    fn test_display_roundtrip() {
        let input = "Ljava/util/Map<Ljava/lang/String;Ljava/lang/Integer;>;";
        let (_, sig) = ClassTypeSignature::parse(input).unwrap();
        assert_eq!(sig.to_string(), input);
    }
}
