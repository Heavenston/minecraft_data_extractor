
#[derive(Debug, Clone)]
enum TypeDescriptorKind {
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
    Object(String),
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
            ).map(|o: &str| TypeDescriptorKind::Object(o.replace("/", "."))),
        )).parse(content)
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

#[derive(Debug, Clone)]
struct TypeDescriptor {
    ty: TypeDescriptorKind,
    array_depth: usize,
}

impl TypeDescriptor {
    pub fn parse(content: &str) -> nom::IResult<&str, Self> {
        use nom::{ character::char, multi::many0_count, Parser as _ };

        (many0_count(char('[')), TypeDescriptorKind::parse)
            .map(|(array_depth, ty)| Self { array_depth, ty }).parse(content)
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

#[derive(Debug, Clone)]
struct MethodDescriptor {
    return_type: TypeDescriptor,
    args: Vec<TypeDescriptor>,
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
