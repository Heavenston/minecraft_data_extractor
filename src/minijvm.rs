//! Types re-defining java classes, used for easier manipulation than straight up
//! using noak's types

mod descriptors;
pub use descriptors::*;

#[derive(Debug, Clone, PartialEq, Eq, derive_more::Display, bincode::Encode, bincode::Decode)]
pub struct Ident(pub String);

/// Classes names like net.minecraft.network.protocol.Packet
#[derive(Debug, Clone, PartialEq, Eq, derive_more::Display, bincode::Encode, bincode::Decode)]
pub struct IdentPath(pub String);

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct AccessFlags {
    pub public: bool,
    pub private: bool,
    pub protected: bool,
    pub static_: bool,
    pub final_: bool,
    pub super_: bool,
    pub synchronized: bool,
    pub bridge: bool,
    pub volatile: bool,
    pub varargs: bool,
    pub transient: bool,
    pub native: bool,
    pub interface: bool,
    pub abstract_: bool,
    pub strict: bool,
    pub synthetic: bool,
    pub annotation: bool,
    pub enum_: bool,
    pub mandated: bool,
    pub module: bool,
}

impl From<noak::AccessFlags> for AccessFlags {
    fn from(value: noak::AccessFlags) -> Self {
        Self {
            public: value.contains(noak::AccessFlags::PUBLIC),
            private: value.contains(noak::AccessFlags::PRIVATE),
            protected: value.contains(noak::AccessFlags::PROTECTED),
            static_: value.contains(noak::AccessFlags::STATIC),
            final_: value.contains(noak::AccessFlags::FINAL),
            super_: value.contains(noak::AccessFlags::SUPER),
            synchronized: value.contains(noak::AccessFlags::SYNCHRONIZED),
            bridge: value.contains(noak::AccessFlags::BRIDGE),
            volatile: value.contains(noak::AccessFlags::VOLATILE),
            varargs: value.contains(noak::AccessFlags::VARARGS),
            transient: value.contains(noak::AccessFlags::TRANSIENT),
            native: value.contains(noak::AccessFlags::NATIVE),
            interface: value.contains(noak::AccessFlags::INTERFACE),
            abstract_: value.contains(noak::AccessFlags::ABSTRACT),
            strict: value.contains(noak::AccessFlags::STRICT),
            synthetic: value.contains(noak::AccessFlags::SYNTHETIC),
            annotation: value.contains(noak::AccessFlags::ANNOTATION),
            enum_: value.contains(noak::AccessFlags::ENUM),
            mandated: value.contains(noak::AccessFlags::MANDATED),
            module: value.contains(noak::AccessFlags::MODULE),
        }
    }
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct Field {
    pub access_flags: AccessFlags,
    pub name: Ident,
    pub descriptor: TypeDescriptor,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct ClassRef {
    pub name: IdentPath,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct FieldRef {
    pub class: ClassRef,
    pub name: Ident,
    pub descriptor: TypeDescriptor,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct MethodRef {
    pub class: ClassRef,
    pub name: Ident,
    pub descriptor: MethodDescriptor,
}

#[derive(Debug, Clone, PartialEq, Eq, bincode::Encode, bincode::Decode)]
pub enum InvokeKind {
    Static,
    Virtual,
    Interface,
    Special,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub enum Constant {
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    String(String),
    Class(ClassRef),
    MethodHandle(MethodRef),
    MethodType(MethodDescriptor),
    Null,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct DynamicCallSite {
    pub bootstrap: MethodRef,
    pub static_args: Vec<Constant>,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub enum Instruction {
    Invoke {
        kind: InvokeKind,
        method: MethodRef,
    },
    InvokeDynamic {
        call_site: DynamicCallSite,
        name: Ident,
        descriptor: MethodDescriptor,
    },

    /// Contains the debug format of the instruction
    Unknown(String),
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct Code {
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct Method {
    pub access_flags: AccessFlags,
    pub name: Ident,
    pub descriptor: MethodDescriptor,
    pub code: Code,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct Class {
    pub name: IdentPath,
    pub super_class: IdentPath,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
}
