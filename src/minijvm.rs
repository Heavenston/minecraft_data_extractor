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

#[derive(Debug, Clone, PartialEq, Eq, bincode::Encode, bincode::Decode)]
pub enum ValueKind {
    Byte,
    Short,
    Int,
    Long,
    Float,
    Double,
    Char,
    Boolean,
    Ref,
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
    Interface { count: u8 },
    Special,
}

// TODO: Vs ConstantValue?
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

// TODO: Vs Constant?
#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub enum ConstantValue {
    Byte(i8),
    Short(i16),
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    String(String),
    Null,
}

#[derive(Debug, Clone, PartialEq, Eq, bincode::Encode, bincode::Decode)]
pub enum MethodKind {
    GetField,
    GetStatic,
    PutField,
    PutStatic,
    InvokeVirtual,
    InvokeStatic,
    InvokeSpecial,
    NewInvokeSpecial,
    InvokeInterface,
}

impl From<noak::reader::cpool::MethodKind> for MethodKind {
    fn from(kind: noak::reader::cpool::MethodKind) -> Self {
        use noak::reader::cpool;

        match kind {
            cpool::MethodKind::GetField => MethodKind::GetField,
            cpool::MethodKind::GetStatic => MethodKind::GetStatic,
            cpool::MethodKind::PutField => MethodKind::PutField,
            cpool::MethodKind::PutStatic => MethodKind::PutStatic,
            cpool::MethodKind::InvokeVirtual => MethodKind::InvokeVirtual,
            cpool::MethodKind::InvokeStatic => MethodKind::InvokeStatic,
            cpool::MethodKind::InvokeSpecial => MethodKind::InvokeSpecial,
            cpool::MethodKind::NewInvokeSpecial => MethodKind::NewInvokeSpecial,
            cpool::MethodKind::InvokeInterface => MethodKind::InvokeInterface,
        }
    }
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct DynamicCallSite {
    pub bootstrap: MethodRef,
    pub method_kind: MethodKind,
    pub static_args: Vec<Constant>,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    GreaterThan,
    LessThan,
    /// lcmp etc...
    Cmp,

    BitAnd,
    BitOr,
    BitXOr,
    BitShl,
    BitShr,
    BitUShl,
    BitUShr,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub enum UnOp {
    Neg,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub enum IfOperand {
    Int,
    /// Only support Eq and Ne operations
    Ref,
    Zero,
    /// Only support Eq and Ne operations
    Null,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub enum IfCmp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct GotoCondition {
    pub operand: IfOperand,
    pub cmp: IfCmp,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub enum Instruction {
    Noop,
    Dup {
        count: u8,
        depth: u8,
    },
    Pop {
        count: u8,
    },
    Swap,

    Constant { value: ConstantValue },
    Convert { from: ValueKind, to: ValueKind },

    Load { kind: ValueKind, index: u16 },
    Store { kind: ValueKind, index: u16 },
    IncInt { index: u16, value: i16 },

    Goto {
        offset: i32,
        cond: Option<GotoCondition>,
    },
    Jsr {
        offset: i32,
    },
    Ret {
        index: u16,
    },

    Ldc {
        constant: Constant,
    },

    BinOp {
        op: BinOp,
        value_kind: ValueKind,
    },
    UnOp {
        op: UnOp,
        value_kind: ValueKind,
    },

    Invoke {
        kind: InvokeKind,
        method: MethodRef,
    },
    InvokeDynamic {
        call_site: DynamicCallSite,
        name: Ident,
        descriptor: MethodDescriptor,
    },

    GetField {
        is_static: bool,
        field: FieldRef,
    },
    PutField {
        is_static: bool,
        field: FieldRef,
    },

    Return {
        /// If none this instruction returns void
        kind: Option<ValueKind>,
    },

    Throw,

    New {
        class: ClassRef,
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
    pub super_class: Option<IdentPath>,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
}
