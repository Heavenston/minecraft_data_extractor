use super::{ Ident, IdentPath, TypeDescriptor, MethodDescriptor, AccessFlags };

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub enum Constant {
    Byte(i8),
    Short(i16),
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    String(String),
    Class(super::ClassRef),
    MethodHandle(super::MethodRef),
    MethodType(MethodDescriptor),
    Null,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub enum Expression {
    Constant {
        value: Constant,
    },
    Load {
        value_kind: super::ValueKind,
        index: u16,
    },
    BinOp {
        op: super::BinOp,
        value_kind: super::ValueKind,

        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    UnOp {
        op: super::UnOp,
        value_kind: super::ValueKind,
        operand: Box<Expression>,
    },
    Invoke {
        kind: super::InvokeKind,
        method: super::MethodRef,
        object: Option<Box<Expression>>,
        args: Vec<Expression>,
    },
    InvokeDynamic {
        call_site: super::DynamicCallSite,
        name: Ident,
        descriptor: super::MethodDescriptor,
        args: Vec<Expression>,
    },

    New {
        class: super::ClassRef,
    },
    NewArray {
        kind: super::ValueKind,
        count: Box<Expression>,
    },

    Convert {
        from: super::ValueKind,
        to: super::ValueKind,
        value: Box<Expression>,
    },

    GetField {
        is_static: bool,
        field: super::FieldRef,
    },

    LoadFromArray {
        kind: super::ValueKind,
        array: Box<Expression>,
        index: Box<Expression>,
    },

    ArrayLength {
        array: Box<Expression>,
    },

    Cast {
        class: super::ClassRef,
        value: Box<Expression>,
    },

    /// Comparison used in `if` and loop conditions.
    ///
    /// This represents a boolean expression of the form
    /// `lhs <cmp> rhs`.
    Compare {
        cmp: super::IfCmp,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },

    Ternary {
        condition: Box<Expression>,
        then_value: Box<Expression>,
        else_value: Box<Expression>,
    },
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub enum Statement {
    Expression {
        expr: Expression,
    },
    Store {
        value_kind: super::ValueKind,
        index: u16,
        value: Expression,
    },
    PutField {
        is_static: bool,
        field: super::FieldRef,
        object: Option<Box<Expression>>,
        value: Expression,
    },
    Return {
        value: Option<(super::ValueKind, Expression)>,
    },
    Throw {
        value: Expression,
    },

    StoreIntoArray {
        kind: super::ValueKind,
        array: Expression,
        index: Expression,
        value: Expression,
    },

    /// Structured `if` statement with optional `else` branch.
    If {
        condition: Expression,
        then_branch: Vec<Statement>,
        else_branch: Vec<Statement>,
    },

    /// Structured `while` loop.
    ///
    /// Represents:
    /// `while (condition) { body }`
    While {
        condition: Expression,
        body: Vec<Statement>,
    },
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct Field {
    pub name: Ident,
    pub descriptor: TypeDescriptor,
    pub access_flags: AccessFlags,

    pub init_value: Option<Expression>,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct Method {
    pub name: Ident,
    pub descriptor: MethodDescriptor,
    pub access_flags: AccessFlags,
    pub code: Vec<Statement>,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct Class {
    pub name: IdentPath,
    pub super_class: Option<IdentPath>,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
}
