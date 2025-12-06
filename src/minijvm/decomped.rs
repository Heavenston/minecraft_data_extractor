use super::{ Ident, IdentPath, TypeDescriptor, MethodDescriptor, AccessFlags };

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub enum Expression {
    Constant {
        value: super::ConstantValue,
    },
    Load {
        value_kind: super::ValueKind,
        index: u16,
    },
    Store {
        value_kind: super::ValueKind,
        index: u16,
        value: Box<Expression>,
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

    New {
        class: super::ClassRef,
    },

    GetField {
        is_static: bool,
        field: super::FieldRef,
    },
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub enum Statement {
    Expression {
        expr: Expression,
    },
    PutField {
        is_static: bool,
        field: super::FieldRef,
        value: Expression,
    },
    Return {
        value: Option<(super::ValueKind, Expression)>,
    },
    Throw {
        value: Expression,
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
