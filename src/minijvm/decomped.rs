use super::{ Ident, IdentPath, TypeDescriptor, MethodDescriptor, AccessFlags };

pub enum Expression {
    
}

pub enum Statement {
    
}

pub struct Field {
    pub name: Ident,
    pub descriptor: TypeDescriptor,
    pub access_flags: AccessFlags,

    pub init_value: Option<Expression>,
}

pub struct Method {
    pub name: Ident,
    pub descriptor: MethodDescriptor,
    pub access_flags: AccessFlags,

    pub code: Statement,
}

pub struct Class {
    pub name: IdentPath,
    pub super_class: Option<IdentPath>,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
}
