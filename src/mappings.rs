use crate::minijvm;

use std::ops::RangeInclusive;
use anyhow::anyhow;

#[derive(Debug, Clone, Copy, PartialEq, Eq, bincode::Encode, bincode::Decode, serde::Serialize, serde::Deserialize)]
pub enum Brand {
    /// The official mojang-provided mappings
    Mojmaps,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct Field {
    pub line_range: Option<RangeInclusive<usize>>,

    pub name: minijvm::Ident,
    pub descriptor: minijvm::TypeDescriptor,

    pub obfuscated_name: minijvm::Ident,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct Method {
    pub line_range: Option<RangeInclusive<usize>>,

    pub name: minijvm::Ident,
    pub descriptor: minijvm::MethodDescriptor,

    pub obfuscated_name: minijvm::Ident,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode, derive_more::From, derive_more::TryInto)]
#[try_into(ref)]
pub enum Item {
    Field(Field),
    Method(Method),
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct Class {
    pub name: minijvm::IdentPath,
    pub obfuscated_name: minijvm::IdentPath,
    pub item_mappings: Vec<Item>,
}

impl Class {
    pub fn map_field(&self, obfuscated_name: &str, type_descriptor: &minijvm::TypeDescriptor) -> Option<&Field> {
        for item in &self.item_mappings {
            let Item::Field(field) = item
            else { continue };

            if &field.obfuscated_name.0 != obfuscated_name {
                continue;
            }

            if &field.descriptor != type_descriptor {
                continue;
            }

            return Some(field);
        }

        None
    }

    pub fn map_method(&self, obfuscated_name: &str, descriptor: &minijvm::MethodDescriptor) -> Option<&Method> {
        for item in &self.item_mappings {
            let Item::Method(method) = item
            else { continue };

            if &method.obfuscated_name.0 != obfuscated_name {
                continue;
            }

            if &method.descriptor != descriptor {
                continue;
            }

            return Some(method);
        }

        None
    }
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct Mappings {
    pub brand: Brand,
    pub class_mappings: Vec<Class>,
}

impl Mappings {
    pub fn map_class(&self, obfuscated_name: &str) -> Option<&Class> {
        self.class_mappings.iter()
            .find(|class| class.obfuscated_name == obfuscated_name)
    }

    pub fn get_class(&self, name: &str) -> Option<&Class> {
        self.class_mappings.iter()
            .find(|class| class.name == name)
    }

    pub fn parse_and_map_type_descriptor(&self, desc: &str) -> anyhow::Result<minijvm::TypeDescriptor> {
        use nom::Parser as _;

        let (_, obf_d) = nom::combinator::complete(minijvm::TypeDescriptor::parse).parse(desc)
            .map_err(|e| anyhow!("Type descriptor parse error: {e}"))?;

        Ok(obf_d.to_mapped(self))
    }

    pub fn parse_and_map_method_descriptor(&self, desc: &str) -> anyhow::Result<minijvm::MethodDescriptor> {
        use nom::Parser as _;

        let (_, obf_d) = nom::combinator::complete(minijvm::MethodDescriptor::parse).parse(desc)
            .map_err(|e| anyhow!("Method descriptor parse error: {e}"))?;

        Ok(obf_d.to_mapped(self))
    }
}
