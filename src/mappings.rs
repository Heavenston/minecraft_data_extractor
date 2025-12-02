use std::ops::RangeInclusive;

#[derive(Debug, Clone, Copy, PartialEq, Eq, bincode::Encode, bincode::Decode, serde::Serialize, serde::Deserialize)]
pub enum Brand {
    /// The official mojang-provided mappings
    Mojmaps,
}

#[derive(Debug, Clone, derive_more::Display, bincode::Encode, bincode::Decode)]
pub struct Ident(pub String);

/// Classes names like net.minecraft.network.protocol.Packet
#[derive(Debug, Clone, derive_more::Display, bincode::Encode, bincode::Decode)]
pub struct IdentPath(pub String);

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct Type {
    pub ident: IdentPath,
    pub array_depth: usize,
}

impl Type {
    /// Returns this type formatted like it would appear in java
    pub fn formatted(&self) -> String {
        // FIXME: Pretty expansive for how hot this path is
        format!("{self}")
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)?;
        for _ in 0..self.array_depth {
            write!(f, "[]")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct Field {
    pub line_range: Option<RangeInclusive<usize>>,

    pub ty: Type,
    pub name: IdentPath,

    pub obfuscated_name: IdentPath,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct Method {
    pub line_range: Option<RangeInclusive<usize>>,

    pub return_type: Type,
    pub name: IdentPath,
    pub arguments: Vec<Type>,

    pub obfuscated_name: IdentPath,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode, derive_more::From, derive_more::TryInto)]
#[try_into(ref)]
pub enum Item {
    Field(Field),
    Method(Method),
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct Class {
    pub name: IdentPath,
    pub obfuscated_name: IdentPath,
    pub item_mappings: Vec<Item>,
}

impl Class {
    pub fn map_field(&self, obfuscated_name: &str, ty: &str) -> Option<&Field> {
        for item in &self.item_mappings {
            let Item::Field(field) = item
            else { continue };

            if &field.obfuscated_name.0 != obfuscated_name {
                continue;
            }

            if &field.ty.formatted() != ty {
                continue;
            }

            return Some(field);
        }

        None
    }

    pub fn map_method<I, T>(&self, obfuscated_name: &str, return_type: &str, args_types: I) -> Option<&Method>
        where I: Clone + IntoIterator<Item = T>,
              T: AsRef<str>,
    {
        for item in &self.item_mappings {
            let Item::Method(method) = item
            else { continue };

            if &method.obfuscated_name.0 != obfuscated_name {
                continue;
            }

            if &method.return_type.formatted() != return_type {
                continue;
            }

            let type_matches = method.arguments.iter()
                .zip(args_types.clone())
                .all(|(a, b)| &a.formatted() == b.as_ref());

            if !type_matches {
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
    pub fn map_class(&self, obfuscated_name: &str) -> Option<&IdentPath> {
        self.class_mappings.iter()
            .find(|class| class.obfuscated_name.0 == obfuscated_name)
            .map(|class| &class.name)
    }
}
