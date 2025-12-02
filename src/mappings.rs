use std::ops::RangeInclusive;

#[derive(Debug, Clone, Copy, PartialEq, Eq, bincode::Encode, bincode::Decode, serde::Serialize, serde::Deserialize)]
pub enum MappingsBrand {
    /// The official mojang-provided mappings
    Mojmaps,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct MappingsIdent(pub String);

impl std::fmt::Display for MappingsIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Classes names like net.minecraft.network.protocol.Packet
#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct MappingsIdentPath(pub Vec<MappingsIdent>);

impl MappingsIdentPath {
    pub fn eq_str(&self, other: &str) -> bool {
        other.split('.')
            .zip(self.0.iter())
            .all(|(a, MappingsIdent(b))| a == b)
    }
}

impl std::fmt::Display for MappingsIdentPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter();
        if let Some(first) = iter.next() {
            write!(f, "{first}")?;
        }
        for other in iter {
            write!(f, ".{other}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct MappingsType {
    pub ident: MappingsIdentPath,
    pub array_depth: usize,
}

impl std::fmt::Display for MappingsType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)?;
        for _ in 0..self.array_depth {
            write!(f, "[]")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct MappingsFieldMapping {
    pub line_range: Option<RangeInclusive<usize>>,

    pub ty: MappingsType,
    pub name: MappingsIdentPath,

    pub obfuscated_name: MappingsIdentPath,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct MappingsMethodMapping {
    pub line_range: Option<RangeInclusive<usize>>,

    pub return_type: MappingsType,
    pub name: MappingsIdentPath,
    pub arguments: Vec<MappingsType>,

    pub obfuscated_name: MappingsIdentPath,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode, derive_more::From, derive_more::TryInto)]
#[try_into(ref)]
pub enum MappingsItemMapping {
    Field(MappingsFieldMapping),
    Method(MappingsMethodMapping),
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct MappingsClassMapping {
    pub name: MappingsIdentPath,
    pub obfuscated_name: MappingsIdentPath,
    pub item_mappings: Vec<MappingsItemMapping>,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct Mappings {
    pub brand: MappingsBrand,
    pub class_mappings: Vec<MappingsClassMapping>,
}
