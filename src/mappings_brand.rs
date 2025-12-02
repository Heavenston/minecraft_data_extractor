
#[derive(Debug, Clone, Copy, PartialEq, Eq, bincode::Encode, bincode::Decode, serde::Serialize, serde::Deserialize)]
pub enum MappingsBrand {
    /// The official mojang-provided mappings
    Mojmaps,
}

pub trait ClassMappings {
    fn map_method(&self, obfuscated_method_name: &str, signature: &str) -> Option<&str>;
}
