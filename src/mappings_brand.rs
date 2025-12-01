
#[derive(Debug, Clone, Copy, PartialEq, Eq, bincode::Encode, bincode::Decode, serde::Serialize, serde::Deserialize)]
pub enum MappingsBrand {
    /// The official mojang-provided mappings
    Mojmaps,
}

