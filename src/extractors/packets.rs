mod using_protocols;
use std::collections::HashMap;

use using_protocols::{ extract_using_protocols };

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, bincode::Decode, bincode::Encode)]
pub struct RecordType {
    pub name: String,
    #[serde(serialize_with = "crate::ordered_map")]
    pub fields: HashMap<String, DataType>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, bincode::Decode, bincode::Encode)]
pub struct EnumType {
    pub name: String,
    // TODO: Variants can be more than just a name as a string
    pub variants: Vec<String>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, bincode::Decode, bincode::Encode)]
#[serde(tag = "type", content = "value", rename_all = "snake_case")]
pub enum DataType {
    Byte,
    Char,
    Double,
    Float,
    Int,
    Long,
    Short,
    Boolean,
    /// java.lang.String
    String,
    /// java.util.UUID
    #[serde(rename = "uuid")]
    UUID,
    Array(Box<DataType>),
    // java.util.List
    List(Box<DataType>),
    // java.util.Optional
    Optional(Box<DataType>),
    Record(RecordType),
    Enum(EnumType),
    Other {
        name: String,
        type_args: Vec<DataType>,
    },
    Error { name: String },
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, bincode::Decode, bincode::Encode)]
pub struct Packet {
    pub display_name: String,
    pub record: RecordType,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, bincode::Decode, bincode::Encode)]
pub struct ProtocolState {
    pub name: String,
    pub serverbound_packets: Vec<Packet>,
    pub clientbound_packets: Vec<Packet>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, bincode::Decode, bincode::Encode)]
pub struct Packets {
    pub states: Vec<ProtocolState>,
}

#[derive(Default, Debug, Clone, bincode::Decode, bincode::Encode)]
pub struct PacketsExtractor;

impl super::ExtractorKind for PacketsExtractor {
    type Output = Packets;

    fn output_encoder_decoder(&self) -> Option<impl super::EncoderDecoder<Self::Output> + 'static> {
        Some(super::BincodeEncoderDecoder)
    }

    fn name(&self) -> &'static str {
        "packets_extractor"
    }

    #[tracing::instrument(name = "packets_extractor", skip_all, fields(version_id = %manager.version().id))]
    async fn extract(self, manager: &mut super::ExtractionManager<'_>) -> anyhow::Result<Packets> {
        extract_using_protocols(manager).await
    }
}
