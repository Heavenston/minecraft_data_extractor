mod using_protocols;
use std::collections::HashMap;

use using_protocols::{ extract_using_protocols };

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, bincode::Decode, bincode::Encode)]
pub enum PacketDirection {
    Serverbound,
    Clientbound,
}

#[derive(Debug, Clone, bincode::Decode, bincode::Encode)]
pub struct RecordType {
    pub name: String,
    pub fields: HashMap<String, DataType>,
}

#[derive(Debug, Clone, bincode::Decode, bincode::Encode)]
pub struct EnumType {
    pub name: String,
    // TODO: Variants can be more than just a name as a string
    pub variants: Vec<String>,
}

#[derive(Debug, Clone, bincode::Decode, bincode::Encode)]
pub enum DataType {
    Byte,
    Int,
    Boolean,
    /// java.lang.String
    String,
    /// java.util.UUID
    UUID,
    Record(RecordType),
    Enum(EnumType),
    Array(Box<DataType>),
    Other,
}

#[derive(Debug, Clone, bincode::Decode, bincode::Encode)]
pub struct Packet {
    pub display_name: String,
    pub record: RecordType,
}

#[derive(Debug, Clone, bincode::Decode, bincode::Encode)]
pub struct ProtocolState {
    pub name: String,
    pub serverbound_packets: Vec<Packet>,
    pub clientbound_packets: Vec<Packet>,
}

#[derive(Debug, Clone, bincode::Decode, bincode::Encode)]
pub struct Packets {
    pub states: Vec<ProtocolState>,
}

#[derive(Default, Debug, Clone, bincode::Decode, bincode::Encode)]
pub struct PacketsExtractor;

impl super::ExtractorKind for PacketsExtractor {
    type Output = Packets;

    fn name(&self) -> &'static str {
        "packets_extractor"
    }

    async fn extract(self, manager: &mut super::ExtractionManager<'_>) -> anyhow::Result<Packets> {
        extract_using_protocols(manager).await
    }
}
