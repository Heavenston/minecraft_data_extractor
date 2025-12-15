
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, bincode::Encode, bincode::Decode)]
#[expect(dead_code)]
pub struct VersionJson {
    pub id: String,
    pub name: String,
    pub protocol_version: i32,
    pub world_version: i32,
    pub series_id: String,
}
