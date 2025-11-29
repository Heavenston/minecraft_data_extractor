
#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct LatestVersion {
    pub release: String,
    pub snapshot: String,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum VersionType {
    Release,
    Snapshot,
    OldBeta,
    OldAlpha,
    #[serde(untagged)]
    Other(String),
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Version {
    pub id: String,
    #[serde(rename = "type")]
    pub ty: VersionType,
    pub url: String,
    pub time: String,
    pub release_time: chrono::DateTime<chrono::Utc>,
    pub sha1: String,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct VersionManifestV2 {
    pub latest: LatestVersion,
    pub versions: Vec<Version>,
}

impl VersionManifestV2 {
    pub const MANIFEST_URL: &'static str = "https://piston-meta.mojang.com/mc/game/version_manifest_v2.json";
}
