use std::collections::HashMap;

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FileDownloadInfo {
    pub sha1: String,
    pub size: i64,
    pub url: String,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct JavaVersion {
    pub component: String,
    pub major_version: String,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ClientJson {
    pub id: String,
    pub java_version: JavaVersion,
    pub downloads: HashMap<String, FileDownloadInfo>,
    pub release_time: String,
    pub main_class: String,
}
