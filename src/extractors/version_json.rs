use std::{io::Read, sync::LazyLock};

use anyhow::bail;
use rc_zip_sync::ReadZip as _;

use crate::version_json::VersionJson;

// The first version to bundle a version.json was '18w47b' which release on '2018-11-23T10:46:41+00:00'
static VERSION_JSON_FIRST_RELEASE_TIME: LazyLock<chrono::DateTime<chrono::Utc>> = LazyLock::new(|| {
    chrono::DateTime::parse_from_rfc3339("2018-11-23T10:46:41+00:00").unwrap()
        .to_utc()
});

#[derive(Debug, Clone, Copy, Default, bincode::Encode)]
pub struct VersionJsonExtractor;
impl super::ExtractorKind for VersionJsonExtractor {
    type Output = VersionJson;

    fn output_encoder_decoder(&self) -> Option<impl super::EncoderDecoder<Self::Output> + 'static> {
        Some(super::BincodeEncoderDecoder)
    }

    fn name(&self) -> &'static str {
        "version_json_extractor"
    }

    async fn extract(self, manager: &mut super::ExtractionManager<'_>) -> anyhow::Result<Self::Output> {
        if manager.version().release_time < *VERSION_JSON_FIRST_RELEASE_TIME {
            return Err(super::VersionNotSupportedError.into());
        }

        let server_jar_path = manager.extract(super::server_jar::ServerJarExtractor).await?;

        let version_json = tokio::task::spawn_blocking(move || -> anyhow::Result<VersionJson> {
            let wrapper_jar_file = std::fs::File::open(&*server_jar_path)?;
            let zip_file = wrapper_jar_file.read_zip()?;

            let Some(version_file_entry) = zip_file.by_name("version.json")
            else { bail!("Could not find the version.json") };

            let mut content = String::new();
            version_file_entry.reader().read_to_string(&mut content)?;

            Ok(serde_json::from_str(&content)?)
        }).await??;

        Ok(version_json)
    }
}
