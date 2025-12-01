use std::{ path::PathBuf, sync::{ Arc, LazyLock } };

use anyhow::bail;

use crate::version_client_json::VersionClientJson;

// The first version to release official mapping was '1.14.4' which release on '2019-07-19T09:25:47+00:00'
static MOJMAPS_FIRST_VERSION_TIME: LazyLock<chrono::DateTime<chrono::Utc>> = LazyLock::new(|| {
    chrono::DateTime::parse_from_rfc3339("2019-07-19T09:25:47+00:00").unwrap()
        .to_utc()
});

static IMPLS: LazyLock<Vec<Arc<dyn MappedServerJarExtractorImpl>>> = LazyLock::new(|| {
    vec![
        Arc::new(MojmapsMapper)
    ]
});

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub enum MappingsBrand {
    Mojmaps,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct MappedServerJarResult {
    pub brand: MappingsBrand,
    pub path: PathBuf,
}

#[derive(Debug, Clone, Copy, Default, bincode::Encode)]
pub struct MappedServerJarExtractor;
impl super::ExtractorKind for MappedServerJarExtractor {
    type Output = MappedServerJarResult;

    fn name(&self) -> &'static str {
        "mapped_server_jar_extractor"
    }

    async fn extract(self, manager: &mut super::ExtractionManager<'_>) -> anyhow::Result<Self::Output> {
        for impl_ in &*IMPLS {
            if impl_.supports_version(manager.version()) {
                return impl_.extract(manager).await;
            }
        }
        Err(super::VersionNotSupportedError.into())
    }
}

#[async_trait::async_trait]
trait MappedServerJarExtractorImpl: Send + Sync {
    fn supports_version(&self, version: &VersionClientJson) -> bool;

    async fn extract(&self, manager: &mut super::ExtractionManager<'_>) -> anyhow::Result<<MappedServerJarExtractor as super::ExtractorKind>::Output>;
}

/// 1.18 changed how the jar file is bundled
struct MojmapsMapper;

#[async_trait::async_trait]
impl MappedServerJarExtractorImpl for MojmapsMapper {
    fn supports_version(&self, version: &VersionClientJson) -> bool {
        version.release_time >= *MOJMAPS_FIRST_VERSION_TIME
    }

    async fn extract(&self, manager: &mut super::ExtractionManager<'_>) -> anyhow::Result<<MappedServerJarExtractor as super::ExtractorKind>::Output> {
        // TODO: Actually does nothing

        let version_folder = manager.app_state().version_folder(&manager.version().id);
        let _server_jar_path = manager.extract(super::server_jar::ServerJarExtractor).await?;

        let mappings_path = manager.download_asset("server_mappings").await?;
        let mapped_jar_path = version_folder.join("server_mojmapped.jar");

        let _ = mappings_path;
        
        Ok(MappedServerJarResult {
            brand: MappingsBrand::Mojmaps,
            path: mapped_jar_path,
        })
    }
}

