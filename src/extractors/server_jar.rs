use std::{ path::PathBuf, sync::{ Arc, LazyLock } };

use anyhow::bail;
use rc_zip_sync::ReadZip;

use crate::version_client_json::VersionClientJson;

// The first version to change how minecraft jars are published (using a wrapper jar) is '21w39a' which released on '2021-09-29T16:27:05+00:00'
static WRAPPER_FIRST_VERSION_TIME: LazyLock<chrono::DateTime<chrono::Utc>> = LazyLock::new(|| {
    chrono::DateTime::parse_from_rfc3339("2021-09-29T16:27:05+00:00").unwrap()
        .to_utc()
});

static IMPLS: LazyLock<Vec<Arc<dyn ServerJarExtractorImpl>>> = LazyLock::new(|| {
    vec![
        Arc::new(Post1Dot18ServerJarExtractor)
    ]
});

pub struct ServerJarExtractor;
impl super::ExtractorKind for ServerJarExtractor {
    type Output = PathBuf;

    fn name() -> &'static str {
        "server_jar_extractor"
    }

    async fn extract(manager: &mut super::ExtractionManager<'_>) -> anyhow::Result<Self::Output> {
        for impl_ in &*IMPLS {
            if impl_.supports_version(manager.version()) {
                return impl_.extract(manager).await;
            }
        }
        Err(super::VersionNotSupportedError.into())
    }
}

#[async_trait::async_trait]
trait ServerJarExtractorImpl: Send + Sync {
    fn supports_version(&self, version: &VersionClientJson) -> bool;

    async fn extract(&self, manager: &mut super::ExtractionManager<'_>) -> anyhow::Result<<ServerJarExtractor as super::ExtractorKind>::Output>;
}

/// 1.18 changed how the jar file is bundled
struct Post1Dot18ServerJarExtractor;

#[async_trait::async_trait]
impl ServerJarExtractorImpl for Post1Dot18ServerJarExtractor {
    fn supports_version(&self, version: &VersionClientJson) -> bool {
        version.release_time >= *WRAPPER_FIRST_VERSION_TIME
    }

    async fn extract(&self, manager: &mut super::ExtractionManager<'_>) -> anyhow::Result<<ServerJarExtractor as super::ExtractorKind>::Output> {
        let version_folder = manager.app_state().version_folder(&manager.version().id);
        let server_wrapper_jar_path = version_folder.join("server_wrapper.jar");
        let server_jar_path = version_folder.join("server.jar");

        // All versions we deal with here (>=1.18) have a server download
        let Some(mappings_info) = manager.version().downloads.get("server")
        else { bail!("Could not find server download for version {}", manager.version().id) };

        crate::download_asset(&manager.app_state().client, &mappings_info, &server_wrapper_jar_path).await?;

        let version_id = manager.version().id.clone();

        let server_jar_path_ = server_jar_path.clone();
        tokio::task::spawn_blocking(move || -> anyhow::Result<()> {
            let wrapper_jar_file = std::fs::File::open(&server_wrapper_jar_path)?;
            let zip_file = wrapper_jar_file.read_zip()?;

            let Some(server_entry) = zip_file.by_name(format!("META-INF/versions/{0}/server-{0}.jar", version_id))
            else { bail!("Could not find actual server jar from wrapper for version {version_id}") };

            let mut jar_file = std::fs::File::create(&server_jar_path_)?;

            std::io::copy(&mut server_entry.reader(), &mut jar_file)?;

            Ok(())
        }).await??;

        Ok(server_jar_path)
    }
}
