mod version_manifest;
use futures::{future, stream::FuturesUnordered, FutureExt as _, StreamExt, TryStreamExt};
use version_client_json::VersionClientJson;
use version_manifest::VersionManifestV2;
mod version_client_json;
mod extractors;

use std::path::{Path, PathBuf};
use tokio::{fs, io::AsyncWriteExt};

use clap::Parser;
use tracing::{debug, error, info, warn};

pub const CODE_HASH: &str = env!("CODE_HASH");

fn get_about(long: bool) -> String {
    let mut start = "Extract data from minecraft jars".to_string();

    if long {
        start = start + "\n" + format!("Code hash: {}",
            &CODE_HASH[..32]
        ).as_str();
    }

    return start;
}

async fn download_asset(client: &reqwest::Client, download_info: &version_client_json::FileDownloadInfo, path: &Path) -> anyhow::Result<()> {
    let sha1_file = path.with_extension("sha1");

    let stored_sha1 = match fs::read_to_string(&sha1_file).await {
        Ok(sha1) => Some(sha1),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => None,
        Err(e) => return Err(e.into())
    };

    if stored_sha1.as_ref() == Some(&download_info.sha1) {
        return Ok(());
    }

    let mut response = client.get(&download_info.url).send().await?.error_for_status()?;
    let mut file = fs::File::create(path).await?;
    while let Some(bytes) = response.chunk().await? {
        file.write_all(&bytes).await?;
    }

    fs::write(&sha1_file, download_info.sha1.as_bytes()).await?;
    
    Ok(())
}

#[derive(Parser, Debug)]
#[command(about = get_about(false), long_about = get_about(true))]
/// Extract data from minecraft jars
pub(crate) struct Args {
    /// Directory where all data should be downloaded
    #[arg(long, short, default_value = "mc_data")]
    output: PathBuf,
    /// If specified, only download and extract the data for this version
    #[arg(short, long)]
    minecraft_version: Option<String>,
    /// How much versions can be processed in parallel
    #[arg(short, long, default_value_t = 10)]
    parallelism: usize,
}

pub(crate) struct AppState {
    pub(crate) args: Args,
    pub(crate) client: reqwest::Client,
}

impl AppState {
    pub fn version_folder(&self, version_id: &str) -> PathBuf {
        self.args.output.join(&version_id)
    }
}

async fn get_updated_manifest_file(state: &AppState) -> anyhow::Result<VersionManifestV2> {
    let manifest_etag_file_path = state.args.output.join("version_manifestv2.etag");
    let manifest_json_file_path = state.args.output.join("version_manifestv2.json");

    let manifest_etag = match fs::read_to_string(&manifest_etag_file_path).await {
        Ok(etag) => {
            debug!(etag, "Found version manifest etag");
            Some(etag)
        },
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            debug!("No version manifest etag file found");
            None
        },
        Err(e) => {
            error!(?e, "Read etag error");
            return Err(e.into());
        }
    };

    let mut manifest_request = state.client.get(VersionManifestV2::MANIFEST_URL);
    if let Some(etag) = manifest_etag {
        manifest_request = manifest_request.header("If-None-Match", etag);
    }
    let manifest_response = manifest_request
        .send().await?;

    let manifest_content = if manifest_response.status() == reqwest::StatusCode::NOT_MODIFIED {
        info!("Manifest did not change since last request!");

        fs::read_to_string(&manifest_json_file_path).await?
    }
    else {
        let manifest_response = manifest_response.error_for_status()?;

        info!("Manifest changed!");

        match manifest_response.headers().get(reqwest::header::ETAG) {
            Some(new_etag) => {
                let new_etag = new_etag.to_str()?;
                debug!(?new_etag, "Writing new etag");
                fs::write(&manifest_etag_file_path, new_etag.as_bytes()).await?;
            },
            None => {
                warn!(?manifest_response, "Didn't receive any etag");
            },
        }

        let manifest_content = manifest_response.text().await?;
        fs::write(&manifest_json_file_path, manifest_content.as_bytes()).await?;

        manifest_content
    };

    Ok(serde_json::from_str(&manifest_content)?)
}

async fn get_updated_version_client_json(state: &AppState, version: &version_manifest::Version) -> anyhow::Result<VersionClientJson> {
    let folder = state.version_folder(&version.id);

    let json_file = folder.join(format!("{}.json", version.id));
    let sha1_file = folder.join(format!("{}.sha1", version.id));

    let saved_sha1 = match fs::read_to_string(&sha1_file).await {
        Ok(etag) => Some(etag),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => None,
        Err(e) => return Err(e.into())
    };

    if saved_sha1.as_ref() == Some(&version.sha1) {
        let json_content = fs::read_to_string(&json_file).await?;
        let client_json = serde_json::from_str(&json_content)?;

        Ok(client_json)
    }
    else {
        let json_content = state.client.get(&version.url).send().await?.error_for_status()?.text().await?;

        fs::write(&json_file, &json_content).await?;
        fs::write(&sha1_file, &version.sha1).await?;

        let client_json = serde_json::from_str(&json_content)?;

        Ok(client_json)
    }
}

async fn load_version(state: &AppState, version: &version_manifest::Version) -> anyhow::Result<()> {
    let version_folder = state.version_folder(&version.id);
    fs::create_dir_all(&version_folder).await?;

    let client_json = get_updated_version_client_json(&state, version).await?;

    let mut manager = extractors::ExtractionManager::new(state, &client_json).await?;

    let rslt = manager.extract::<extractors::mapped_server_jar::MappedServerJarExtractor>().await?;
    info!(?rslt, ?version, "Extracted");

    manager.finish().await?;
    
    Ok(())
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();
    let args = Args::parse();
    let state = AppState {
        args,
        client: reqwest::Client::new(),
    };

    fs::create_dir_all(&state.args.output).await?;
    let mut manifest = get_updated_manifest_file(&state).await?;
    manifest.versions.sort_by_key(|v| std::cmp::Reverse(v.release_time));

    info!(parallelism = state.args.parallelism, version_count = manifest.versions.len(), "Loading versions");

    let mut version_iter = manifest.versions.iter().enumerate();

    let mut errors = Vec::new();
    loop {
        let futures_acc = FuturesUnordered::new();
        for (i, version) in version_iter.by_ref().take(state.args.parallelism) {
            futures_acc.push(
                load_version(&state, version)
                .map(move |result| (i, version, result))
            );
        }
        futures_acc.for_each(|(i, version, result)| {
            match result {
                Ok(_) => {
                    info!("{:03}/{:03} Version '{}' success", i + 1, manifest.versions.len(), version.id);
                },
                Err(e) => {
                    error!("{:03}/{:03} Version '{}' error: {e}", i + 1, manifest.versions.len(), version.id);
                    errors.push(e);
                },
            }
        
            future::ready(())
        }).await;
    }

    info!("Finished");
    
    if errors.is_empty() {
        Ok(())
    }
    else {
        Err(anyhow::anyhow!("{}", errors.into_iter().map(|e| format!("{e}")).reduce(|a, b| a + ", " + &b).unwrap()))
    }
}
