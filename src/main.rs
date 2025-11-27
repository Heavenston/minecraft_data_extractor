mod version_manifest;
use version_manifest::VersionManifestV2;

use std::path::PathBuf;
use tokio::fs;

use clap::Parser;
use tracing::{debug, error, info, warn};

#[derive(Parser, Debug)]
struct Args {
    /// Directory where all data should be downloaded
    #[arg(long, short, default_value = "mc_data")]
    output: PathBuf,
    /// If specified, only download and extract the data for this version
    #[arg(long)]
    version: Option<String>,
}

async fn get_updated_manifest_file(args: &Args, client: &reqwest::Client) -> anyhow::Result<VersionManifestV2> {
    let manifest_etag_file_path = args.output.join("version_manifestv2.etag");
    let manifest_json_file_path = args.output.join("version_manifestv2.json");

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

    let mut manifest_request = client.get(VersionManifestV2::MANIFEST_URL);
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

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();
    let args = Args::parse();

    let client = reqwest::Client::new();

    fs::create_dir_all(&args.output).await?;
    let manifest = get_updated_manifest_file(&args, &client).await?;

    info!("Latest release: {}", manifest.latest.release);
    info!("Latest snapshot: {}", manifest.latest.snapshot);
    info!("Version count: {}", manifest.versions.len());

    anyhow::Result::Ok(())
}
