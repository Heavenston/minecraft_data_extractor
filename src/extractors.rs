pub mod packet_info;
pub mod server_jar;
pub mod mapped_server_jar;

use std::any::Any;

pub trait ExtractorOutput: Any + bincode::Decode<()> + bincode::Encode + Send + Sync
{ }

impl<T: Any + bincode::Decode<()> + bincode::Encode + Send + Sync> ExtractorOutput for T
{ }

pub trait ExtractorKind: Any {
    type Output: ExtractorOutput;

    /// Unique name for this extractor kind, used for serialization so should
    /// not change with time
    fn name() -> &'static str;
    async fn extract(manager: &mut ExtractionManager) -> anyhow::Result<Self::Output>;
}

mod manager {
    use std::{ any::{ Any, TypeId }, collections::HashMap, sync::Arc };
    use anyhow::bail;
    use tokio::fs;
    use tracing::{ debug, warn };

    use crate::{ version_client_json::VersionClientJson, AppState };
    use super::*;

    #[derive(bincode::Decode, bincode::Encode)]
    struct VersionExtractionCache {
        /// We fully invalidate the cache when the code_hash changes
        code_hash: String,
        extractions: HashMap<String, Box<[u8]>>,
    }

    pub struct ExtractionManager<'a> {
        cache: Option<VersionExtractionCache>,
        app_state: &'a AppState,
        version: &'a VersionClientJson,
        extractions: HashMap<&'static str, Option<(Box<[u8]>, Arc<dyn Any + Send + Sync>)>>,
    }

    impl<'a> ExtractionManager<'a> {
        fn bincode_config() -> bincode::config::Configuration {
            bincode::config::standard()
        }

        pub async fn new(app_state: &'a AppState, version: &'a VersionClientJson) -> anyhow::Result<Self> {
            let version_folder = app_state.version_folder(&version.id);
            let cache = match fs::read(version_folder.join("extraction_cache.bincode")).await {
                Ok(bytes) => match bincode::decode_from_slice::<VersionExtractionCache, _>(&bytes, Self::bincode_config()) {
                    Ok((cache, _)) => {
                        if cache.code_hash == crate::CODE_HASH {
                            Some(cache)
                        }
                        else {
                            debug!(version_id = version.id, "Disarded version extraction cache");
                            None
                        }
                    },
                    Err(error) => {
                        warn!(%error, version_id = version.id, "Error on decoding extraction cache");
                        None
                    },
                },
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => None,
                Err(e) => return Err(e.into()),
            };

            Ok(Self {
                cache,
                app_state,
                version,
                extractions: Default::default(),
            })
        }

        pub async fn finish(self) -> anyhow::Result<()> {
            let version_folder = self.app_state.version_folder(&self.version.id);
            let cache_path = version_folder.join("extraction_cache.bincode");

            let cache = VersionExtractionCache {
                code_hash: crate::CODE_HASH.into(),
                extractions: self.extractions.into_iter()
                    .map(|(k, v)| (k.to_string(), v.unwrap().0))
                    .collect(),
            };

            let data = bincode::encode_to_vec(cache, Self::bincode_config())?;
            tokio::fs::write(cache_path, &data).await?;

            Ok(())
        }

        pub fn app_state(&self) -> &'a AppState {
            self.app_state
        }

        pub fn version(&self) -> &'a VersionClientJson {
            self.version
        }

        pub async fn extract<K: ExtractorKind>(&mut self) -> anyhow::Result<Arc<K::Output>> {
            if let Some(extracted_value) = self.extractions.get(&K::name()) {
                match extracted_value {
                    Some((_, v)) => return Ok(Arc::downcast(Arc::clone(v)).expect("Should be the correct type")),
                    None => bail!("Detected recursive call of ExtractionManager::extract<{}>", std::any::type_name::<K>()),
                }
            }

            // Insert None to detect if while running K::extract this very method
            // is callled recursively, making an infinite loop
            self.extractions.insert(K::name(), None);

            let cached_extraction = self.cache.as_ref()
                .and_then(|cache| cache.extractions.get(K::name()))
                .and_then(|extraction_data| match bincode::decode_from_slice::<K::Output, _>(&extraction_data, Self::bincode_config()) {
                    Ok((output, _)) => Some(Arc::new(output)),
                    Err(error) => {
                        warn!(%error, version_id = self.version.id, extractor = K::name(), extractor_type_name = std::any::type_name::<K>(), "Error decoding extractor cached output");
                        None
                    },
                });
            let output = if let Some(cached_extraction) = cached_extraction {
                cached_extraction
            } else {
                Arc::new(K::extract(self).await?)
            };
            let encoded = bincode::encode_to_vec(&*output, Self::bincode_config())?;
            self.extractions.insert(K::name(), Some((encoded.into_boxed_slice(), Arc::clone(&output) as Arc<dyn Any + Send + Sync>)));

            Ok(output)
        }
    }
}
pub use manager::*;
