pub mod packet_info;
pub mod server_jar;
pub mod version_json;
pub mod mapped_class;
pub mod mojang_mappings;
pub mod read_class;
pub mod decomp_class;
pub mod packets;

use std::any::Any;

pub trait ExtractorOutput: Any + Send + Sync
{ }

impl<T: Any + Send + Sync> ExtractorOutput for T
{ }

pub trait EncoderDecoder<T> {
    fn encode(&self, val: &T) -> anyhow::Result<Box<[u8]>>;
    fn decode(&self, data: &[u8]) -> anyhow::Result<T>;
}

pub struct BincodeEncoderDecoder;
impl<T: bincode::Decode<()> + bincode::Encode> EncoderDecoder<T> for BincodeEncoderDecoder {
    fn encode(&self, val: &T) -> anyhow::Result<Box<[u8]>> {
        Ok(bincode::encode_to_vec(val, bincode::config::standard())?.into_boxed_slice())
    }

    fn decode(&self, data: &[u8]) -> anyhow::Result<T> {
        Ok(bincode::decode_from_slice(data, bincode::config::standard())?.0)
    }
}

pub struct DummyEncoderDecoder;
impl<T> EncoderDecoder<T> for DummyEncoderDecoder {
    fn encode(&self, _val: &T) -> anyhow::Result<Box<[u8]>> {
        bail!("unimplemented")
    }

    fn decode(&self, _data: &[u8]) -> anyhow::Result<T> {
        bail!("unimplemented")
    }
}

pub trait ExtractorKind: Any + std::fmt::Debug + bincode::Encode {
    type Output: ExtractorOutput;

    // If None is returned, the output will not be cached
    fn output_encoder_decoder(&self) -> Option<impl EncoderDecoder<Self::Output> + 'static> {
        None::<DummyEncoderDecoder>
    }

    /// Unique name for this extractor kind, used for serialization so should
    /// not change with time
    fn name(&self) -> &'static str;
    async fn extract(self, manager: &mut ExtractionManager) -> anyhow::Result<Self::Output>;
}

#[derive(Debug, thiserror::Error)]
#[error("This version is not supported by this extractor")]
pub struct VersionNotSupportedError;

mod manager {
    use std::{ any::Any, collections::HashMap, path::PathBuf, sync::Arc };
    use anyhow::bail;
    use sha2::Digest;
    use tokio::fs;
    use tracing::{ trace, trace_span, warn, Instrument };

    use crate::{ version_client_json::VersionClientJson, AppState };
    use super::*;

    #[derive(bincode::Decode, bincode::Encode)]
    struct VersionExtractionCache {
        /// We fully invalidate the cache when the code_hash changes
        code_hash: String,
        extractions: HashMap<(String, [u8; 32]), Box<[u8]>>,
    }

    struct ExtractionEntry {
        /// None when the value should not be stored in the cache
        /// Contains the serialized value to be stored in the cache otherwise
        encoded: Option<Box<[u8]>>,
        value: Arc<dyn Any + Send + Sync>,
    }

    pub struct ExtractionManager<'a> {
        cache: Option<VersionExtractionCache>,
        app_state: &'a AppState,
        version: &'a VersionClientJson,
        #[cfg(debug_assertions)]
        duplicate_id_detection: HashMap<&'static str, std::any::TypeId>,
        extractions: HashMap<(&'static str, [u8; 32]), Option<ExtractionEntry>>,
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
                            trace!(version_id = version.id, "Disarded version extraction cache");
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
                #[cfg(debug_assertions)]
                duplicate_id_detection: Default::default(),
                extractions: Default::default(),
            })
        }

        pub async fn finish(self) -> anyhow::Result<()> {
            let version_folder = self.app_state.version_folder(&self.version.id);
            let cache_path = version_folder.join("extraction_cache.bincode");

            let cache = VersionExtractionCache {
                code_hash: crate::CODE_HASH.into(),
                extractions: self.extractions.into_iter()
                    .filter_map(|(key, entry_opt)| entry_opt.map(|entry| (key, entry)))
                    .filter_map(|(key, entry)| entry.encoded.map(|encoded| (key, encoded)))
                    .map(|((k1, k2), encoded)| ((k1.to_string(), k2), encoded))
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

        pub async fn download_asset(&mut self, name: &str) -> anyhow::Result<PathBuf> {
            let Some(asset_info) = self.version().downloads.get(name)
            else { bail!("Could not find asset {name}") };

            // Get the file extension from the url
            let extension = asset_info.url.split('.')
                .last()
                .map(|ext| format!(".{ext}"))
                .unwrap_or(String::new());

            let file_folder = self.app_state()
                .version_folder(&self.version().id)
                .join("assets");
            fs::create_dir_all(&file_folder).await?;
            let file_path = file_folder
                .join(format!("{name}{extension}"));

            crate::download_asset(&self.app_state.client, asset_info, &file_path).await?;

            Ok(file_path)
        }

        pub async fn extract<K: ExtractorKind>(&mut self, extractor: K) -> anyhow::Result<Arc<K::Output>> {
            let extractor_name = extractor.name();
            let extractor_hash = {
                let mut hasher = sha2::Sha256::new();
                bincode::encode_into_std_write(&extractor, &mut hasher, Self::bincode_config())?;
                hasher.finalize().as_slice().try_into().expect("Correct length")
            };

            #[cfg(debug_assertions)]
            match self.duplicate_id_detection.entry(&extractor_name) {
                std::collections::hash_map::Entry::Occupied(occupied_entry) => {
                    assert_eq!(*occupied_entry.get(), std::any::TypeId::of::<K>(), "Duplicate extractor id detected on '{extractor_name}'");
                },
                std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(std::any::TypeId::of::<K>());
                },
            }

            if let Some(extracted_value) = self.extractions.get(&(extractor_name, extractor_hash)) {
                match extracted_value {
                    Some(ExtractionEntry { encoded: _, value }) =>
                        return Ok(Arc::downcast(Arc::clone(value)).expect("Should be the correct type")),
                    None => {
                        println!("{}", std::backtrace::Backtrace::force_capture());
                        bail!("Detected recursive call of ExtractionManager::extract<{}>", std::any::type_name::<K>())
                    },
                }
            }

            // Insert None to detect if while running K::extract this very method
            // is callled recursively, making an infinite loop
            self.extractions.insert((extractor_name, extractor_hash), None);

            let output_encoder_decoder = extractor.output_encoder_decoder();
            let cached_extraction = self.cache.as_ref().zip(output_encoder_decoder.as_ref())
                .and_then(|(cache, ocd)| cache.extractions.get(&(extractor_name.to_string(), extractor_hash)).zip(Some(ocd)))
                .and_then(|(extraction_data, ocd)| match ocd.decode(&extraction_data) {
                    Ok(output) => Some(Arc::new(output)),
                    Err(error) => {
                        warn!(%error, version_id = self.version.id, extractor = extractor_name, extractor_type_name = std::any::type_name::<K>(), "Error decoding extractor cached output");
                        None
                    },
                });

            let output = if let Some(cached_extraction) = cached_extraction {
                cached_extraction
            } else {
                let span = trace_span!("Running extractor", extractor_name, ?extractor, version_id = self.version.id);
                let simple_span = trace_span!("Running extractor", extractor_name, version_id = self.version.id);
                span.clone().in_scope(|| trace!("Started extractor"));
                let output = Arc::new(extractor.extract(self).instrument(simple_span).await?);
                span.in_scope(|| trace!("Finished running extractor"));
                output
            };

            self.extractions.insert((extractor_name, extractor_hash), Some(ExtractionEntry {
                encoded: output_encoder_decoder.as_ref()
                    .map(|encoder_decoder| encoder_decoder.encode(&*output))
                    .transpose()?,
                value: Arc::clone(&output) as Arc<dyn Any + Send + Sync>,
            }));

            Ok(output)
        }
    }
}
use anyhow::bail;
pub use manager::*;
