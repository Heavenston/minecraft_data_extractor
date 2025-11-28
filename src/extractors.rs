use std::{cell::OnceCell, sync::{Arc, LazyLock}};

use crate::version_manifest;

pub enum ExtractorOutput {
    
}

pub trait Extractor {
    fn name(&self) -> &'static str;
}

pub type ExtractorObject = Arc<dyn Extractor + Send + Sync>;

pub static EXTRACTORS: LazyLock<Vec<ExtractorObject>> = LazyLock::new(|| {
    vec![]
});

pub fn find_extractor(version: &version_manifest::Version) -> Option<ExtractorObject> {
    EXTRACTORS.iter()
        .find(|extractor| false)
        .cloned()
}
