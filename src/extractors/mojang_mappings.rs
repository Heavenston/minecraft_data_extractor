
#[derive(Debug, bincode::Encode)]
pub struct MojangMappingsExtractor {
    
}

impl super::ExtractorKind for MojangMappingsExtractor {
    type Output = ();

    fn config(&self) -> super::ExtractorConfig {
        super::ExtractorConfig { store_output_in_cache: false }
    }
    
    fn name(&self) -> &'static str {
        "mojang_mappings_extractor"
    }

    async fn extract(self, _manager: &mut super::ExtractionManager<'_>) -> anyhow::Result<Self::Output> {
        todo!()
    }
}

