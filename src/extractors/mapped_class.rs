
#[derive(Debug, bincode::Encode)]
pub struct MappedClassExtractor {
    
}

impl super::ExtractorKind for MappedClassExtractor {
    type Output = ();

    fn config(&self) -> super::ExtractorConfig {
        super::ExtractorConfig { store_output_in_cache: false }
    }
    
    fn name(&self) -> &'static str {
        "mapped_class_extractor"
    }

    async fn extract(self, _manager: &mut super::ExtractionManager<'_>) -> anyhow::Result<Self::Output> {
        todo!()
    }
}
