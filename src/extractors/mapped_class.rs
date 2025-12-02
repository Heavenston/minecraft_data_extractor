mod descriptors;
use descriptors::*;

use std::io::Read;

use anyhow::{bail, Context};
use rc_zip_sync::ReadZip as _;
use itertools::Itertools;
use tracing::{info, trace};

use crate::{extractors::mojang_mappings::MojmapIdentPath, mappings::MappingsBrand};

pub struct MappedField {
    
}

pub struct MappedClass {
    pub name: String,
}

#[derive(Debug, bincode::Encode)]
pub struct MappedClassExtractor {
    pub class: String,
    pub mappings: MappingsBrand,
}

impl super::ExtractorKind for MappedClassExtractor {
    type Output = ();

    fn config(&self) -> super::ExtractorConfig {
        super::ExtractorConfig { store_output_in_cache: false }
    }
    
    fn name(&self) -> &'static str {
        "mapped_class_extractor"
    }

    async fn extract(self, manager: &mut super::ExtractionManager<'_>) -> anyhow::Result<Self::Output> {
        match self.mappings {
            MappingsBrand::Mojmaps => get_with_mojmaps(manager, &self.class).await,
        }
    }
}

async fn get_with_mojmaps(manager: &mut super::ExtractionManager<'_>, class: &str) -> anyhow::Result<<MappedClassExtractor as super::ExtractorKind>::Output> {
    let mappings = manager.extract(super::mojang_mappings::MojangMappingsExtractor).await?;

    let Some(class_map) = mappings.class_mappings.iter()
        .find(|class_map| class_map.name.eq_str(class))
        .cloned()
    else { bail!("Could not find class '{class}'") };

    trace!("Found class {class} at {}", class_map.obfuscated_name);

    let server_jar_path = manager.extract(super::server_jar::ServerJarExtractor).await?;

    tokio::task::spawn_blocking(move || -> anyhow::Result<()> {
        let server_jar_file = std::fs::File::open(&*server_jar_path)?;
        let zip_file = server_jar_file.read_zip()?;

        #[expect(unstable_name_collisions)]
        let class_path = class_map.obfuscated_name.0.iter()
            .map(|p| p.0.as_str())
            .intersperse("/")
            .chain([".class"])
            .collect::<String>();

        let Some(version_file_entry) = zip_file.by_name(&class_path)
        else { bail!("Could not find the class file at {class_path}") };

        let mut data = Vec::new();
        version_file_entry.reader().read_to_end(&mut data)
            .with_context(|| format!("Reading entry of server.jar at {class_path}"))?;

        let noak_class = noak::reader::Class::new(&data)
            .with_context(|| format!("Reading class at {class_path}"))?;

        println!("Class name: {}", class_map.name);

        for field in noak_class.fields() {
            let field = field?;
            let field_name = noak_class.pool().get(field.name())?.content.to_str().unwrap_or_default();
            let field_desc = noak_class.pool().get(field.descriptor())?.content.to_str().unwrap_or_default();

            let found = class_map.item_mappings.iter()
                .filter_map(|map| { <&crate::extractors::mojang_mappings::MojmapFieldMapping>::try_from(map).ok() })
                .filter(|m| m.obfuscated_name.eq_str(field_name))
                .map(|found| format!("{}", found.name))
                .collect_vec();

            println!("Field {field_name} aka {}", found.iter().map(String::as_str).intersperse(", ").collect::<String>());
            println!("  {field_desc}");
        }

        for method in noak_class.methods() {
            let method = method?;
            let method_name = noak_class.pool().get(method.name())?;
            let str_name = method_name.content.to_str().unwrap_or_default();
            let found = class_map.item_mappings.iter()
                .filter_map(|map| { <&crate::extractors::mojang_mappings::MojmapMethodMapping>::try_from(map).ok() })
                .filter(|m| m.obfuscated_name.eq_str(str_name))
                .map(|found| format!("{}", found.name))
                .collect_vec();

            println!("Real names: {}", found.iter().map(String::as_str).intersperse(", ").collect::<String>());
            let method_desc = noak_class.pool().get(method.descriptor()).with_context(|| format!("Method desc"))?;
            let str_desc = method_desc.content.to_str().unwrap_or_default();
            println!("  desc: {str_desc}");

            for attr in method.attributes() {
                let Ok(attr) = attr
                else { continue };
                if let Ok(noak::reader::AttributeContent::Code(code)) = attr.read_content(noak_class.pool()) {
                    // Iterate through the instructions
                    for instruction in code.raw_instructions() {
                        let instr = instruction.with_context(|| format!("Instruction decode error"))?;
                        println!("  {:?}", instr);
                    }
                }
            }
        }

        if let Some(sup) = noak_class.super_class().and_then(|sup| noak_class.pool().get(sup).ok()) {
            let name = noak_class.pool().get(sup.name)?;
            println!("{}", name.content.to_str().unwrap_or("<error>"));
        }

        println!("Sucessfully read: {noak_class:#?}");

        Ok(())
    }).await?
}
