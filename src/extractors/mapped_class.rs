mod descriptors;
use descriptors::*;
use nom::Parser as _;

use crate::mappings;

use std::{io::Read, path::Path, sync::Arc};

use anyhow::{anyhow, bail, Context};
use rc_zip_sync::ReadZip as _;
use itertools::Itertools;
use tracing::trace;

pub struct MappedField {
    
}

pub struct MappedClass {
    pub name: String,
}

#[derive(Debug, bincode::Encode)]
pub struct MappedClassExtractor {
    pub class: String,
    pub mappings_brand: mappings::Brand,
}

impl MappedClassExtractor {
    fn map_class(mappings: Arc<mappings::Mappings>, server_jar_path: &Path, class: &str) -> anyhow::Result<<Self as super::ExtractorKind>::Output> {
        let Some(class_map) = mappings.class_mappings.iter()
            .find(|class_map| &class_map.name.0 == class)
            .cloned()
        else { bail!("Could not find class '{class}'") };

        trace!("Found class {class} at {}", class_map.obfuscated_name);

        let server_jar_file = std::fs::File::open(&*server_jar_path)?;
        let zip_file = server_jar_file.read_zip()?;

        let class_path = class_map.obfuscated_name.0.replace(".", "/") + ".class";

        let Some(version_file_entry) = zip_file.by_name(&class_path)
        else { bail!("Could not find the class file at {class_path}") };

        let mut data = Vec::new();
        version_file_entry.reader().read_to_end(&mut data)
            .with_context(|| format!("Reading entry of server.jar at {class_path}"))?;

        let noak_class = noak::reader::Class::new(&data)
            .with_context(|| format!("Reading class at {class_path}"))?;

        println!("Class name: {}", class_map.name);

        for field in noak_class.fields() {
            let field = field.with_context(|| anyhow!("Could not decode field"))?;
            let field_name = noak_class.pool().get(field.name())?.content.to_str().unwrap_or_default();
            let field_desc = noak_class.pool().get(field.descriptor())?.content.to_str().unwrap_or_default();

            let (_, obf_td) = nom::combinator::complete(TypeDescriptor::parse).parse(field_desc)
                .map_err(|e| anyhow!("Type descriptor parse error: {e}"))?;
            let td = obf_td.to_mapped(&mappings);

            let mapped = class_map.map_field(field_name, &format!("{td}")).ok_or_else(|| anyhow!("Could not find field '{field_name}' in class '{}'", class_map.name))?;
            println!("{obf_td} {field_name} -> {} {}", mapped.ty, mapped.name);
        }

        for method in noak_class.methods() {
            let method = method.with_context(|| anyhow!("Could not decode method"))?;

            let method_name = noak_class.pool().get(method.name())?.content.to_str().unwrap_or_default();
            let method_desc = noak_class.pool().get(method.descriptor())?.content.to_str().unwrap_or_default();
            print!("\n\n{method_name}\n");

            let (_, obf_md) = nom::combinator::complete(MethodDescriptor::parse).parse(method_desc)
                .map_err(|e| anyhow!("Method descriptor parse error: {e}"))?;
            let md = obf_md.to_mapped(&mappings);

            let mapped = class_map.map_method(method_name, &md.return_type.to_string(), md.args.iter().map(|h| h.to_string()))
                .ok_or_else(|| anyhow!("Could not find method '{method_name}' in class '{}'", class_path))?;

            println!("{} {}({}) -> {} {}({})",
                obf_md.return_type, method_name, obf_md.args.iter().map(|h| h.to_string()).intersperse(",".to_string()).collect::<String>(),
                mapped.return_type, mapped.name, mapped.arguments.iter().map(|h| h.formatted()).intersperse(",".to_string()).collect::<String>());

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
    }
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
        let mappings = match self.mappings_brand {
            mappings::Brand::Mojmaps => manager.extract(super::mojang_mappings::MojangMappingsExtractor).await?,
        };

        let server_jar_path = manager.extract(super::server_jar::ServerJarExtractor).await?;

        let class = self.class.clone();

        crate::spawn_cpu_bound(move || Self::map_class(mappings, &server_jar_path, &class)).await?
    }
}
