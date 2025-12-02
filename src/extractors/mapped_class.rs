mod descriptors;
use descriptors::*;

use crate::mappings;

use std::{io::Read, path::Path, sync::Arc};

use anyhow::{anyhow, bail, Context};
use rc_zip_sync::ReadZip as _;
use itertools::Itertools;
use tracing::trace;

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct MappedField {
    pub name: mappings::Ident,
    pub ty: mappings::Type,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct MappedMethod {
    
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct MappedClass {
    pub name: mappings::IdentPath,
    pub super_class: mappings::IdentPath,
    pub fields: Vec<MappedField>,
    pub methods: Vec<MappedMethod>,
}

#[derive(Debug, bincode::Encode)]
pub struct MappedClassExtractor {
    pub class: String,
    pub mappings_brand: mappings::Brand,
}

impl MappedClassExtractor {
    fn map_class(mappings: Arc<mappings::Mappings>, server_jar_path: &Path, class: &str) -> anyhow::Result<<Self as super::ExtractorKind>::Output> {
        let Some(class_map) = mappings.get_class(class)
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

        print!("class: {}", class_map.name);

        if let Some(sup) = noak_class.super_class().and_then(|sup| noak_class.pool().get(sup).ok()) {
            let name = noak_class.pool().get(sup.name)?;
            print!(" extends {}", name.content.to_str().unwrap_or("<error>"));
        }

        println!();

        for field in noak_class.fields() {
            let field = field.with_context(|| anyhow!("Could not decode field"))?;
            let field_name = noak_class.pool().get(field.name())?.content.to_str().unwrap_or_default();
            let field_desc = noak_class.pool().get(field.descriptor())?.content.to_str().unwrap_or_default();

            let td = mappings.parse_and_map_type_descriptor(field_desc)?;

            let mapped = class_map.map_field(field_name, &format!("{td}")).ok_or_else(|| anyhow!("Could not find field '{field_name}' in class '{}'", class_map.name))?;
            println!("\n{} {}", mapped.ty, mapped.name);

            println!("{:?}", field.access_flags());
            for attr in field.attributes() {
                let Ok(attr) = attr
                else { continue };
                println!("Attr: {}", noak_class.pool().get(attr.name())?.content.to_str().unwrap_or_default());
            }
        }

        for method in noak_class.methods() {
            let method = method.with_context(|| anyhow!("Could not decode method"))?;

            let method_name = noak_class.pool().get(method.name())?.content.to_str().unwrap_or_default();
            let method_desc = noak_class.pool().get(method.descriptor())?.content.to_str().unwrap_or_default();

            let md = mappings.parse_and_map_method_descriptor(method_desc)?;
            let mapped = class_map.map_method_with_desc(method_name, &md)
                .ok_or_else(|| anyhow!("Could not find method '{method_name}' in class '{}'", class_path))?;

            println!("\n{method_name} -> {} {}({})",
                mapped.return_type, mapped.name, mapped.arguments.iter().map(|h| h.formatted()).intersperse(",".to_string()).collect::<String>());

            println!("{:?}", method.access_flags());
            for attr in method.attributes() {
                let Ok(attr) = attr
                else { continue };
                println!("Attr: {}", noak_class.pool().get(attr.name())?.content.to_str().unwrap_or_default());

                if let Ok(noak::reader::AttributeContent::Code(code)) = attr.read_content(noak_class.pool()) {
                    // Iterate through the instructions
                    for instruction in code.raw_instructions() {
                        let (_, instr) = instruction.with_context(|| format!("Instruction decode error"))?;
                        use noak::reader::attributes::RawInstruction as Instr;
                        print!("  ");
                        match &instr {
                            Instr::GetStatic { index } => {
                                let field_ref = noak_class.pool().get(*index)?;
                                let class = noak_class.pool().get(field_ref.class)?;

                                let obf_class_name = noak_class.pool().get(class.name)?.content.to_str().unwrap_or_default();
                                let instruction_class_map = mappings.map_class(&obf_class_name);
                                let class_name = instruction_class_map.map(|cm| cm.name.0.as_str()).unwrap_or(obf_class_name);

                                let name_and_type = noak_class.pool().get(field_ref.name_and_type)?;
                                let obf_field_name = noak_class.pool().get(name_and_type.name)?.content.to_str().unwrap_or_default();
                                let obf_descriptor = noak_class.pool().get(name_and_type.descriptor)?.content.to_str().unwrap_or_default();
                                let field_descriptor = mappings.parse_and_map_type_descriptor(obf_descriptor)?;
                                // let field_name = instruction_class_map.map_field(obf_name, &field_descriptor.to_string()).map(|f| f.name.0.as_str()).unwrap_or("UNKNOWN");
                                let field_name = instruction_class_map.map(|class_map| {
                                    class_map.map_field(obf_field_name, &field_descriptor.to_string()).map(|f| f.name.0.as_str()).unwrap_or("UNKNOWN")
                                }).unwrap_or(obf_field_name);

                                println!("{instr:?} - {field_descriptor} {field_name} on cass {class_name}");
                            },
                            Instr::PutStatic { index } => {
                                let field_ref = noak_class.pool().get(*index)?;
                                let class = noak_class.pool().get(field_ref.class)?;

                                let obf_class_name = noak_class.pool().get(class.name)?.content.to_str().unwrap_or_default();
                                let instruction_class_map = mappings.map_class(&obf_class_name);
                                let class_name = instruction_class_map.map(|cm| cm.name.0.as_str()).unwrap_or(obf_class_name);

                                let name_and_type = noak_class.pool().get(field_ref.name_and_type)?;
                                let obf_field_name = noak_class.pool().get(name_and_type.name)?.content.to_str().unwrap_or_default();
                                let obf_descriptor = noak_class.pool().get(name_and_type.descriptor)?.content.to_str().unwrap_or_default();
                                let field_descriptor = mappings.parse_and_map_type_descriptor(obf_descriptor)?;
                                // let field_name = instruction_class_map.map_field(obf_name, &field_descriptor.to_string()).map(|f| f.name.0.as_str()).unwrap_or("UNKNOWN");
                                let field_name = instruction_class_map.map(|class_map| {
                                    class_map.map_field(obf_field_name, &field_descriptor.to_string()).map(|f| f.name.0.as_str()).unwrap_or("UNKNOWN")
                                }).unwrap_or(obf_field_name);

                                println!("{instr:?} - {field_descriptor} {field_name} on class {class_name}");
                            },
                            Instr::InvokeDynamic { index } => {
                                let invoke_dyn = noak_class.pool().get(*index)?;
                                let name_and_type = noak_class.pool().get(invoke_dyn.name_and_type)?;
                                let name = noak_class.pool().get(name_and_type.name)?.content.to_str().unwrap_or_default();
                                let descriptor = noak_class.pool().get(name_and_type.descriptor)?.content.to_str().unwrap_or_default();
                                println!("{instr:?} - {name}{descriptor}");
                            },
                            Instr::InvokeVirtual { index } => {
                                let method_ref = noak_class.pool().get(*index)?;
                                let name_and_type = noak_class.pool().get(method_ref.name_and_type)?;
                                let name = noak_class.pool().get(name_and_type.name)?.content.to_str().unwrap_or_default();
                                let obf_descriptor = noak_class.pool().get(name_and_type.descriptor)?.content.to_str().unwrap_or_default();
                                let descriptor = mappings.parse_and_map_method_descriptor(obf_descriptor)?;
                                println!("{instr:?} - {} {name}({})", descriptor.return_type, descriptor.args.iter()
                                    .map(ToString::to_string).intersperse(",".to_string()).collect::<String>());
                            },
                            Instr::InvokeInterface { index, count: _ } => {
                                let interface_method = noak_class.pool().get(*index)?;

                                let class = noak_class.pool().get(interface_method.class)?;
                                let obf_class_name = noak_class.pool().get(class.name)?.content.to_str().unwrap_or("<ERROR>");
                                let instruction_class_map = mappings.map_class(&obf_class_name);
                                let class_name = instruction_class_map.map(|cm| cm.name.0.as_str()).unwrap_or(obf_class_name);

                                let name_and_type = noak_class.pool().get(interface_method.name_and_type)?;
                                let obf_method_name = noak_class.pool().get(name_and_type.name)?.content.to_str().unwrap_or("<ERROR>");
                                let obf_method_descriptor = noak_class.pool().get(name_and_type.descriptor)?.content.to_str().unwrap_or("<ERROR>");
                                let method_descriptor = mappings.parse_and_map_method_descriptor(obf_method_descriptor)?;
                                
                                let method_name = instruction_class_map.and_then(|map|
                                    map.map_method_with_desc(obf_method_name, &method_descriptor)
                                ).map(|m| m.name.0.as_str()).unwrap_or(obf_method_name);

                                println!("{instr:?} - {method_name}{method_descriptor} on class {class_name}");
                            },
                            Instr::InvokeSpecial { index } => {
                                let invoke_dyn = noak_class.pool().get(*index)?;
                                println!("{instr:?} - {invoke_dyn:?}");
                                // let name_and_type = noak_class.pool().get(invoke_dyn.name_and_type)?;
                                // let name = noak_class.pool().get(name_and_type.name)?.content.to_str().unwrap_or_default();
                                // let descriptor = noak_class.pool().get(name_and_type.descriptor)?.content.to_str().unwrap_or_default();
                            },
                            Instr::InvokeStatic { index } => {
                                let item = noak_class.pool().get(*index)?;
                                match item {
                                    noak::reader::cpool::Item::MethodRef(method_ref) => {
                                        let name_and_type = noak_class.pool().get(method_ref.name_and_type)?;
                                        let name = noak_class.pool().get(name_and_type.name)?.content.to_str().unwrap_or_default();
                                        let descriptor = noak_class.pool().get(name_and_type.descriptor)?.content.to_str().unwrap_or_default();
                                        println!("{instr:?} - {name}{descriptor}", );
                                    },
                                    _ => println!("{instr:?} - {item:?}"),
                                }

                            },
                            other => println!("{:?}", other),
                        }
                    }
                }
            }
        }

        println!("Sucessfully read: {noak_class:#?}");

        Ok(todo!())
    }
}

impl super::ExtractorKind for MappedClassExtractor {
    type Output = MappedClass;

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
