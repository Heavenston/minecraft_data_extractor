use crate::{mappings, minijvm};

use anyhow::bail;
use tracing::warn;

#[derive(Debug, bincode::Encode)]
pub struct MappedClassExtractor {
    pub class: String,
    pub mappings_brand: mappings::Brand,
}

impl MappedClassExtractor {
    fn map_instruction(mappings: &mappings::Mappings, _class_map: &mappings::Class, instruction: &minijvm::Instruction) -> minijvm::Instruction {
        use minijvm::Instruction as Instr;
        use minijvm::Constant as Const;

        let map_class_ref = |class_ref: &minijvm::ClassRef| -> minijvm::ClassRef {
            minijvm::ClassRef {
                descriptor: class_ref.descriptor.to_mapped(mappings),
            }
        };
        let class_name_from_type = |ty: &minijvm::TypeDescriptor| -> Option<minijvm::IdentPath> {
            match ty {
                minijvm::TypeDescriptor {
                    ty: minijvm::TypeDescriptorKind::Object(name),
                    array_depth: 0,
                } => Some(name.clone()),
                _ => None,
            }
        };
        let type_from_class_name = |name: minijvm::IdentPath| -> minijvm::TypeDescriptor {
            minijvm::TypeDescriptor {
                ty: minijvm::TypeDescriptorKind::Object(name),
                array_depth: 0,
            }
        };
        let map_method_ref = |method_ref: &minijvm::MethodRef| -> minijvm::MethodRef {
            let Some(class_name) = class_name_from_type(&method_ref.class.descriptor) else { return method_ref.clone() };
            let Some(mapped_class) = mappings.map_class(&*class_name)
            else { return method_ref.clone() };
            let descriptor = method_ref.descriptor.to_mapped(mappings);
            let name = mapped_class.map_method(&method_ref.name.0, &descriptor)
                .map(|method| method.name.clone())
                .unwrap_or_else(|| {
                    warn!(obfuscated_method_name = %method_ref.name, class_name = %mapped_class.name, obfuscated_class_name = %class_name, "Did not find method in mappings");
                    method_ref.name.clone()
                });
            minijvm::MethodRef {
                class: minijvm::ClassRef { descriptor: type_from_class_name(mapped_class.name.clone()) },
                name,
                descriptor,
            }
        };
        let map_field_ref = |field_ref: &minijvm::FieldRef| -> minijvm::FieldRef {
            let Some(class_name) = class_name_from_type(&field_ref.class.descriptor) else { return field_ref.clone() };
            let Some(mapped_class) = mappings.map_class(&*class_name)
            else { return field_ref.clone() };
            let descriptor = field_ref.descriptor.to_mapped(mappings);
            let name = mapped_class.map_field(&field_ref.name.0, &descriptor)
                .map(|field| field.name.clone())
                .unwrap_or_else(|| {
                    warn!(obfuscated_field_name = %field_ref.name, class_name = %mapped_class.name, obfuscated_class_name = %class_name, "Did not find field in mappings");
                    field_ref.name.clone()
                });
            minijvm::FieldRef {
                class: minijvm::ClassRef { descriptor: type_from_class_name(mapped_class.name.clone()) },
                name,
                descriptor,
            }
        };

        let map_constant = |constant: &minijvm::Constant| -> minijvm::Constant {
            match constant {
                Const::Class(class_ref) => Const::Class(map_class_ref(class_ref)),
                Const::MethodHandle(method_ref) => Const::MethodHandle(map_method_ref(method_ref)),
                Const::MethodType(method_descriptor) => Const::MethodType(method_descriptor.to_mapped(mappings)),
                _ => constant.clone(),
            }
        };

        // FIXME: This can very easily be out-of-sync with the instructions
        match instruction {
            Instr::Ldc { constant } => Instr::Ldc { constant: map_constant(constant) },
            Instr::Invoke { kind, method } => Instr::Invoke { kind: kind.clone(), method: map_method_ref(method) },
            Instr::InvokeDynamic { call_site, name, descriptor } => {
                Instr::InvokeDynamic {
                    call_site: minijvm::DynamicCallSite {
                        bootstrap: map_method_ref(&call_site.bootstrap),
                        method_kind: call_site.method_kind.clone(),
                        static_args: call_site.static_args.iter()
                            .map(map_constant)
                            .collect(),
                    },
                    // we dont bother mapping these, presumable minecraft do not
                    // do custom invoke dynamics so this will be internal java
                    // method
                    name: name.clone(),
                    descriptor: descriptor.clone(),
                }
            },
            Instr::GetField { is_static, field } => Instr::GetField { is_static: *is_static, field: map_field_ref(field) },
            Instr::PutField { is_static, field } => Instr::PutField { is_static: *is_static, field: map_field_ref(field) },
            Instr::New { class } => Instr::New { class: map_class_ref(class) },
            Instr::CheckCast { class } => Instr::CheckCast { class: map_class_ref(class) },

            _ => instruction.clone(),
        }
    }

    fn map_class(mappings: &mappings::Mappings, class_map: &mappings::Class, class: &minijvm::Class) -> anyhow::Result<minijvm::Class> {
        Ok(minijvm::Class {
            access_flags: class.access_flags.clone(),
            name: class_map.name.clone(),
            super_class: class.super_class.as_ref().map(|super_class| {
                mappings.map_class(&**super_class)
                    .map(|msc| msc.name.clone())
                    .unwrap_or_else(|| super_class.clone())
            }),
            fields: class.fields.iter()
                .map(|field| {
                    let descriptor = field.descriptor.to_mapped(mappings);
                    let name = class_map.map_field(&field.name.0, &descriptor)
                        .map(|mapped_field| mapped_field.name.clone())
                        .unwrap_or_else(|| field.name.clone());
                    minijvm::Field {
                        access_flags: field.access_flags.clone(),
                        name,
                        descriptor,
                    }
                })
                .collect(),
            methods: class.methods.iter()
                .map(|method| {
                    let descriptor = method.descriptor.to_mapped(mappings);
                    let name = class_map.map_method(&method.name.0, &descriptor)
                        .map(|mapped_method| mapped_method.name.clone())
                        .unwrap_or_else(|| method.name.clone());
                    minijvm::Method {
                        access_flags: method.access_flags.clone(),
                        name,
                        descriptor,
                        code: minijvm::Code {
                            instructions: method.code.instructions.iter()
                                .map(|instr| Self::map_instruction(mappings, class_map, instr))
                                .collect(),
                        },
                    }
                })
                .collect(),
        })
    }
}

impl super::ExtractorKind for MappedClassExtractor {
    type Output = minijvm::Class;
    
    fn name(&self) -> &'static str {
        "mapped_class_extractor"
    }

    async fn extract(self, manager: &mut super::ExtractionManager<'_>) -> anyhow::Result<Self::Output> {
        let mappings = match self.mappings_brand {
            mappings::Brand::Mojmaps => manager.extract(super::mojang_mappings::MojangMappingsExtractor).await?,
        };

        let Some(class_map) = mappings.get_class(&self.class)
        else { bail!("No class with name '{}' in mappings", self.class) };

        let decomped_class = manager.extract(super::read_class::ReadClassExtractor {
            class: (&**class_map.obfuscated_name).to_string(),
        }).await?;

        crate::spawn_cpu_bound(move || {
            Self::map_class(&mappings, mappings.get_class(&self.class).unwrap(), &decomped_class)
        }).await?
    }
}
