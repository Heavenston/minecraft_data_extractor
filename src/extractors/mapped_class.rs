use crate::{extractors::mojang_mappings, mappings, minijvm};

use anyhow::bail;
use tracing::warn;

fn type_from_class_name(name: minijvm::IdentPath) -> minijvm::TypeDescriptor {
    minijvm::TypeDescriptor {
        ty: minijvm::TypeDescriptorKind::Object(name),
        array_depth: 0,
    }
}

trait MappingsExt {
    fn map_class_ref(&self, class_ref: &minijvm::ClassRef) -> minijvm::ClassRef;
    fn map_method_ref(&self, method_ref: &minijvm::MethodRef) -> minijvm::MethodRef;
    fn map_field_ref(&self, field_ref: &minijvm::FieldRef) -> minijvm::FieldRef;
    fn map_method_handle_ref(&self, handle_ref: &minijvm::MethodHandleRef) -> minijvm::MethodHandleRef;
    fn map_constant(&self, constant: &minijvm::Constant) -> minijvm::Constant;
}

impl MappingsExt for mappings::Mappings {
    fn map_class_ref(&self, class_ref: &minijvm::ClassRef) -> minijvm::ClassRef {
        minijvm::ClassRef {
            descriptor: class_ref.descriptor.to_mapped(self),
        }
    }

    #[tracing::instrument(skip(self))]
    fn map_method_ref(&self, method_ref: &minijvm::MethodRef) -> minijvm::MethodRef {
        let Some(class_name) = method_ref.class.descriptor.simple_class_name()
        else { return method_ref.clone() };
        let Some(mapped_class) = self.map_class(&*class_name)
        else { return method_ref.clone() };
        let descriptor = method_ref.descriptor.to_mapped(self);
        let name = mapped_class.map_method(&method_ref.name.0, &descriptor)
            .map(|method| method.name.clone())
            .unwrap_or_else(|| {
                warn!(?method_ref, class_name = %mapped_class.name, obfuscated_class_name = %class_name, "Did not find method in mappings");
                method_ref.name.clone()
            });
        minijvm::MethodRef {
            class: minijvm::ClassRef { descriptor: type_from_class_name(mapped_class.name.clone()) },
            name,
            descriptor,
        }
    }

    #[tracing::instrument(skip(self))]
    fn map_field_ref(&self, field_ref: &minijvm::FieldRef) -> minijvm::FieldRef {
        let Some(class_name) = field_ref.class.descriptor.simple_class_name()
        else { return field_ref.clone() };
        let Some(mapped_class) = self.map_class(&*class_name)
        else { return field_ref.clone() };
        let descriptor = field_ref.descriptor.to_mapped(self);
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
    }

    fn map_method_handle_ref(&self, handle_ref: &minijvm::MethodHandleRef) -> minijvm::MethodHandleRef {
        match handle_ref {
            minijvm::MethodHandleRef::Method(m) => minijvm::MethodHandleRef::Method(self.map_method_ref(m)),
            minijvm::MethodHandleRef::Field(f) => minijvm::MethodHandleRef::Field(self.map_field_ref(f)),
        }
    }

    fn map_constant(&self, constant: &minijvm::Constant) -> minijvm::Constant {
        match constant {
            minijvm::Constant::Class(class_ref) => minijvm::Constant::Class(self.map_class_ref(class_ref)),
            minijvm::Constant::MethodHandle(handle) => minijvm::Constant::MethodHandle(minijvm::MethodHandle {
                kind: handle.kind.clone(),
                reference: self.map_method_handle_ref(&handle.reference),
            }),
            minijvm::Constant::MethodType(method_descriptor) => minijvm::Constant::MethodType(method_descriptor.to_mapped(self)),
            _ => constant.clone(),
        }
    }
}

#[derive(Debug, bincode::Encode)]
pub struct MappedClassExtractor {
    pub class: String,
    pub mappings_brand: mappings::Brand,
}

impl MappedClassExtractor {
    #[tracing::instrument(level = "debug", skip(mappings, _class_map))]
    fn map_instruction(mappings: &mappings::Mappings, _class_map: &mappings::Class, instruction: &minijvm::Instruction) -> minijvm::Instruction {
        use minijvm::Instruction as Instr;

        // FIXME: This can very easily be out-of-sync with the instructions
        match instruction {
            Instr::Ldc { constant } => Instr::Ldc { constant: mappings.map_constant(constant) },
            Instr::InstanceOf { class } => Instr::InstanceOf { class: mappings.map_class_ref(class) },
            Instr::Invoke { kind, method } => Instr::Invoke { kind: kind.clone(), method: mappings.map_method_ref(method) },
            Instr::InvokeDynamic { call_site, name, descriptor } => {
                Instr::InvokeDynamic {
                    call_site: minijvm::DynamicCallSite {
                        bootstrap: mappings.map_method_ref(&call_site.bootstrap),
                        method_kind: call_site.method_kind.clone(),
                        static_args: call_site.static_args.iter()
                            .map(|c| mappings.map_constant(c))
                            .collect(),
                    },
                    // we dont bother mapping these, presumable minecraft do not
                    // do custom invoke dynamics so this will be internal java
                    // method
                    name: name.clone(),
                    descriptor: descriptor.clone(),
                }
            },
            Instr::GetField { is_static, field } => Instr::GetField { is_static: *is_static, field: mappings.map_field_ref(field) },
            Instr::PutField { is_static, field } => Instr::PutField { is_static: *is_static, field: mappings.map_field_ref(field) },
            Instr::New { class } => Instr::New { class: mappings.map_class_ref(class) },
            Instr::CheckCast { class } => Instr::CheckCast { class: mappings.map_class_ref(class) },

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
            signature: class.signature.as_ref().map(|sign| sign.to_mapped(mappings)),
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
                        signature: field.signature.as_ref().map(|sign| sign.to_mapped(mappings)),
                        constant_value: field.constant_value.as_ref().map(|mc| mappings.map_constant(mc)),
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
                        signature: method.signature.as_ref().map(|sign| sign.to_mapped(mappings)),
                        code: method.code.as_ref().map(|code| minijvm::Code {
                            instructions: code.instructions.iter()
                                .map(|instr| Self::map_instruction(mappings, class_map, instr))
                                .collect(),
                        }),
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
            // Versions without obfuscation
            mappings::Brand::Mojmaps if manager.version().release_time > *mojang_mappings::MOJMAPS_LAST_VERSION_TIME => {
                let decomped_class = manager.extract(super::read_class::ReadClassExtractor {
                    class: self.class.clone(),
                }).await?;

                return Ok((*decomped_class).clone());
            },
            mappings::Brand::Mojmaps => {
                manager.extract(super::mojang_mappings::MojangMappingsExtractor).await?
            },
        };

        let Some(class_map) = mappings.get_class(&self.class)
        else { bail!("No class with name '{}' in mappings", self.class) };

        let decomped_class = manager.extract(super::read_class::ReadClassExtractor {
            class: class_map.obfuscated_name.to_string(),
        }).await?;

        crate::spawn_cpu_bound(move || {
            Self::map_class(&mappings, mappings.get_class(&self.class).unwrap(), &decomped_class)
        }).await?
    }
}
