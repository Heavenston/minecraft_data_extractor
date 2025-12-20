use std::collections::HashMap;
use std::sync::Arc;

use anyhow::{anyhow, bail};
use tracing::{ warn, error };

use crate::extractors::{ self, decomp_class };
use crate::minijvm::decomped::visitor::ref_visitor::{ self as rv, RefVisitor };
use crate::{ minijvm::{ self, decomped } };
use super::{ Packet, Packets, ProtocolState, DataType };

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum PacketDirection {
    Serverbound,
    Clientbound,
}

#[derive(Debug)]
struct AddedPacket {
    packet_class_name: minijvm::ClassRef,
    #[expect(dead_code)]
    packet_type_field: minijvm::FieldRef,
}

#[derive(Default)]
struct AddPacketVisitor {
    packets: Vec<AddedPacket>,
}

impl rv::RefVisitor for AddPacketVisitor {
    fn visit_expression(&mut self, expr: &decomped::Expression) -> anyhow::Result<()> {
        rv::walk_expression(self, expr)?;

        match expr {
            decomped::Expression::Invoke { method, args, .. }
                if method.name.0 == "addPacket"
            => {
                let Some(packet_class_name) = (match args.get(0) {
                    Some(decomped::Expression::GetField { field, .. }) => Some(field.class.clone()),
                    _ => None,
                }) else { bail!("Could not get packet class name") };
                let Some(packet_type_field) = (match args.get(1) {
                    Some(decomped::Expression::GetField { field, .. }) => Some(field.clone()),
                    _ => None,
                }) else { bail!("Could not get packet type field") };

                self.packets.push(AddedPacket {
                    packet_class_name,
                    packet_type_field,
                });
            },
            _ => (),
        }

        Ok(())
    }
}

async fn read_java_data_type(manager: &mut extractors::ExtractionManager<'_>, signature: &minijvm::JavaTypeSignature) -> anyhow::Result<Arc<DataType>> {
    match Box::pin(manager.extract(JavaDataTypeExtractor { signature: signature.clone() })).await {
        Ok(dt) => Ok(dt),
        Err(error) => {
            error!(?signature, %error, "Error while extracting java data type");
            return Ok(Arc::new(DataType::Error { name: format!("{signature:?}") }));
        },
    }
}

#[derive(derive_more::Debug, bincode::Encode)]
struct JavaDataTypeExtractor {
    #[debug("{signature}")]
    signature: minijvm::JavaTypeSignature,
}

impl extractors::ExtractorKind for JavaDataTypeExtractor {
    type Output = DataType;

    fn name(&self) -> &'static str {
        "java_data_type_extractor"
    }

    #[tracing::instrument(name = "extract_java_data_type", skip(manager), fields(version_id = manager.version().id))]
    async fn extract(self, manager: &mut extractors::ExtractionManager<'_>) -> anyhow::Result<DataType> {
        use minijvm::signatures::{
            BaseTypeSignature as Base,
            JavaTypeSignature as TypeSign,
            ReferenceTypeSignature as RefSign,
        };
        let ref_type = match self.signature {
            TypeSign::Reference(ref_type) => ref_type,
            TypeSign::Base(Base::Byte) => return Ok(DataType::Byte),
            TypeSign::Base(Base::Char) => return Ok(DataType::Char),
            TypeSign::Base(Base::Double) => return Ok(DataType::Double),
            TypeSign::Base(Base::Float) => return Ok(DataType::Float),
            TypeSign::Base(Base::Int) => return Ok(DataType::Int),
            TypeSign::Base(Base::Long) => return Ok(DataType::Long),
            TypeSign::Base(Base::Short) => return Ok(DataType::Short),
            TypeSign::Base(Base::Boolean) => return Ok(DataType::Boolean),
        };

        let class_type = match ref_type {
            RefSign::Class(class_type) => class_type,
            RefSign::TypeVariable(v) => return Ok(DataType::Other {
                name: format!("typevar({v})"),
                type_args: vec![],
            }),
            RefSign::Array(array) => return Ok(DataType::Array({
                let element = Box::pin(manager.extract(Self {
                    signature: (*array.ty).clone(),
                })).await?;
                return Ok(DataType::Array(Box::new((*element).clone())));
            })),
        };

        let class_name = class_type.class_name();

        // Convert all type arguments to DataTypes
        let type_args: Vec<DataType> = {
            let mut type_args = vec![];
            for inner_ty in &class_type.class.type_arguments {
                let inner_dt = match inner_ty {
                    minijvm::signatures::TypeArgument::Type { wildcard_indicator: None, ty } => {
                        (*Box::pin(manager.extract(Self {
                            signature: TypeSign::Reference(ty.clone()),
                        })).await?).clone()
                    },
                    type_parameter => {
                        warn!(?type_parameter, "Unsupported wildcard inside Optional/List");
                        DataType::Other { name: "*".into(), type_args: vec![] }
                    },
                };
                type_args.push(inner_dt)
            }
            type_args
        };

        match class_name.as_str() {
            "java.lang.String" => return Ok(DataType::String),
            "java.lang.Integer" => return Ok(DataType::Int),
            "java.util.UUID" => return Ok(DataType::UUID),
            "java.util.Optional" | "java.util.List" => {
                let inner_dt = type_args.first()
                    .ok_or_else(|| anyhow!("java.util.Optional/List without any type argument?"))?;
                return Ok(match class_name.as_str() {
                    "java.util.Optional" => DataType::Optional(Box::new(inner_dt.clone())),
                    "java.util.List" => DataType::List(Box::new(inner_dt.clone())),
                    _ => unreachable!(),
                });
            },
            s if s.starts_with("net.minecraft") => (/* We can read these classes */),
            s => {
                warn!(%class_name, "Unknown ty");
                return Ok(DataType::Other { name: s.into(), type_args });
            },
        }

        let decomped_class = manager.extract(decomp_class::DecompClassExtractor {
            class: class_name.clone(),
            mappings_brand: crate::mappings::Brand::Mojmaps,
        }).await?;

        if decomped_class.is_enum() {
            // TODO: Variants can be more than just a name as string

            let mut variants = Vec::<String>::new();
            for var in &decomped_class.enum_variants {
                variants.push(var.name.to_string());
            }
            Ok(DataType::Enum(super::EnumType {
                name: class_name.clone(),
                variants
            }))
        }
        else {
            let mut fields = HashMap::<String, DataType>::new();

            for field in &decomped_class.fields {
                if field.access_flags.static_ { continue; }
                let ty = read_java_data_type(manager, &field.signature).await?;
                fields.insert(field.name.to_string(), (*ty).clone());
            }

            Ok(DataType::Record(super::RecordType {
                name: class_name.clone(),
                fields,
            }))
        }
    }
}

async fn extract_packet(manager: &mut extractors::ExtractionManager<'_>, packet_class: &minijvm::ClassRef) -> anyhow::Result<Packet> {
    let Some(packet_class_name) = packet_class.descriptor.simple_class_name()
    else { bail!("Could not get a simple packet class name") };

    // let decomped_packet_class = manager.extract(decomp_class::DecompClassExtractor {
    //     class: packet_class_name.to_string(),
    //     mappings_brand: crate::mappings::Brand::Mojmaps,
    // }).await?;

    let packet_class_data_type = read_java_data_type(manager, &packet_class.descriptor.to_signature()).await?;
    let DataType::Record(packet_class_record_type) = &*packet_class_data_type
    else { bail!("Expected packet class to be a record but got: {packet_class_data_type:?}") };

    Ok(Packet {
        display_name: packet_class_name.last_name().trim_end_matches("Packet").to_string(),
        record: packet_class_record_type.clone(),
    })
}

#[tracing::instrument(skip_all)]
pub(super) async fn extract_using_protocols(manager: &mut extractors::ExtractionManager<'_>) -> anyhow::Result<Packets> {
    let protocols_names = vec![
        "net.minecraft.network.protocol.handshake.HandshakeProtocols",
        "net.minecraft.network.protocol.status.StatusProtocols",
        "net.minecraft.network.protocol.configuration.ConfigurationProtocols",
        "net.minecraft.network.protocol.login.LoginProtocols",
        "net.minecraft.network.protocol.game.GameProtocols",
    ];
    let mut protocols = Vec::new();
    for class_name in protocols_names {
        protocols.push(
            manager.extract(decomp_class::DecompClassExtractor {
                class: class_name.to_string(),
                mappings_brand: crate::mappings::Brand::Mojmaps,
            }).await?
        );
    }

    let mut states = Vec::new();

    for protocol in &protocols {
        let templates = [
            (PacketDirection::Serverbound, protocol.fields.iter().find(|l| l.access_flags.static_ && l.name.0 == "SERVERBOUND_TEMPLATE")),
            (PacketDirection::Clientbound, protocol.fields.iter().find(|l| l.access_flags.static_ && l.name.0 == "CLIENTBOUND_TEMPLATE")),
        ];

        let mut packets: HashMap<PacketDirection, Vec<Packet>> = Default::default();

        for (direction, field) in templates {
            let Some(field) = field
            else { continue };

            let Some(init_value) = &field.init_value
            else { warn!("Found no serverbound init value"); continue };

            // Find the name of the method (it's actually a lambda, compiled into a method)
            // that adds all packets of this protocol
            let add_packets_method_name = match init_value {
                decomped::Expression::Invoke { method: minijvm::MethodRef { name: method_name, .. }, args, .. }
                    if &method_name.0 == "serverboundProtocol" || &method_name.0 == "clientboundProtocol"
                => {
                    match &args[0] {
                        decomped::Expression::Lambda { target, .. } => &target.name,
                        _ => {
                            warn!(arg = ?args[0], "Unexpected first argument of {direction:?}"); continue;
                        },
                    }
                },
                _ => {
                    warn!(?init_value, "Unexpected init value for {direction:?}"); continue;
                },
            };

            let Some(add_packets_method) = protocol.methods.iter().find(|m| &m.name == add_packets_method_name)
            else { bail!("Could not find method {add_packets_method_name}") };

            let mut added_packet = AddPacketVisitor::default();
            added_packet.visit_method(add_packets_method)?;

            for added_packet in &added_packet.packets {
                let packet = match extract_packet(manager, &added_packet.packet_class_name).await {
                    Ok(p) => p,
                    Err(error) => {
                        error!(%error, ?added_packet, "Error while reading packet");
                        continue;
                    },
                };
                packets.entry(direction).or_default().push(packet);
            }
        }

        states.push(ProtocolState {
            name: protocol.name.last_name().trim_end_matches("Protocols").to_string(),
            serverbound_packets: packets.remove(&PacketDirection::Serverbound).unwrap_or_default(),
            clientbound_packets: packets.remove(&PacketDirection::Clientbound).unwrap_or_default(),
        });
    }

    Ok(Packets { states })
}
