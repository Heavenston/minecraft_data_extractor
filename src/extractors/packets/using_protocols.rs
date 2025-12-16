use std::collections::HashMap;
use std::sync::Arc;

use anyhow::bail;
use tracing::{ warn, error };

use crate::extractors::{ self, decomp_class };
use crate::minijvm::decomped::visitor::ref_visitor::{ self as rv, RefVisitor };
use crate::{ minijvm::{ self, decomped } };

use super::{ Packet, Packets, PacketDirection, ProtocolState, DataType };

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

async fn read_java_data_type(manager: &mut extractors::ExtractionManager<'_>, descriptor: &minijvm::TypeDescriptor) -> anyhow::Result<Arc<DataType>> {
    match Box::pin(manager.extract(JavaDataTypeExtractor { descriptor: descriptor.clone() })).await {
        Ok(dt) => Ok(dt),
        Err(error) => {
            error!(?descriptor, %error, "Error while extracting java data type");
            return Ok(Arc::new(DataType::Error { name: format!("{descriptor:?}") }));
        },
    }
}

#[derive(Debug, bincode::Encode)]
struct JavaDataTypeExtractor {
    descriptor: minijvm::TypeDescriptor,
}

impl extractors::ExtractorKind for JavaDataTypeExtractor {
    type Output = DataType;

    fn name(&self) -> &'static str {
        "java_data_type_extractor"
    }

    #[tracing::instrument(name = "extract_java_data_type", skip(manager), fields(version_id = manager.version().id))]
    async fn extract(mut self, manager: &mut extractors::ExtractionManager<'_>) -> anyhow::Result<Self::Output> {
        if self.descriptor.array_depth > 0 {
            self.descriptor.array_depth -= 1;
            let element = Box::pin(manager.extract(self)).await?;
            return Ok(DataType::Array(Box::new((*element).clone())));
        }

        let ident_path = match &self.descriptor.ty {
            minijvm::TypeDescriptorKind::Byte => return Ok(DataType::Byte),
            minijvm::TypeDescriptorKind::Int => return Ok(DataType::Int),
            minijvm::TypeDescriptorKind::Boolean => return Ok(DataType::Boolean),
            minijvm::TypeDescriptorKind::Object(ident_path) if ident_path == "java.lang.String" => return Ok(DataType::String),
            minijvm::TypeDescriptorKind::Object(ident_path) if ident_path == "java.util.UUID" => return Ok(DataType::UUID),
            minijvm::TypeDescriptorKind::Object(ident_path) if ident_path.starts_with("net.minecraft") || ident_path.starts_with("com.mojang") => ident_path,
            ty => {
                warn!(?ty, "Unknown ty");
                return Ok(DataType::Other);
            },
        };

        let decomped_class = manager.extract(decomp_class::DecompClassExtractor {
            class: ident_path.to_string(),
            mappings_brand: crate::mappings::Brand::Mojmaps,
        }).await?;

        if decomped_class.is_enum() {
            // TODO: Variants can be more than just a name as string

            let mut variants = Vec::<String>::new();
            for var in &decomped_class.enum_variants {
                variants.push(var.name.to_string());
            }
            Ok(DataType::Enum(super::EnumType {
                name: ident_path.to_string(),
                variants
            }))
        }
        else {
            let mut fields = HashMap::<String, DataType>::new();

            for field in &decomped_class.fields {
                if field.access_flags.static_ { continue; }
                let ty = read_java_data_type(manager, &field.descriptor).await?;
                fields.insert(field.name.to_string(), (*ty).clone());
            }

            Ok(DataType::Record(super::RecordType {
                name: ident_path.to_string(),
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

    let packet_class_data_type = read_java_data_type(manager, &packet_class.descriptor).await?;
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
        "net.minecraft.network.protocol.status.StatusProtocols",
        // "net.minecraft.network.protocol.handshake.HandshakeProtocols",
        // "net.minecraft.network.protocol.login.LoginProtocols",
        // "net.minecraft.network.protocol.game.GameProtocols",
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
