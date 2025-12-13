use anyhow::{anyhow, bail};
use tracing::warn;

use crate::extractors::decomp_class;
use crate::minijvm::decomped::visitor::mut_visitor as mv;
use crate::{ minijvm::{ self, decomped } };

#[derive(Debug, Clone, bincode::Decode, bincode::Encode)]
pub struct Packets {
    
}

#[derive(Default, Debug, Clone, bincode::Decode, bincode::Encode)]
pub struct PacketsExtractor;

impl super::ExtractorKind for PacketsExtractor {
    type Output = Packets;

    fn name(&self) -> &'static str {
        "packets_extractor"
    }

    async fn extract(self, manager: &mut super::ExtractionManager<'_>) -> anyhow::Result<Packets> {
        extract_modern(manager).await
    }
}

struct AddedPacket {
    packet_class_name: minijvm::ClassRef,
    packet_type_field: minijvm::FieldRef,
}

#[derive(Default)]
struct AddPacketVisitor {
    packets: Vec<AddedPacket>,
}

impl mv::MutVisitor for AddPacketVisitor {
    fn visit_expression(&mut self, expr: &mut decomped::Expression) -> anyhow::Result<()> {
        mv::walk_expression(self, expr)?;

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

async fn extract_modern(manager: &mut super::ExtractionManager<'_>) -> anyhow::Result<Packets> {
    let protocols_names = vec![
        "net.minecraft.network.protocol.handshake.HandshakeProtocols",
        "net.minecraft.network.protocol.login.LoginProtocols",
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

    for protocol in &protocols {
        let serverbound_template = protocol.fields.iter().find(|l| l.access_flags.static_ && l.name.0 == "SERVERBOUND_TEMPLATE");
        let clientbound_template = protocol.fields.iter().find(|l| l.access_flags.static_ && l.name.0 == "CLIENTBOUND_TEMPLATE");

        if let Some(serverbound_template) = serverbound_template { 'serverbound: {
            let Some(init_value) = &serverbound_template.init_value
            else { warn!("Found no serverbound init value"); break 'serverbound };

            // Find the name of the method (it's actually a lambda, compiled into a method)
            // that adds all packets of this protocol
            let add_packets_method_name = match init_value {
                decomped::Expression::Invoke { method: minijvm::MethodRef { name: method_name, .. }, args, .. }
                    if &method_name.0 == "serverboundProtocol"
                => {
                    match &args[0] {
                        decomped::Expression::Lambda { target, .. } => &target.name,
                        _ => {
                            warn!(arg = ?args[0], "Unexpected first argument of serverboundProtocol"); break 'serverbound;
                        },
                    }
                },
                _ => {
                    warn!(?init_value, "Unexpected init value for SERVERBOUND_TEMPLATE"); break 'serverbound;
                },
            };

            let Some(add_packets_method) = protocol.methods.iter().find(|m| &m.name == add_packets_method_name)
            else { bail!("Could not find method {add_packets_method_name}") };

            let ctx = decomped::ClassPrintContext::new(&protocol);
            println!("serverbound -> {}", add_packets_method.printed(&ctx));
        }}

        if let Some(clientbound_template) = clientbound_template { 'clientbound: {
            let Some(init_value) = &clientbound_template.init_value
            else { warn!("Found no clientbound init value"); break 'clientbound };

            let ctx = decomped::ClassPrintContext::new(&protocol);
            let ctx = decomped::MethodPrintContext::new(&ctx, true, &[]);
            println!("clientbound -> {}", init_value.printed(&ctx));
        }}
    }

    Ok(Packets {
        
    })
}
