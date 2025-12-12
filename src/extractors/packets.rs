use tracing::warn;

use crate::extractors::decomp_class;


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

async fn extract_modern(manager: &mut super::ExtractionManager<'_>) -> anyhow::Result<Packets> {
    let protocols = vec![
        manager.extract(decomp_class::DecompClassExtractor {
            class: "net.minecraft.network.protocol.handshake.HandshakeProtocols".to_string(),
            mappings_brand: crate::mappings::Brand::Mojmaps,
        }).await?
    ];

    for protocol in &protocols {
        let serverbound_template = protocol.fields.iter().find(|l| l.access_flags.static_ && l.name.0 == "SERVERBOUND_TEMPLATE");
        let clientbound_template = protocol.fields.iter().find(|l| l.access_flags.static_ && l.name.0 == "CLIENTBOUND_TEMPLATE");

        if let Some(serverbound_template) = serverbound_template { 'serverbound: {
            let Some(init_value) = &serverbound_template.init_value
            else { warn!("Found no serverbound init value"); break 'serverbound };

            let ctx = crate::minijvm::decomped::ClassPrintContext::new(&protocol);
            let ctx = crate::minijvm::decomped::MethodPrintContext::new(&ctx, true, &[]);
            println!("serverbound -> {}", init_value.printed(&ctx));
        }}

        if let Some(clientbound_template) = clientbound_template { 'clientbound: {
            let Some(init_value) = &clientbound_template.init_value
            else { warn!("Found no clientbound init value"); break 'clientbound };

            let ctx = crate::minijvm::decomped::ClassPrintContext::new(&protocol);
            let ctx = crate::minijvm::decomped::MethodPrintContext::new(&ctx, true, &[]);
            println!("clientbound -> {}", init_value.printed(&ctx));
        }}
    }

    Ok(Packets {
        
    })
}
