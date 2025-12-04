use crate::minijvm;

use std::{ io::Read, path::Path };

use anyhow::{ anyhow, bail, Context };
use rc_zip_sync::ReadZip as _;
use tracing::warn;

#[derive(Debug, bincode::Encode)]
pub struct DecompClassExtractor {
    /// The (obfuscated) class name
    /// directly used to find its path
    pub class: String,
}

impl DecompClassExtractor {
    fn decomp_class(server_jar_path: &Path, class: &str) -> anyhow::Result<minijvm::Class> {
        let _span = tracing::trace_span!("Decompiling class", ?server_jar_path, class);
        let server_jar_file = std::fs::File::open(&*server_jar_path)?;
        let zip_file = server_jar_file.read_zip()?;

        let class_path = class.replace(".", "/") + ".class";

        let Some(version_file_entry) = zip_file.by_name(&class_path)
        else { bail!("Could not find the class file at {class_path}") };

        let mut data = Vec::new();
        version_file_entry.reader().read_to_end(&mut data)
            .with_context(|| format!("Reading entry of server.jar at {class_path}"))?;

        let noak_class = noak::reader::Class::new(&data)
            .with_context(|| format!("Reading class at {class_path}"))?;

        macro_rules! pool { ($e:expr) => { noak_class.pool().get($e) }; }
        macro_rules! pool_str { ($e:expr) => { noak_class.pool().get($e).map_err(anyhow::Error::from).and_then(|s| s.content.to_str().ok_or_else(|| anyhow!("Invalid utf8 string in java class"))) }; }
        macro_rules! try_or {
            ($e: expr; orelse $else: expr) => {match $e {
                Ok(val) => val,
                Err(e) => { tracing::warn!(?e); $else },
            }};
        }

        let noak_this_class = pool!(noak_class.this_class())?;
        let noak_super_class = noak_class.super_class().map(|e| pool!(e)).transpose()?;

        let mut out_class = minijvm::Class {
            name: minijvm::IdentPath(pool_str!(noak_this_class.name)?.into()),
            super_class: noak_super_class.map(|c| pool_str!(c.name)).transpose()?.map(String::from).map(minijvm::IdentPath),
            fields: Vec::new(),
            methods: Vec::new(),
        };

        for field in noak_class.fields() {
            let _span = tracing::trace_span!("Reading a method");
            let field = try_or!(field; orelse continue);
            out_class.fields.push(minijvm::Field {
                access_flags: minijvm::AccessFlags::from(field.access_flags()),
                name: minijvm::Ident(pool_str!(field.name())?.into()),
                descriptor: minijvm::TypeDescriptor::parse_complete(pool_str!(field.descriptor())?)?,
            });
        }

        let decomp_code = |code: noak::reader::attributes::Code<'_>| -> anyhow::Result<minijvm::Code> {
            let mut instructions = Vec::<minijvm::Instruction>::new();
            for instruction in code.raw_instructions() {
                let (_, instruction) = try_or!(instruction; orelse continue);

                use noak::reader::attributes::RawInstruction as RI;
                use minijvm::Instruction as MiniInstr;
                use minijvm::ValueKind as VK;
                use minijvm::ConstantValue as CV;
                use minijvm::{ BinOp, UnOp };
                match instruction {
                    RI::AConstNull => todo!(),
                    RI::ALoad { index }   => instructions.push(MiniInstr::Load     { kind: VK::Ref, index: index.into() }),
                    RI::ALoadW { index }  => instructions.push(MiniInstr::Load     { kind: VK::Ref, index: index.into() }),
                    RI::ALoad0            => instructions.push(MiniInstr::Load     { kind: VK::Ref, index: 0 }),
                    RI::ALoad1            => instructions.push(MiniInstr::Load     { kind: VK::Ref, index: 1 }),
                    RI::ALoad2            => instructions.push(MiniInstr::Load     { kind: VK::Ref, index: 2 }),
                    RI::ALoad3            => instructions.push(MiniInstr::Load     { kind: VK::Ref, index: 3 }),
                    RI::AReturn           => instructions.push(MiniInstr::Return   { kind: Some(VK::Ref) }),
                    RI::AStore { index }  => instructions.push(MiniInstr::Store    { kind: VK::Ref, index: index.into() }),
                    RI::AStoreW { index } => instructions.push(MiniInstr::Store    { kind: VK::Ref, index: index.into() }),
                    RI::AStore0           => instructions.push(MiniInstr::Store    { kind: VK::Ref, index: 0 }),
                    RI::AStore1           => instructions.push(MiniInstr::Store    { kind: VK::Ref, index: 1 }),
                    RI::AStore2           => instructions.push(MiniInstr::Store    { kind: VK::Ref, index: 2 }),
                    RI::AStore3           => instructions.push(MiniInstr::Store    { kind: VK::Ref, index: 3 }),
                    RI::AThrow            => instructions.push(MiniInstr::Throw),
                    RI::BIPush { value }  => instructions.push(MiniInstr::Constant { value: CV::Byte(value) }),
                    RI::D2F               => instructions.push(MiniInstr::Convert  { from: VK::Double, to: VK::Float }),
                    RI::D2I               => instructions.push(MiniInstr::Convert  { from: VK::Double, to: VK::Int   }),
                    RI::D2L               => instructions.push(MiniInstr::Convert  { from: VK::Double, to: VK::Long  }),
                    RI::DAdd              => instructions.push(MiniInstr::BinOp    { op: BinOp::Add, value_kind: VK::Double }),
                    RI::DCmpG             => instructions.push(MiniInstr::BinOp    { op: BinOp::GreaterThan, value_kind: VK::Double }),
                    RI::DCmpL             => instructions.push(MiniInstr::BinOp    { op: BinOp::LessThan, value_kind: VK::Double }),
                    RI::DConst0           => instructions.push(MiniInstr::Constant { value: CV::Double(0.) }),
                    RI::DConst1           => instructions.push(MiniInstr::Constant { value: CV::Double(1.) }),
                    RI::DDiv              => instructions.push(MiniInstr::BinOp    { op: BinOp::Div, value_kind: VK::Double }),
                    RI::DLoad { index }   => instructions.push(MiniInstr::Load     { kind: VK::Double, index: index.into() }),
                    RI::DLoadW { index }  => instructions.push(MiniInstr::Load     { kind: VK::Double, index: index.into() }),
                    RI::DLoad0            => instructions.push(MiniInstr::Load     { kind: VK::Double, index: 0 }),
                    RI::DLoad1            => instructions.push(MiniInstr::Load     { kind: VK::Double, index: 1 }),
                    RI::DLoad2            => instructions.push(MiniInstr::Load     { kind: VK::Double, index: 2 }),
                    RI::DLoad3            => instructions.push(MiniInstr::Load     { kind: VK::Double, index: 3 }),
                    RI::DMul              => instructions.push(MiniInstr::BinOp    { op: BinOp::Mul, value_kind: VK::Double }),
                    RI::DNeg              => instructions.push(MiniInstr::UnOp     { op: UnOp::Neg, value_kind: VK::Double }),
                    RI::DRem              => instructions.push(MiniInstr::BinOp    { op: BinOp::Rem, value_kind: VK::Double }),
                    RI::DReturn           => instructions.push(MiniInstr::Return   { kind: Some(VK::Ref) }),
                    RI::DStore { index }  => instructions.push(MiniInstr::Store    { kind: VK::Double, index: index.into() }),
                    RI::DStoreW { index } => instructions.push(MiniInstr::Store    { kind: VK::Double, index: index.into() }),
                    RI::DStore0           => instructions.push(MiniInstr::Store    { kind: VK::Double, index: 0 }),
                    RI::DStore1           => instructions.push(MiniInstr::Store    { kind: VK::Double, index: 1 }),
                    RI::DStore2           => instructions.push(MiniInstr::Store    { kind: VK::Double, index: 2 }),
                    RI::DStore3           => instructions.push(MiniInstr::Store    { kind: VK::Double, index: 3 }),
                    RI::DSub              => instructions.push(MiniInstr::BinOp    { op: BinOp::Sub, value_kind: VK::Double }),
                    RI::Dup               => instructions.push(MiniInstr::Dup      { count: 1, depth: 0 }),
                    RI::DupX1             => instructions.push(MiniInstr::Dup      { count: 1, depth: 1 }),
                    RI::DupX2             => instructions.push(MiniInstr::Dup      { count: 1, depth: 2 }),
                    RI::Dup2              => instructions.push(MiniInstr::Dup      { count: 2, depth: 0 }),
                    RI::Dup2X1            => instructions.push(MiniInstr::Dup      { count: 2, depth: 1 }),
                    RI::Dup2X2            => instructions.push(MiniInstr::Dup      { count: 2, depth: 2 }),
                    RI::F2D               => instructions.push(MiniInstr::Convert  { from: VK::Float, to: VK::Float }),
                    RI::F2I               => instructions.push(MiniInstr::Convert  { from: VK::Float, to: VK::Int   }),
                    RI::F2L               => instructions.push(MiniInstr::Convert  { from: VK::Float, to: VK::Long  }),
                    RI::FAdd              => instructions.push(MiniInstr::BinOp    { op: BinOp::Add, value_kind: VK::Float }),
                    RI::FCmpG             => instructions.push(MiniInstr::BinOp    { op: BinOp::GreaterThan, value_kind: VK::Float }),
                    RI::FCmpL             => instructions.push(MiniInstr::BinOp    { op: BinOp::LessThan, value_kind: VK::Float }),
                    RI::FConst0           => instructions.push(MiniInstr::Constant { value: CV::Float(0.) }),
                    RI::FConst1           => instructions.push(MiniInstr::Constant { value: CV::Float(1.) }),
                    RI::FConst2           => instructions.push(MiniInstr::Constant { value: CV::Float(2.) }),
                    RI::FDiv              => instructions.push(MiniInstr::BinOp    { op: BinOp::Div, value_kind: VK::Float }),
                    RI::FLoad { index }   => instructions.push(MiniInstr::Load     { kind: VK::Float, index: index.into() }),
                    RI::FLoadW { index }  => instructions.push(MiniInstr::Load     { kind: VK::Float, index: index.into() }),
                    RI::FLoad0            => instructions.push(MiniInstr::Load     { kind: VK::Float, index: 0 }),
                    RI::FLoad1            => instructions.push(MiniInstr::Load     { kind: VK::Float, index: 1 }),
                    RI::FLoad2            => instructions.push(MiniInstr::Load     { kind: VK::Float, index: 2 }),
                    RI::FLoad3            => instructions.push(MiniInstr::Load     { kind: VK::Float, index: 3 }),
                    RI::FMul              => instructions.push(MiniInstr::BinOp    { op: BinOp::Mul, value_kind: VK::Float }),
                    RI::FNeg              => instructions.push(MiniInstr::UnOp     { op: UnOp::Neg, value_kind: VK::Float }),
                    RI::FRem              => instructions.push(MiniInstr::BinOp    { op: BinOp::Rem, value_kind: VK::Float }),
                    RI::FReturn           => instructions.push(MiniInstr::Return   { kind: Some(VK::Ref) }),
                    RI::FStore { index }  => instructions.push(MiniInstr::Store    { kind: VK::Float, index: index.into() }),
                    RI::FStoreW { index } => instructions.push(MiniInstr::Store    { kind: VK::Float, index: index.into() }),
                    RI::FStore0           => instructions.push(MiniInstr::Store    { kind: VK::Float, index: 0 }),
                    RI::FStore1           => instructions.push(MiniInstr::Store    { kind: VK::Float, index: 1 }),
                    RI::FStore2           => instructions.push(MiniInstr::Store    { kind: VK::Float, index: 2 }),
                    RI::FStore3           => instructions.push(MiniInstr::Store    { kind: VK::Float, index: 3 }),
                    RI::FSub              => instructions.push(MiniInstr::BinOp    { op: BinOp::Sub, value_kind: VK::Float }),
                    RI::GetField { index } => todo!(),
                    RI::GetStatic { index } => todo!(),
                    RI::Goto { offset } => todo!(),
                    RI::GotoW { offset } => todo!(),
                    RI::I2B => todo!(),
                    RI::I2C => todo!(),
                    RI::I2D => todo!(),
                    RI::I2F => todo!(),
                    RI::I2L => todo!(),
                    RI::I2S => todo!(),
                    RI::IAdd => todo!(),
                    RI::IALoad => todo!(),
                    RI::IAnd => todo!(),
                    RI::IAStore => todo!(),
                    RI::IConstM1 => todo!(),
                    RI::IConst0 => todo!(),
                    RI::IConst1 => todo!(),
                    RI::IConst2 => todo!(),
                    RI::IConst3 => todo!(),
                    RI::IConst4 => todo!(),
                    RI::IConst5 => todo!(),
                    RI::IDiv => todo!(),
                    RI::IfACmpEq { offset } => todo!(),
                    RI::IfACmpNe { offset } => todo!(),
                    RI::IfICmpEq { offset } => todo!(),
                    RI::IfICmpNe { offset } => todo!(),
                    RI::IfICmpLt { offset } => todo!(),
                    RI::IfICmpGe { offset } => todo!(),
                    RI::IfICmpGt { offset } => todo!(),
                    RI::IfICmpLe { offset } => todo!(),
                    RI::IfEq { offset } => todo!(),
                    RI::IfNe { offset } => todo!(),
                    RI::IfLt { offset } => todo!(),
                    RI::IfGe { offset } => todo!(),
                    RI::IfGt { offset } => todo!(),
                    RI::IfLe { offset } => todo!(),
                    RI::IfNonNull { offset } => todo!(),
                    RI::IfNull { offset } => todo!(),
                    RI::IInc { index, value } => todo!(),
                    RI::IIncW { index, value } => todo!(),
                    RI::ILoad { index } => todo!(),
                    RI::ILoadW { index } => todo!(),
                    RI::ILoad0 => todo!(),
                    RI::ILoad1 => todo!(),
                    RI::ILoad2 => todo!(),
                    RI::ILoad3 => todo!(),
                    RI::IMul => todo!(),
                    RI::INeg => todo!(),
                    RI::InstanceOf { index } => todo!(),
                    RI::InvokeDynamic { index } => todo!(),
                    RI::InvokeInterface { index, count } => todo!(),
                    RI::InvokeSpecial { index } => todo!(),
                    RI::InvokeStatic { index } => todo!(),
                    RI::InvokeVirtual { index } => todo!(),
                    RI::IOr => todo!(),
                    RI::IRem => todo!(),
                    RI::IReturn => todo!(),
                    RI::IShL => todo!(),
                    RI::IShR => todo!(),
                    RI::IStore { index } => todo!(),
                    RI::IStoreW { index } => todo!(),
                    RI::IStore0 => todo!(),
                    RI::IStore1 => todo!(),
                    RI::IStore2 => todo!(),
                    RI::IStore3 => todo!(),
                    RI::ISub => todo!(),
                    RI::IUShR => todo!(),
                    RI::IXor => todo!(),
                    RI::JSr { offset } => todo!(),
                    RI::JSrW { offset } => todo!(),
                    RI::L2D => todo!(),
                    RI::L2F => todo!(),
                    RI::L2I => todo!(),
                    RI::LAdd => todo!(),
                    RI::LALoad => todo!(),
                    RI::LAnd => todo!(),
                    RI::LAStore => todo!(),
                    RI::LCmp => todo!(),
                    RI::LConst0 => todo!(),
                    RI::LConst1 => todo!(),
                    RI::LdC { index } => todo!(),
                    RI::LdCW { index } => todo!(),
                    RI::LdC2W { index } => todo!(),
                    RI::LDiv => todo!(),
                    RI::LLoad { index } => todo!(),
                    RI::LLoadW { index } => todo!(),
                    RI::LLoad0 => todo!(),
                    RI::LLoad1 => todo!(),
                    RI::LLoad2 => todo!(),
                    RI::LLoad3 => todo!(),
                    RI::LMul => todo!(),
                    RI::LNeg => todo!(),
                    RI::LookupSwitch(lookup_switch) => todo!(),
                    RI::LOr => todo!(),
                    RI::LRem => todo!(),
                    RI::LReturn => todo!(),
                    RI::LShL => todo!(),
                    RI::LShR => todo!(),
                    RI::LStore { index } => todo!(),
                    RI::LStoreW { index } => todo!(),
                    RI::LStore0 => todo!(),
                    RI::LStore1 => todo!(),
                    RI::LStore2 => todo!(),
                    RI::LStore3 => todo!(),
                    RI::LSub => todo!(),
                    RI::LUShR => todo!(),
                    RI::LXor => todo!(),
                    RI::MonitorEnter => todo!(),
                    RI::MonitorExit => todo!(),
                    RI::MultiANewArray { index, dimensions } => todo!(),
                    RI::New { index } => todo!(),
                    RI::NewArray { atype } => todo!(),
                    RI::Nop => todo!(),
                    RI::Pop => todo!(),
                    RI::Pop2 => todo!(),
                    RI::PutField { index } => todo!(),
                    RI::PutStatic { index } => {
                        todo!();
                    },
                    RI::Ret { index } => todo!(),
                    RI::RetW { index } => todo!(),
                    RI::Return => instructions.push(MiniInstr::Return { kind: None }),
                    RI::SALoad => todo!(),
                    RI::SAStore => todo!(),
                    RI::SIPush { value } => todo!(),
                    RI::Swap => todo!(),
                    RI::TableSwitch(table_switch) => todo!(),

                    _ => {
                        warn!(?instruction, "Unknown instruction");
                    },
                }
            }
            Ok(minijvm::Code { instructions })
        };

        for method in noak_class.methods() {
            tracing::trace_span!("Reading a method");
            let method = try_or!(method; orelse continue);
            let mut found_code = None;

            for attr in method.attributes() {
                tracing::trace_span!("Reading a method's attr");
                let attr = try_or!(attr; orelse continue);
                let Ok(noak::reader::AttributeContent::Code(code)) = attr.read_content(noak_class.pool())
                else { continue };
                found_code = Some(decomp_code(code)?);
            }

            let name = minijvm::Ident(pool_str!(method.name())?.into());
            let code = found_code.ok_or_else(|| anyhow!("Could not find code attribute for method {name}"))?;
            out_class.methods.push(minijvm::Method {
                access_flags: minijvm::AccessFlags::from(method.access_flags()),
                name,
                descriptor: minijvm::MethodDescriptor::parse_complete(pool_str!(method.descriptor())?)?,
                code,
            });
        }

        Ok(out_class)
    }
}

impl super::ExtractorKind for DecompClassExtractor {
    type Output = minijvm::Class;

    fn config(&self) -> super::ExtractorConfig {
        super::ExtractorConfig { store_output_in_cache: false }
    }
    
    fn name(&self) -> &'static str {
        "mapped_class_extractor"
    }

    async fn extract(self, manager: &mut super::ExtractionManager<'_>) -> anyhow::Result<Self::Output> {
        let server_jar_path = manager.extract(super::server_jar::ServerJarExtractor).await?;
        crate::spawn_cpu_bound(move || Self::decomp_class(&server_jar_path, &self.class)).await?
    }
}
