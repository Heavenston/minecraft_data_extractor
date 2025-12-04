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

        let bootstrap_methods = {
            let mut methods = Vec::new();
            for attr in noak_class.attributes() {
                let attr = try_or!(attr; orelse continue);
                let Ok(noak::reader::AttributeContent::BootstrapMethods(bsm)) = attr.read_content(noak_class.pool())
                else { continue };
                for method in bsm.methods() {
                    let method = try_or!(method; orelse continue);
                    methods.push(method);
                }
            }
            methods
        };

        let decomp_code = |code: noak::reader::attributes::Code<'_>| -> anyhow::Result<minijvm::Code> {
            use minijvm::ConstantValue as CV;
            let str_from_mstr = |s: &noak::mutf8::MStr| -> anyhow::Result<String> {
                s.to_str().ok_or_else(|| anyhow!("Invalid utf8 string in java class")).map(String::from)
            };

            let method_ref_from_parts = |class_name: &noak::mutf8::MStr, name: &noak::mutf8::MStr, descriptor: &noak::mutf8::MStr| -> anyhow::Result<minijvm::MethodRef> {
                Ok(minijvm::MethodRef {
                    class: minijvm::ClassRef { name: minijvm::IdentPath(str_from_mstr(class_name)?) },
                    name: minijvm::Ident(str_from_mstr(name)?),
                    descriptor: minijvm::MethodDescriptor::parse_complete(&str_from_mstr(descriptor)?)?,
                })
            };

            let method_ref_from_index = |index: noak::reader::cpool::Index<noak::reader::cpool::MethodRef<'_>>| -> anyhow::Result<minijvm::MethodRef> {
                let method_ref = noak_class.pool().retrieve(index)?;
                method_ref_from_parts(method_ref.class.name, method_ref.name_and_type.name, method_ref.name_and_type.descriptor)
            };

            let interface_method_ref_from_index = |index: noak::reader::cpool::Index<noak::reader::cpool::InterfaceMethodRef<'_>>| -> anyhow::Result<minijvm::MethodRef> {
                let method_ref = noak_class.pool().retrieve(index)?;
                method_ref_from_parts(method_ref.class.name, method_ref.name_and_type.name, method_ref.name_and_type.descriptor)
            };

            let field_ref_from_index = |index: noak::reader::cpool::Index<noak::reader::cpool::FieldRef<'_>>| -> anyhow::Result<minijvm::FieldRef> {
                let field_ref = noak_class.pool().retrieve(index)?;
                Ok(minijvm::FieldRef {
                    class: minijvm::ClassRef { name: minijvm::IdentPath(str_from_mstr(field_ref.class.name)?) },
                    name: minijvm::Ident(str_from_mstr(field_ref.name_and_type.name)?),
                    descriptor: minijvm::TypeDescriptor::parse_complete(&str_from_mstr(field_ref.name_and_type.descriptor)?)?,
                })
            };

            let method_ref_from_item = |item: &noak::reader::cpool::Item<'_>| -> anyhow::Result<Option<minijvm::MethodRef>> {
                Ok(match item {
                    noak::reader::cpool::Item::MethodRef(mr) => {
                        let class = noak_class.pool().retrieve(mr.class)?;
                        let name_and_type = noak_class.pool().retrieve(mr.name_and_type)?;
                        Some(method_ref_from_parts(class.name, name_and_type.name, name_and_type.descriptor)?)
                    },
                    noak::reader::cpool::Item::InterfaceMethodRef(mr) => {
                        let class = noak_class.pool().retrieve(mr.class)?;
                        let name_and_type = noak_class.pool().retrieve(mr.name_and_type)?;
                        Some(method_ref_from_parts(class.name, name_and_type.name, name_and_type.descriptor)?)
                    },
                    _ => None,
                })
            };

            let method_ref_from_item_index = |index: noak::reader::cpool::Index<noak::reader::cpool::Item<'_>>| -> anyhow::Result<Option<minijvm::MethodRef>> {
                method_ref_from_item(pool!(index)?)
            };

            let constant_value_from_item_index = |index: noak::reader::cpool::Index<noak::reader::cpool::Item<'_>>| -> anyhow::Result<Option<CV>> {
                Ok(match pool!(index)? {
                    noak::reader::cpool::Item::Integer(i) => Some(CV::Int(i.value)),
                    noak::reader::cpool::Item::Float(f) => Some(CV::Float(f.value)),
                    noak::reader::cpool::Item::Long(l) => Some(CV::Long(l.value)),
                    noak::reader::cpool::Item::Double(d) => Some(CV::Double(d.value)),
                    noak::reader::cpool::Item::String(s) => Some(CV::String(str_from_mstr(noak_class.pool().retrieve(s.string)?)?)),
                    _ => None,
                })
            };

            let constant_from_item_index = |index: noak::reader::cpool::Index<noak::reader::cpool::Item<'_>>| -> anyhow::Result<Option<minijvm::Constant>> {
                Ok(match pool!(index)? {
                    noak::reader::cpool::Item::Integer(i) => Some(minijvm::Constant::Int(i.value)),
                    noak::reader::cpool::Item::Float(f) => Some(minijvm::Constant::Float(f.value)),
                    noak::reader::cpool::Item::Long(l) => Some(minijvm::Constant::Long(l.value)),
                    noak::reader::cpool::Item::Double(d) => Some(minijvm::Constant::Double(d.value)),
                    noak::reader::cpool::Item::String(s) => Some(minijvm::Constant::String(str_from_mstr(noak_class.pool().retrieve(s.string)?)?)),
                    noak::reader::cpool::Item::Class(c) => Some(minijvm::Constant::Class(minijvm::ClassRef {
                        name: minijvm::IdentPath(str_from_mstr(noak_class.pool().retrieve(c.name)?)?),
                    })),
                    noak::reader::cpool::Item::MethodType(t) => Some(minijvm::Constant::MethodType(minijvm::MethodDescriptor::parse_complete(&str_from_mstr(noak_class.pool().retrieve(t.descriptor)?)?)?)),
                    noak::reader::cpool::Item::MethodHandle(h) => {
                        let referenced = noak_class.pool().get(h.reference)?;
                        let Some(method) = method_ref_from_item(referenced)? else { return Ok(None) };
                        Some(minijvm::Constant::MethodHandle(method))
                    },
                    _ => None,
                })
            };

            let mut instructions = Vec::<minijvm::Instruction>::new();
            for instruction in code.raw_instructions() {
                let (_, instruction) = try_or!(instruction; orelse continue);

                use noak::reader::attributes::RawInstruction as RI;
                use minijvm::Instruction as MiniInstr;
                use minijvm::ValueKind as VK;
                use minijvm::{ BinOp, UnOp };
                match instruction {
                    RI::AConstNull => instructions.push(MiniInstr::Constant { value: CV::Null }),
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
                    RI::DReturn           => instructions.push(MiniInstr::Return   { kind: Some(VK::Double) }),
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
                    RI::F2D               => instructions.push(MiniInstr::Convert  { from: VK::Float, to: VK::Double }),
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
                    RI::FReturn           => instructions.push(MiniInstr::Return   { kind: Some(VK::Float) }),
                    RI::FStore { index }  => instructions.push(MiniInstr::Store    { kind: VK::Float, index: index.into() }),
                    RI::FStoreW { index } => instructions.push(MiniInstr::Store    { kind: VK::Float, index: index.into() }),
                    RI::FStore0           => instructions.push(MiniInstr::Store    { kind: VK::Float, index: 0 }),
                    RI::FStore1           => instructions.push(MiniInstr::Store    { kind: VK::Float, index: 1 }),
                    RI::FStore2           => instructions.push(MiniInstr::Store    { kind: VK::Float, index: 2 }),
                    RI::FStore3           => instructions.push(MiniInstr::Store    { kind: VK::Float, index: 3 }),
                    RI::FSub              => instructions.push(MiniInstr::BinOp    { op: BinOp::Sub, value_kind: VK::Float }),
                    RI::GetField { index } => {
                        // Field access is not modeled in the current minijvm instruction set.
                        let _ = field_ref_from_index(index);
                        instructions.push(MiniInstr::Unknown(format!("getfield #{}", index.as_u16())));
                    },
                    RI::GetStatic { index } => {
                        // Field access is not modeled in the current minijvm instruction set.
                        let _ = field_ref_from_index(index);
                        instructions.push(MiniInstr::Unknown(format!("getstatic #{}", index.as_u16())));
                    },
                    RI::Goto { offset } => instructions.push(MiniInstr::Goto { offset }),
                    RI::GotoW { offset } => {
                        if let Ok(offset) = i16::try_from(offset) {
                            instructions.push(MiniInstr::Goto { offset });
                        } else {
                            // Offsets wider than i16 cannot be represented by minijvm::Goto.
                            instructions.push(MiniInstr::Unknown(format!("goto_w offset {offset} does not fit i16")));
                        }
                    },
                    RI::I2B => instructions.push(MiniInstr::Convert  { from: VK::Int, to: VK::Byte }),
                    RI::I2C => instructions.push(MiniInstr::Convert  { from: VK::Int, to: VK::Char }),
                    RI::I2D => instructions.push(MiniInstr::Convert  { from: VK::Int, to: VK::Double }),
                    RI::I2F => instructions.push(MiniInstr::Convert  { from: VK::Int, to: VK::Float }),
                    RI::I2L => instructions.push(MiniInstr::Convert  { from: VK::Int, to: VK::Long }),
                    RI::I2S => instructions.push(MiniInstr::Convert  { from: VK::Int, to: VK::Short }),
                    RI::IAdd => instructions.push(MiniInstr::BinOp    { op: BinOp::Add, value_kind: VK::Int }),
                    RI::IALoad => {
                        // Array operations are not representable with the current minijvm instructions.
                        instructions.push(MiniInstr::Unknown("iaload".to_string()));
                    },
                    RI::IAnd => {
                        // Bitwise operations are not represented in the current instruction set.
                        instructions.push(MiniInstr::Unknown("iand".to_string()));
                    },
                    RI::IAStore => {
                        // Array operations are not representable with the current minijvm instructions.
                        instructions.push(MiniInstr::Unknown("iastore".to_string()));
                    },
                    RI::IConstM1 => instructions.push(MiniInstr::Constant { value: CV::Int(-1) }),
                    RI::IConst0 => instructions.push(MiniInstr::Constant { value: CV::Int(0) }),
                    RI::IConst1 => instructions.push(MiniInstr::Constant { value: CV::Int(1) }),
                    RI::IConst2 => instructions.push(MiniInstr::Constant { value: CV::Int(2) }),
                    RI::IConst3 => instructions.push(MiniInstr::Constant { value: CV::Int(3) }),
                    RI::IConst4 => instructions.push(MiniInstr::Constant { value: CV::Int(4) }),
                    RI::IConst5 => instructions.push(MiniInstr::Constant { value: CV::Int(5) }),
                    RI::IDiv => instructions.push(MiniInstr::BinOp    { op: BinOp::Div, value_kind: VK::Int }),
                    RI::IfACmpEq { offset } => {
                        // Reference equality cannot be encoded with the available minijvm comparisons.
                        instructions.push(MiniInstr::Unknown(format!("if_acmpeq {offset}")));
                    },
                    RI::IfACmpNe { offset } => {
                        // Reference equality cannot be encoded with the available minijvm comparisons.
                        instructions.push(MiniInstr::Unknown(format!("if_acmpne {offset}")));
                    },
                    RI::IfICmpEq { offset } => {
                        // Equality comparison is not supported by minijvm::BinOp.
                        instructions.push(MiniInstr::Unknown(format!("if_icmpeq {offset}")));
                    },
                    RI::IfICmpNe { offset } => {
                        // Equality comparison is not supported by minijvm::BinOp.
                        instructions.push(MiniInstr::Unknown(format!("if_icmpne {offset}")));
                    },
                    RI::IfICmpLt { offset } => {
                        instructions.push(MiniInstr::BinOp { op: BinOp::LessThan, value_kind: VK::Int });
                        instructions.push(MiniInstr::BranchIf { offset });
                    },
                    RI::IfICmpGe { offset } => {
                        // Greater-or-equal comparison cannot be represented without equality support.
                        instructions.push(MiniInstr::Unknown(format!("if_icmpge {offset}")));
                    },
                    RI::IfICmpGt { offset } => {
                        instructions.push(MiniInstr::BinOp { op: BinOp::GreaterThan, value_kind: VK::Int });
                        instructions.push(MiniInstr::BranchIf { offset });
                    },
                    RI::IfICmpLe { offset } => {
                        // Less-or-equal comparison cannot be represented without equality support.
                        instructions.push(MiniInstr::Unknown(format!("if_icmple {offset}")));
                    },
                    RI::IfEq { offset } => {
                        // Zero-equality check cannot be encoded with the existing minijvm operators.
                        instructions.push(MiniInstr::Unknown(format!("ifeq {offset}")));
                    },
                    RI::IfNe { offset } => {
                        // Zero-inequality check cannot be encoded with the existing minijvm operators.
                        instructions.push(MiniInstr::Unknown(format!("ifne {offset}")));
                    },
                    RI::IfLt { offset } => {
                        // Single-value comparisons against zero are not representable with the current instruction set.
                        instructions.push(MiniInstr::Unknown(format!("iflt {offset}")));
                    },
                    RI::IfGe { offset } => {
                        // Single-value comparisons against zero are not representable with the current instruction set.
                        instructions.push(MiniInstr::Unknown(format!("ifge {offset}")));
                    },
                    RI::IfGt { offset } => {
                        // Single-value comparisons against zero are not representable with the current instruction set.
                        instructions.push(MiniInstr::Unknown(format!("ifgt {offset}")));
                    },
                    RI::IfLe { offset } => {
                        // Single-value comparisons against zero are not representable with the current instruction set.
                        instructions.push(MiniInstr::Unknown(format!("ifle {offset}")));
                    },
                    RI::IfNonNull { offset } => {
                        // Reference nullability checks are not representable in the current instruction set.
                        instructions.push(MiniInstr::Unknown(format!("ifnonnull {offset}")));
                    },
                    RI::IfNull { offset } => {
                        // Reference nullability checks are not representable in the current instruction set.
                        instructions.push(MiniInstr::Unknown(format!("ifnull {offset}")));
                    },
                    RI::IInc { index, value } => {
                        let index: u16 = index.into();
                        instructions.push(MiniInstr::Load   { kind: VK::Int, index });
                        instructions.push(MiniInstr::Constant { value: CV::Int(i32::from(value)) });
                        instructions.push(MiniInstr::BinOp  { op: BinOp::Add, value_kind: VK::Int });
                        instructions.push(MiniInstr::Store  { kind: VK::Int, index });
                    },
                    RI::IIncW { index, value } => {
                        instructions.push(MiniInstr::Load   { kind: VK::Int, index });
                        instructions.push(MiniInstr::Constant { value: CV::Int(i32::from(value)) });
                        instructions.push(MiniInstr::BinOp  { op: BinOp::Add, value_kind: VK::Int });
                        instructions.push(MiniInstr::Store  { kind: VK::Int, index });
                    },
                    RI::ILoad { index } => instructions.push(MiniInstr::Load     { kind: VK::Int, index: index.into() }),
                    RI::ILoadW { index } => instructions.push(MiniInstr::Load     { kind: VK::Int, index: index.into() }),
                    RI::ILoad0 => instructions.push(MiniInstr::Load     { kind: VK::Int, index: 0 }),
                    RI::ILoad1 => instructions.push(MiniInstr::Load     { kind: VK::Int, index: 1 }),
                    RI::ILoad2 => instructions.push(MiniInstr::Load     { kind: VK::Int, index: 2 }),
                    RI::ILoad3 => instructions.push(MiniInstr::Load     { kind: VK::Int, index: 3 }),
                    RI::IMul => instructions.push(MiniInstr::BinOp     { op: BinOp::Mul, value_kind: VK::Int }),
                    RI::INeg => instructions.push(MiniInstr::UnOp      { op: UnOp::Neg, value_kind: VK::Int }),
                    RI::InstanceOf { index } => {
                        // Type checks are not represented in the current instruction set.
                        instructions.push(MiniInstr::Unknown(format!("instanceof #{}", index.as_u16())));
                    },
                    RI::InvokeDynamic { index } => {
                        let Ok(invoke_dyn) = noak_class.pool().retrieve(index) else {
                            instructions.push(MiniInstr::Unknown(format!("invokedynamic #{}", index.as_u16())));
                            continue;
                        };
                        let Some(bootstrap) = bootstrap_methods.get(invoke_dyn.bootstrap_method_attr as usize) else {
                            // Missing bootstrap entry; cannot map invokedynamic.
                            instructions.push(MiniInstr::Unknown(format!("invokedynamic missing bootstrap #{}", index.as_u16())));
                            continue;
                        };
                        let Ok(handle) = noak_class.pool().retrieve(bootstrap.method_ref()) else {
                            instructions.push(MiniInstr::Unknown(format!("invokedynamic invalid bootstrap handle #{}", index.as_u16())));
                            continue;
                        };
                        let Some(bootstrap_ref) = try_or!(method_ref_from_item(&handle.reference); orelse {
                            // Method handle is not a method reference; unsupported.
                            instructions.push(MiniInstr::Unknown(format!("invokedynamic unsupported bootstrap kind #{}", index.as_u16())));
                            continue;
                        }) else {
                            instructions.push(MiniInstr::Unknown(format!("invokedynamic unsupported bootstrap kind #{}", index.as_u16())));
                            continue;
                        };

                        let mut static_args = Vec::new();
                        let mut unsupported = false;
                        for arg in bootstrap.arguments() {
                            let arg = try_or!(arg; orelse { unsupported = true; break; });
                            match try_or!(constant_from_item_index(arg); orelse { unsupported = true; break; }) {
                                Some(constant) => static_args.push(constant),
                                None => { unsupported = true; break; }
                            }
                        }
                        if unsupported {
                            // Some bootstrap static argument is not representable.
                            instructions.push(MiniInstr::Unknown(format!("invokedynamic unsupported bootstrap args #{}", index.as_u16())));
                            continue;
                        }

                        let name = match str_from_mstr(invoke_dyn.name_and_type.name) {
                            Ok(name) => minijvm::Ident(name),
                            Err(e) => {
                                warn!(?e, "Failed to read invokedynamic name");
                                continue;
                            }
                        };
                        let descriptor = match minijvm::MethodDescriptor::parse_complete(&match str_from_mstr(invoke_dyn.name_and_type.descriptor) {
                            Ok(d) => d,
                            Err(e) => {
                                warn!(?e, "Failed to read invokedynamic descriptor");
                                continue;
                            }
                        }) {
                            Ok(d) => d,
                            Err(e) => {
                                warn!(?e, "Failed to parse invokedynamic descriptor");
                                continue;
                            }
                        };

                        instructions.push(MiniInstr::InvokeDynamic {
                            call_site: minijvm::DynamicCallSite { bootstrap: bootstrap_ref, static_args },
                            name,
                            descriptor,
                        });
                    },
                    RI::InvokeInterface { index, count: _ } => {
                        let Ok(method) = interface_method_ref_from_index(index) else {
                            instructions.push(MiniInstr::Unknown(format!("invokeinterface #{}", index.as_u16())));
                            continue;
                        };
                        instructions.push(MiniInstr::Invoke { kind: minijvm::InvokeKind::Interface, method });
                    },
                    RI::InvokeSpecial { index } => {
                        match method_ref_from_item_index(index) {
                            Ok(Some(method)) => instructions.push(MiniInstr::Invoke { kind: minijvm::InvokeKind::Special, method }),
                            Ok(None) => instructions.push(MiniInstr::Unknown(format!("invokespecial #{}", index.as_u16()))),
                            Err(e) => {
                                warn!(?e, "Failed to read invokespecial target");
                                instructions.push(MiniInstr::Unknown(format!("invokespecial #{}", index.as_u16())));
                            }
                        }
                    },
                    RI::InvokeStatic { index } => {
                        match method_ref_from_item_index(index) {
                            Ok(Some(method)) => instructions.push(MiniInstr::Invoke { kind: minijvm::InvokeKind::Static, method }),
                            Ok(None) => instructions.push(MiniInstr::Unknown(format!("invokestatic #{}", index.as_u16()))),
                            Err(e) => {
                                warn!(?e, "Failed to read invokestatic target");
                                instructions.push(MiniInstr::Unknown(format!("invokestatic #{}", index.as_u16())));
                            }
                        }
                    },
                    RI::InvokeVirtual { index } => {
                        let Ok(method) = method_ref_from_index(index) else {
                            instructions.push(MiniInstr::Unknown(format!("invokevirtual #{}", index.as_u16())));
                            continue;
                        };
                        instructions.push(MiniInstr::Invoke { kind: minijvm::InvokeKind::Virtual, method });
                    },
                    RI::IOr => {
                        // Bitwise operations are not represented in the current instruction set.
                        instructions.push(MiniInstr::Unknown("ior".to_string()));
                    },
                    RI::IRem => instructions.push(MiniInstr::BinOp    { op: BinOp::Rem, value_kind: VK::Int }),
                    RI::IReturn => instructions.push(MiniInstr::Return { kind: Some(VK::Int) }),
                    RI::IShL => {
                        // Shift operations are not represented in the current instruction set.
                        instructions.push(MiniInstr::Unknown("ishl".to_string()));
                    },
                    RI::IShR => {
                        // Shift operations are not represented in the current instruction set.
                        instructions.push(MiniInstr::Unknown("ishr".to_string()));
                    },
                    RI::IStore { index } => instructions.push(MiniInstr::Store    { kind: VK::Int, index: index.into() }),
                    RI::IStoreW { index } => instructions.push(MiniInstr::Store    { kind: VK::Int, index: index.into() }),
                    RI::IStore0 => instructions.push(MiniInstr::Store    { kind: VK::Int, index: 0 }),
                    RI::IStore1 => instructions.push(MiniInstr::Store    { kind: VK::Int, index: 1 }),
                    RI::IStore2 => instructions.push(MiniInstr::Store    { kind: VK::Int, index: 2 }),
                    RI::IStore3 => instructions.push(MiniInstr::Store    { kind: VK::Int, index: 3 }),
                    RI::ISub => instructions.push(MiniInstr::BinOp    { op: BinOp::Sub, value_kind: VK::Int }),
                    RI::IUShR => {
                        // Shift operations are not represented in the current instruction set.
                        instructions.push(MiniInstr::Unknown("iushr".to_string()));
                    },
                    RI::IXor => {
                        // Bitwise operations are not represented in the current instruction set.
                        instructions.push(MiniInstr::Unknown("ixor".to_string()));
                    },
                    RI::JSr { offset } => {
                        // Subroutine jumps are deprecated and not modeled in minijvm.
                        instructions.push(MiniInstr::Unknown(format!("jsr {offset}")));
                    },
                    RI::JSrW { offset } => {
                        // Subroutine jumps are deprecated and not modeled in minijvm.
                        instructions.push(MiniInstr::Unknown(format!("jsr_w {offset}")));
                    },
                    RI::L2D => instructions.push(MiniInstr::Convert  { from: VK::Long, to: VK::Double }),
                    RI::L2F => instructions.push(MiniInstr::Convert  { from: VK::Long, to: VK::Float }),
                    RI::L2I => instructions.push(MiniInstr::Convert  { from: VK::Long, to: VK::Int }),
                    RI::LAdd => instructions.push(MiniInstr::BinOp    { op: BinOp::Add, value_kind: VK::Long }),
                    RI::LALoad => {
                        // Array operations are not representable with the current minijvm instructions.
                        instructions.push(MiniInstr::Unknown("laload".to_string()));
                    },
                    RI::LAnd => {
                        // Bitwise operations are not represented in the current instruction set.
                        instructions.push(MiniInstr::Unknown("land".to_string()));
                    },
                    RI::LAStore => {
                        // Array operations are not representable with the current minijvm instructions.
                        instructions.push(MiniInstr::Unknown("lastore".to_string()));
                    },
                    RI::LCmp => {
                        // Three-way comparisons are not represented in the current instruction set.
                        instructions.push(MiniInstr::Unknown("lcmp".to_string()));
                    },
                    RI::LConst0 => instructions.push(MiniInstr::Constant { value: CV::Long(0) }),
                    RI::LConst1 => instructions.push(MiniInstr::Constant { value: CV::Long(1) }),
                    RI::LdC { index } => match try_or!(constant_value_from_item_index(index); orelse { continue; }) {
                        Some(value) => instructions.push(MiniInstr::Constant { value }),
                        None => {
                            // Constant type is not representable with minijvm::ConstantValue.
                            instructions.push(MiniInstr::Unknown(format!("ldc #{}", index.as_u16())));
                        }
                    },
                    RI::LdCW { index } => match try_or!(constant_value_from_item_index(index); orelse { continue; }) {
                        Some(value) => instructions.push(MiniInstr::Constant { value }),
                        None => {
                            // Constant type is not representable with minijvm::ConstantValue.
                            instructions.push(MiniInstr::Unknown(format!("ldc_w #{}", index.as_u16())));
                        }
                    },
                    RI::LdC2W { index } => match try_or!(constant_value_from_item_index(index); orelse { continue; }) {
                        Some(value) => instructions.push(MiniInstr::Constant { value }),
                        None => {
                            // Constant type is not representable with minijvm::ConstantValue.
                            instructions.push(MiniInstr::Unknown(format!("ldc2_w #{}", index.as_u16())));
                        }
                    },
                    RI::LDiv => instructions.push(MiniInstr::BinOp    { op: BinOp::Div, value_kind: VK::Long }),
                    RI::LLoad { index } => instructions.push(MiniInstr::Load     { kind: VK::Long, index: index.into() }),
                    RI::LLoadW { index } => instructions.push(MiniInstr::Load     { kind: VK::Long, index: index.into() }),
                    RI::LLoad0 => instructions.push(MiniInstr::Load     { kind: VK::Long, index: 0 }),
                    RI::LLoad1 => instructions.push(MiniInstr::Load     { kind: VK::Long, index: 1 }),
                    RI::LLoad2 => instructions.push(MiniInstr::Load     { kind: VK::Long, index: 2 }),
                    RI::LLoad3 => instructions.push(MiniInstr::Load     { kind: VK::Long, index: 3 }),
                    RI::LMul => instructions.push(MiniInstr::BinOp    { op: BinOp::Mul, value_kind: VK::Long }),
                    RI::LNeg => instructions.push(MiniInstr::UnOp     { op: UnOp::Neg, value_kind: VK::Long }),
                    RI::LookupSwitch(_lookup_switch) => {
                        // Switch tables are not represented in the current instruction set.
                        instructions.push(MiniInstr::Unknown("lookupswitch".to_string()));
                    },
                    RI::LOr => {
                        // Bitwise operations are not represented in the current instruction set.
                        instructions.push(MiniInstr::Unknown("lor".to_string()));
                    },
                    RI::LRem => instructions.push(MiniInstr::BinOp    { op: BinOp::Rem, value_kind: VK::Long }),
                    RI::LReturn => instructions.push(MiniInstr::Return { kind: Some(VK::Long) }),
                    RI::LShL => {
                        // Shift operations are not represented in the current instruction set.
                        instructions.push(MiniInstr::Unknown("lshl".to_string()));
                    },
                    RI::LShR => {
                        // Shift operations are not represented in the current instruction set.
                        instructions.push(MiniInstr::Unknown("lshr".to_string()));
                    },
                    RI::LStore { index } => instructions.push(MiniInstr::Store    { kind: VK::Long, index: index.into() }),
                    RI::LStoreW { index } => instructions.push(MiniInstr::Store    { kind: VK::Long, index: index.into() }),
                    RI::LStore0 => instructions.push(MiniInstr::Store    { kind: VK::Long, index: 0 }),
                    RI::LStore1 => instructions.push(MiniInstr::Store    { kind: VK::Long, index: 1 }),
                    RI::LStore2 => instructions.push(MiniInstr::Store    { kind: VK::Long, index: 2 }),
                    RI::LStore3 => instructions.push(MiniInstr::Store    { kind: VK::Long, index: 3 }),
                    RI::LSub => instructions.push(MiniInstr::BinOp    { op: BinOp::Sub, value_kind: VK::Long }),
                    RI::LUShR => {
                        // Shift operations are not represented in the current instruction set.
                        instructions.push(MiniInstr::Unknown("lushr".to_string()));
                    },
                    RI::LXor => {
                        // Bitwise operations are not represented in the current instruction set.
                        instructions.push(MiniInstr::Unknown("lxor".to_string()));
                    },
                    RI::MonitorEnter => {
                        // Monitor enter/exit are not modeled in minijvm.
                        instructions.push(MiniInstr::Unknown("monitorenter".to_string()));
                    },
                    RI::MonitorExit => {
                        // Monitor enter/exit are not modeled in minijvm.
                        instructions.push(MiniInstr::Unknown("monitorexit".to_string()));
                    },
                    RI::MultiANewArray { index, dimensions } => {
                        // Multi-dimensional array creation is not represented in the current instruction set.
                        instructions.push(MiniInstr::Unknown(format!("multianewarray #{} dims {dimensions}", index.as_u16())));
                    },
                    RI::New { index } => {
                        // Object allocation is not represented in the current instruction set.
                        instructions.push(MiniInstr::Unknown(format!("new #{}", index.as_u16())));
                    },
                    RI::NewArray { atype } => {
                        // Array creation is not represented in the current instruction set.
                        instructions.push(MiniInstr::Unknown(format!("newarray {:?}", atype)));
                    },
                    RI::Nop => instructions.push(MiniInstr::Noop),
                    RI::Pop => {
                        // Stack manipulation is not represented directly in minijvm.
                        instructions.push(MiniInstr::Unknown("pop".to_string()));
                    },
                    RI::Pop2 => {
                        // Stack manipulation is not represented directly in minijvm.
                        instructions.push(MiniInstr::Unknown("pop2".to_string()));
                    },
                    RI::PutField { index } => {
                        // Field writes are not modeled in the current minijvm instruction set.
                        let _ = field_ref_from_index(index);
                        instructions.push(MiniInstr::Unknown(format!("putfield #{}", index.as_u16())));
                    },
                    RI::PutStatic { index } => {
                        // Field writes are not modeled in the current minijvm instruction set.
                        let _ = field_ref_from_index(index);
                        instructions.push(MiniInstr::Unknown(format!("putstatic #{}", index.as_u16())));
                    },
                    RI::Ret { index } => {
                        // RET is not represented in the current instruction set.
                        instructions.push(MiniInstr::Unknown(format!("ret {index}")));
                    },
                    RI::RetW { index } => {
                        // RET is not represented in the current instruction set.
                        instructions.push(MiniInstr::Unknown(format!("ret_w {index}")));
                    },
                    RI::Return => instructions.push(MiniInstr::Return { kind: None }),
                    RI::SALoad => {
                        // Array operations are not representable with the current minijvm instructions.
                        instructions.push(MiniInstr::Unknown("saload".to_string()));
                    },
                    RI::SAStore => {
                        // Array operations are not representable with the current minijvm instructions.
                        instructions.push(MiniInstr::Unknown("sastore".to_string()));
                    },
                    RI::SIPush { value } => instructions.push(MiniInstr::Constant { value: CV::Short(value) }),
                    RI::Swap => {
                        // Stack manipulation is not represented directly in minijvm.
                        instructions.push(MiniInstr::Unknown("swap".to_string()));
                    },
                    RI::TableSwitch(_table_switch) => {
                        // Switch tables are not represented in the current instruction set.
                        instructions.push(MiniInstr::Unknown("tableswitch".to_string()));
                    },

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
