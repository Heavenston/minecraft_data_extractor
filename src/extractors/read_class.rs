use crate::minijvm;

use std::{ io::Read, path::Path };

use anyhow::{ anyhow, bail, Context };
use itertools::Itertools;
use rc_zip_sync::ReadZip as _;
use tracing::warn;

#[derive(Debug, bincode::Encode)]
pub struct ReadClassExtractor {
    /// The (obfuscated) class name
    /// directly used to find its path
    pub class: String,
}

impl ReadClassExtractor {
    fn read_class(server_jar_path: &Path, class: &str) -> anyhow::Result<minijvm::Class> {
        use noak::reader::cpool as cpool;
        use noak::reader::attributes::RawInstruction as RI;
        use minijvm::{ GotoCondition, IfOperand, IfCmp, BinOp, UnOp, Instruction as MiniInstr, ValueKind as VK, ConstantValue as CV };

        let _span = tracing::trace_span!("Reading class", ?server_jar_path, class);
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

        let make_class_ref = |class: &cpool::Class<'_>| -> anyhow::Result<minijvm::ClassRef> {
            Ok(minijvm::ClassRef { name: minijvm::IdentPath(pool_str!(class.name)?.into()) })
        };

        let make_method_ref = |method_ref: &cpool::MethodRef<'_>| -> anyhow::Result<minijvm::MethodRef> {
            let n_and_t = pool!(method_ref.name_and_type)?;
            Ok(minijvm::MethodRef {
                class: make_class_ref(pool!(method_ref.class)?)?,
                name: minijvm::Ident(pool_str!(n_and_t.name)?.into()),
                descriptor: minijvm::MethodDescriptor::parse_complete(&pool_str!(n_and_t.descriptor)?)?,
            })
        };

        let make_interface_method_ref = |interface_method_ref: &cpool::InterfaceMethodRef<'_>| -> anyhow::Result<minijvm::MethodRef> {
            let n_and_t = pool!(interface_method_ref.name_and_type)?;
            Ok(minijvm::MethodRef {
                class: make_class_ref(pool!(interface_method_ref.class)?)?,
                name: minijvm::Ident(pool_str!(n_and_t.name)?.into()),
                descriptor: minijvm::MethodDescriptor::parse_complete(&pool_str!(n_and_t.descriptor)?)?,
            })
        };

        let make_field_ref = |field_ref: &cpool::FieldRef<'_>| -> anyhow::Result<minijvm::FieldRef> {
            let n_and_t = pool!(field_ref.name_and_type)?;
            Ok(minijvm::FieldRef {
                class: make_class_ref(pool!(field_ref.class)?)?,
                name: minijvm::Ident(pool_str!(n_and_t.name)?.into()),
                descriptor: minijvm::TypeDescriptor::parse_complete(pool_str!(n_and_t.descriptor)?)?,
            })
        };

        let make_method_ref_from_item = |item: &cpool::Item<'_>| -> anyhow::Result<minijvm::MethodRef> {
            Ok(match item {
                cpool::Item::MethodRef(mr) => make_method_ref(mr)?,
                cpool::Item::InterfaceMethodRef(imr) => make_interface_method_ref(imr)?,
                _ => {
                    warn!(?item, "Invalid item for method ref");
                    bail!("Invalid item for method ref: {item:?}");
                },
            })
        };

        let constant_from_item = |item: &cpool::Item<'_>| -> anyhow::Result<minijvm::Constant> {
            Ok(match item {
                cpool::Item::Integer(i) => minijvm::Constant::Int(i.value),
                cpool::Item::Float(f) => minijvm::Constant::Float(f.value),
                cpool::Item::Long(l) => minijvm::Constant::Long(l.value),
                cpool::Item::Double(d) => minijvm::Constant::Double(d.value),
                cpool::Item::String(s) => minijvm::Constant::String(pool_str!(s.string)?.into()),
                cpool::Item::Class(c) => minijvm::Constant::Class(make_class_ref(c)?),
                cpool::Item::MethodType(t) => minijvm::Constant::MethodType(minijvm::MethodDescriptor::parse_complete(&pool_str!(t.descriptor)?)?),
                cpool::Item::MethodHandle(h) => minijvm::Constant::MethodHandle(make_method_ref_from_item(pool!(h.reference)?)?),
                item => {
                    warn!(?item, "Invalid item for constant");
                    bail!("Invalid item for constant: {item:?}");
                },
            })
        };


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

        // Convert the noak methods into minijvm's
        let bootstrap_methods = bootstrap_methods.into_iter()
            .map(|bootstrap_method| -> anyhow::Result<minijvm::DynamicCallSite> {
                let mut static_args = Vec::new();
                for arg in bootstrap_method.arguments() {
                    static_args.push(constant_from_item(pool!(arg?)?)?);
                }

                let handle = pool!(bootstrap_method.method_ref())?;
                let bootstrap = make_method_ref_from_item(pool!(handle.reference)?)?;

                Ok(minijvm::DynamicCallSite {
                    bootstrap,
                    method_kind: handle.kind.into(),
                    static_args,
                })
            })
            .try_collect::<_, Vec<_>, _>()?
        ;
        
        let convert_code = |code: noak::reader::attributes::Code<'_>| -> anyhow::Result<minijvm::Code> {
            let mut instructions = Vec::<minijvm::Instruction>::new();
            for instruction in code.raw_instructions() {
                let (_, instruction) = try_or!(instruction; orelse continue);

                match instruction {
                    RI::AConstNull              => instructions.push(MiniInstr::Constant { value: CV::Null }),
                    RI::ALoad { index }         => instructions.push(MiniInstr::Load     { kind: VK::Ref, index: index.into() }),
                    RI::ALoadW { index }        => instructions.push(MiniInstr::Load     { kind: VK::Ref, index: index.into() }),
                    RI::ALoad0                  => instructions.push(MiniInstr::Load     { kind: VK::Ref, index: 0 }),
                    RI::ALoad1                  => instructions.push(MiniInstr::Load     { kind: VK::Ref, index: 1 }),
                    RI::ALoad2                  => instructions.push(MiniInstr::Load     { kind: VK::Ref, index: 2 }),
                    RI::ALoad3                  => instructions.push(MiniInstr::Load     { kind: VK::Ref, index: 3 }),
                    RI::AReturn                 => instructions.push(MiniInstr::Return   { kind: Some(VK::Ref) }),
                    RI::AStore { index }        => instructions.push(MiniInstr::Store    { kind: VK::Ref, index: index.into() }),
                    RI::AStoreW { index }       => instructions.push(MiniInstr::Store    { kind: VK::Ref, index: index.into() }),
                    RI::AStore0                 => instructions.push(MiniInstr::Store    { kind: VK::Ref, index: 0 }),
                    RI::AStore1                 => instructions.push(MiniInstr::Store    { kind: VK::Ref, index: 1 }),
                    RI::AStore2                 => instructions.push(MiniInstr::Store    { kind: VK::Ref, index: 2 }),
                    RI::AStore3                 => instructions.push(MiniInstr::Store    { kind: VK::Ref, index: 3 }),
                    RI::AThrow                  => instructions.push(MiniInstr::Throw),
                    RI::BIPush { value }        => instructions.push(MiniInstr::Constant { value: CV::Byte(value) }),
                    RI::D2F                     => instructions.push(MiniInstr::Convert  { from: VK::Double, to: VK::Float }),
                    RI::D2I                     => instructions.push(MiniInstr::Convert  { from: VK::Double, to: VK::Int   }),
                    RI::D2L                     => instructions.push(MiniInstr::Convert  { from: VK::Double, to: VK::Long  }),
                    RI::DAdd                    => instructions.push(MiniInstr::BinOp    { op: BinOp::Add, value_kind: VK::Double }),
                    RI::DCmpG                   => instructions.push(MiniInstr::BinOp    { op: BinOp::GreaterThan, value_kind: VK::Double }),
                    RI::DCmpL                   => instructions.push(MiniInstr::BinOp    { op: BinOp::LessThan, value_kind: VK::Double }),
                    RI::DConst0                 => instructions.push(MiniInstr::Constant { value: CV::Double(0.) }),
                    RI::DConst1                 => instructions.push(MiniInstr::Constant { value: CV::Double(1.) }),
                    RI::DDiv                    => instructions.push(MiniInstr::BinOp    { op: BinOp::Div, value_kind: VK::Double }),
                    RI::DLoad { index }         => instructions.push(MiniInstr::Load     { kind: VK::Double, index: index.into() }),
                    RI::DLoadW { index }        => instructions.push(MiniInstr::Load     { kind: VK::Double, index: index.into() }),
                    RI::DLoad0                  => instructions.push(MiniInstr::Load     { kind: VK::Double, index: 0 }),
                    RI::DLoad1                  => instructions.push(MiniInstr::Load     { kind: VK::Double, index: 1 }),
                    RI::DLoad2                  => instructions.push(MiniInstr::Load     { kind: VK::Double, index: 2 }),
                    RI::DLoad3                  => instructions.push(MiniInstr::Load     { kind: VK::Double, index: 3 }),
                    RI::DMul                    => instructions.push(MiniInstr::BinOp    { op: BinOp::Mul, value_kind: VK::Double }),
                    RI::DNeg                    => instructions.push(MiniInstr::UnOp     { op: UnOp::Neg, value_kind: VK::Double }),
                    RI::DRem                    => instructions.push(MiniInstr::BinOp    { op: BinOp::Rem, value_kind: VK::Double }),
                    RI::DReturn                 => instructions.push(MiniInstr::Return   { kind: Some(VK::Double) }),
                    RI::DStore { index }        => instructions.push(MiniInstr::Store    { kind: VK::Double, index: index.into() }),
                    RI::DStoreW { index }       => instructions.push(MiniInstr::Store    { kind: VK::Double, index: index.into() }),
                    RI::DStore0                 => instructions.push(MiniInstr::Store    { kind: VK::Double, index: 0 }),
                    RI::DStore1                 => instructions.push(MiniInstr::Store    { kind: VK::Double, index: 1 }),
                    RI::DStore2                 => instructions.push(MiniInstr::Store    { kind: VK::Double, index: 2 }),
                    RI::DStore3                 => instructions.push(MiniInstr::Store    { kind: VK::Double, index: 3 }),
                    RI::DSub                    => instructions.push(MiniInstr::BinOp    { op: BinOp::Sub, value_kind: VK::Double }),
                    RI::Dup                     => instructions.push(MiniInstr::Dup      { count: 1, depth: 0 }),
                    RI::DupX1                   => instructions.push(MiniInstr::Dup      { count: 1, depth: 1 }),
                    RI::DupX2                   => instructions.push(MiniInstr::Dup      { count: 1, depth: 2 }),
                    RI::Dup2                    => instructions.push(MiniInstr::Dup      { count: 2, depth: 0 }),
                    RI::Dup2X1                  => instructions.push(MiniInstr::Dup      { count: 2, depth: 1 }),
                    RI::Dup2X2                  => instructions.push(MiniInstr::Dup      { count: 2, depth: 2 }),
                    RI::F2D                     => instructions.push(MiniInstr::Convert  { from: VK::Float, to: VK::Double }),
                    RI::F2I                     => instructions.push(MiniInstr::Convert  { from: VK::Float, to: VK::Int   }),
                    RI::F2L                     => instructions.push(MiniInstr::Convert  { from: VK::Float, to: VK::Long  }),
                    RI::FAdd                    => instructions.push(MiniInstr::BinOp    { op: BinOp::Add, value_kind: VK::Float }),
                    RI::FCmpG                   => instructions.push(MiniInstr::BinOp    { op: BinOp::GreaterThan, value_kind: VK::Float }),
                    RI::FCmpL                   => instructions.push(MiniInstr::BinOp    { op: BinOp::LessThan, value_kind: VK::Float }),
                    RI::FConst0                 => instructions.push(MiniInstr::Constant { value: CV::Float(0.) }),
                    RI::FConst1                 => instructions.push(MiniInstr::Constant { value: CV::Float(1.) }),
                    RI::FConst2                 => instructions.push(MiniInstr::Constant { value: CV::Float(2.) }),
                    RI::FDiv                    => instructions.push(MiniInstr::BinOp    { op: BinOp::Div, value_kind: VK::Float }),
                    RI::FLoad { index }         => instructions.push(MiniInstr::Load     { kind: VK::Float, index: index.into() }),
                    RI::FLoadW { index }        => instructions.push(MiniInstr::Load     { kind: VK::Float, index: index.into() }),
                    RI::FLoad0                  => instructions.push(MiniInstr::Load     { kind: VK::Float, index: 0 }),
                    RI::FLoad1                  => instructions.push(MiniInstr::Load     { kind: VK::Float, index: 1 }),
                    RI::FLoad2                  => instructions.push(MiniInstr::Load     { kind: VK::Float, index: 2 }),
                    RI::FLoad3                  => instructions.push(MiniInstr::Load     { kind: VK::Float, index: 3 }),
                    RI::FMul                    => instructions.push(MiniInstr::BinOp    { op: BinOp::Mul, value_kind: VK::Float }),
                    RI::FNeg                    => instructions.push(MiniInstr::UnOp     { op: UnOp::Neg, value_kind: VK::Float }),
                    RI::FRem                    => instructions.push(MiniInstr::BinOp    { op: BinOp::Rem, value_kind: VK::Float }),
                    RI::FReturn                 => instructions.push(MiniInstr::Return   { kind: Some(VK::Float) }),
                    RI::FStore { index }        => instructions.push(MiniInstr::Store    { kind: VK::Float, index: index.into() }),
                    RI::FStoreW { index }       => instructions.push(MiniInstr::Store    { kind: VK::Float, index: index.into() }),
                    RI::FStore0                 => instructions.push(MiniInstr::Store    { kind: VK::Float, index: 0 }),
                    RI::FStore1                 => instructions.push(MiniInstr::Store    { kind: VK::Float, index: 1 }),
                    RI::FStore2                 => instructions.push(MiniInstr::Store    { kind: VK::Float, index: 2 }),
                    RI::FStore3                 => instructions.push(MiniInstr::Store    { kind: VK::Float, index: 3 }),
                    RI::FSub                    => instructions.push(MiniInstr::BinOp    { op: BinOp::Sub, value_kind: VK::Float }),
                    RI::GetField { index }      => instructions.push(MiniInstr::GetField { is_static: false, field: make_field_ref(pool!(index)?)? }),
                    RI::GetStatic { index }     => instructions.push(MiniInstr::GetField { is_static: true, field: make_field_ref(pool!(index)?)? }),
                    RI::Goto { offset }         => instructions.push(MiniInstr::Goto     { offset: offset.into(), cond: None }),
                    RI::GotoW { offset }        => instructions.push(MiniInstr::Goto     { offset, cond: None }),
                    RI::I2B                     => instructions.push(MiniInstr::Convert  { from: VK::Int, to: VK::Byte }),
                    RI::I2C                     => instructions.push(MiniInstr::Convert  { from: VK::Int, to: VK::Char }),
                    RI::I2D                     => instructions.push(MiniInstr::Convert  { from: VK::Int, to: VK::Double }),
                    RI::I2F                     => instructions.push(MiniInstr::Convert  { from: VK::Int, to: VK::Float }),
                    RI::I2L                     => instructions.push(MiniInstr::Convert  { from: VK::Int, to: VK::Long }),
                    RI::I2S                     => instructions.push(MiniInstr::Convert  { from: VK::Int, to: VK::Short }),
                    RI::IAdd                    => instructions.push(MiniInstr::BinOp    { op: BinOp::Add, value_kind: VK::Int }),
                    RI::IAnd                    => instructions.push(MiniInstr::BinOp    { op: BinOp::BitAnd, value_kind: VK::Int }),
                    RI::IConstM1                => instructions.push(MiniInstr::Constant { value: CV::Int(-1) }),
                    RI::IConst0                 => instructions.push(MiniInstr::Constant { value: CV::Int(0) }),
                    RI::IConst1                 => instructions.push(MiniInstr::Constant { value: CV::Int(1) }),
                    RI::IConst2                 => instructions.push(MiniInstr::Constant { value: CV::Int(2) }),
                    RI::IConst3                 => instructions.push(MiniInstr::Constant { value: CV::Int(3) }),
                    RI::IConst4                 => instructions.push(MiniInstr::Constant { value: CV::Int(4) }),
                    RI::IConst5                 => instructions.push(MiniInstr::Constant { value: CV::Int(5) }),
                    RI::IDiv                    => instructions.push(MiniInstr::BinOp    { op: BinOp::Div, value_kind: VK::Int }),
                    RI::IfACmpEq { offset }     => instructions.push(MiniInstr::Goto     { offset: offset.into(), cond: Some(GotoCondition { operand: IfOperand::Ref, cmp: IfCmp::Eq }) }),
                    RI::IfACmpNe { offset }     => instructions.push(MiniInstr::Goto     { offset: offset.into(), cond: Some(GotoCondition { operand: IfOperand::Ref, cmp: IfCmp::Ne }) }),
                    RI::IfICmpEq { offset }     => instructions.push(MiniInstr::Goto     { offset: offset.into(), cond: Some(GotoCondition { operand: IfOperand::Int, cmp: IfCmp::Eq }) }),
                    RI::IfICmpNe { offset }     => instructions.push(MiniInstr::Goto     { offset: offset.into(), cond: Some(GotoCondition { operand: IfOperand::Int, cmp: IfCmp::Ne }) }),
                    RI::IfICmpLt { offset }     => instructions.push(MiniInstr::Goto     { offset: offset.into(), cond: Some(GotoCondition { operand: IfOperand::Int, cmp: IfCmp::Lt }) }),
                    RI::IfICmpGe { offset }     => instructions.push(MiniInstr::Goto     { offset: offset.into(), cond: Some(GotoCondition { operand: IfOperand::Int, cmp: IfCmp::Ge }) }),
                    RI::IfICmpGt { offset }     => instructions.push(MiniInstr::Goto     { offset: offset.into(), cond: Some(GotoCondition { operand: IfOperand::Int, cmp: IfCmp::Gt }) }),
                    RI::IfICmpLe { offset }     => instructions.push(MiniInstr::Goto     { offset: offset.into(), cond: Some(GotoCondition { operand: IfOperand::Int, cmp: IfCmp::Le }) }),
                    RI::IfEq { offset }         => instructions.push(MiniInstr::Goto     { offset: offset.into(), cond: Some(GotoCondition { operand: IfOperand::Zero, cmp: IfCmp::Eq }) }),
                    RI::IfNe { offset }         => instructions.push(MiniInstr::Goto     { offset: offset.into(), cond: Some(GotoCondition { operand: IfOperand::Zero, cmp: IfCmp::Ne }) }),
                    RI::IfLt { offset }         => instructions.push(MiniInstr::Goto     { offset: offset.into(), cond: Some(GotoCondition { operand: IfOperand::Zero, cmp: IfCmp::Lt }) }),
                    RI::IfGe { offset }         => instructions.push(MiniInstr::Goto     { offset: offset.into(), cond: Some(GotoCondition { operand: IfOperand::Zero, cmp: IfCmp::Ge }) }),
                    RI::IfGt { offset }         => instructions.push(MiniInstr::Goto     { offset: offset.into(), cond: Some(GotoCondition { operand: IfOperand::Zero, cmp: IfCmp::Gt }) }),
                    RI::IfLe { offset }         => instructions.push(MiniInstr::Goto     { offset: offset.into(), cond: Some(GotoCondition { operand: IfOperand::Zero, cmp: IfCmp::Le }) }),
                    RI::IfNonNull { offset }    => instructions.push(MiniInstr::Goto     { offset: offset.into(), cond: Some(GotoCondition { operand: IfOperand::Null, cmp: IfCmp::Ne }) }),
                    RI::IfNull { offset }       => instructions.push(MiniInstr::Goto     { offset: offset.into(), cond: Some(GotoCondition { operand: IfOperand::Null, cmp: IfCmp::Eq }) }),
                    RI::IInc { index, value }   => instructions.push(MiniInstr::IncInt   { index: index.into(), value: value.into() }),
                    RI::IIncW { index, value }  => instructions.push(MiniInstr::IncInt   { index: index.into(), value: value.into() }),
                    RI::ILoad { index }         => instructions.push(MiniInstr::Load     { kind: VK::Int, index: index.into() }),
                    RI::ILoadW { index }        => instructions.push(MiniInstr::Load     { kind: VK::Int, index: index.into() }),
                    RI::ILoad0                  => instructions.push(MiniInstr::Load     { kind: VK::Int, index: 0 }),
                    RI::ILoad1                  => instructions.push(MiniInstr::Load     { kind: VK::Int, index: 1 }),
                    RI::ILoad2                  => instructions.push(MiniInstr::Load     { kind: VK::Int, index: 2 }),
                    RI::ILoad3                  => instructions.push(MiniInstr::Load     { kind: VK::Int, index: 3 }),
                    RI::IMul                    => instructions.push(MiniInstr::BinOp    { op: BinOp::Mul, value_kind: VK::Int }),
                    RI::INeg                    => instructions.push(MiniInstr::UnOp     { op: UnOp::Neg, value_kind: VK::Int }),
                    RI::InvokeDynamic { index } => {
                        let invoke_dyn = pool!(index)?;
                        let name_and_type = pool!(invoke_dyn.name_and_type)?;
                        instructions.push(MiniInstr::InvokeDynamic {
                            call_site: bootstrap_methods[invoke_dyn.bootstrap_method_attr as usize].clone(),
                            name: minijvm::Ident(pool_str!(name_and_type.name)?.into()),
                            descriptor: minijvm::MethodDescriptor::parse_complete(pool_str!(name_and_type.descriptor)?)?,
                        });
                    },
                    RI::InvokeInterface { index, count } => instructions.push(MiniInstr::Invoke   { kind: minijvm::InvokeKind::Interface { count }, method: make_interface_method_ref(pool!(index)?)? }),
                    RI::InvokeSpecial { index }          => instructions.push(MiniInstr::Invoke   { kind: minijvm::InvokeKind::Special, method: make_method_ref_from_item(pool!(index)?)? }),
                    RI::InvokeStatic { index }           => instructions.push(MiniInstr::Invoke   { kind: minijvm::InvokeKind::Static, method: make_method_ref_from_item(pool!(index)?)? }),
                    RI::InvokeVirtual { index }          => instructions.push(MiniInstr::Invoke   { kind: minijvm::InvokeKind::Virtual, method: make_method_ref(pool!(index)?)? }),
                    RI::IOr                              => instructions.push(MiniInstr::BinOp    { op: BinOp::BitOr, value_kind: VK::Int }),
                    RI::IRem                             => instructions.push(MiniInstr::BinOp    { op: BinOp::Rem, value_kind: VK::Int }),
                    RI::IReturn                          => instructions.push(MiniInstr::Return   { kind: Some(VK::Int) }),
                    RI::IShL                             => instructions.push(MiniInstr::BinOp    { op: BinOp::BitShl, value_kind: VK::Int }),
                    RI::IShR                             => instructions.push(MiniInstr::BinOp    { op: BinOp::BitShr, value_kind: VK::Int }),
                    RI::IStore { index }                 => instructions.push(MiniInstr::Store    { kind: VK::Int, index: index.into() }),
                    RI::IStoreW { index }                => instructions.push(MiniInstr::Store    { kind: VK::Int, index: index.into() }),
                    RI::IStore0                          => instructions.push(MiniInstr::Store    { kind: VK::Int, index: 0 }),
                    RI::IStore1                          => instructions.push(MiniInstr::Store    { kind: VK::Int, index: 1 }),
                    RI::IStore2                          => instructions.push(MiniInstr::Store    { kind: VK::Int, index: 2 }),
                    RI::IStore3                          => instructions.push(MiniInstr::Store    { kind: VK::Int, index: 3 }),
                    RI::ISub                             => instructions.push(MiniInstr::BinOp    { op: BinOp::Sub, value_kind: VK::Int }),
                    RI::IUShR                            => instructions.push(MiniInstr::BinOp    { op: BinOp::BitUShr, value_kind: VK::Int }),
                    RI::IXor                             => instructions.push(MiniInstr::BinOp    { op: BinOp::BitXOr, value_kind: VK::Int }),
                    RI::JSr { offset }                   => instructions.push(MiniInstr::Jsr      { offset: offset.into() }),
                    RI::JSrW { offset }                  => instructions.push(MiniInstr::Jsr      { offset: offset.into() }),
                    RI::L2D                              => instructions.push(MiniInstr::Convert  { from: VK::Long, to: VK::Double }),
                    RI::L2F                              => instructions.push(MiniInstr::Convert  { from: VK::Long, to: VK::Float }),
                    RI::L2I                              => instructions.push(MiniInstr::Convert  { from: VK::Long, to: VK::Int }),
                    RI::LAdd                             => instructions.push(MiniInstr::BinOp    { op: BinOp::Add, value_kind: VK::Long }),
                    RI::LAnd                             => instructions.push(MiniInstr::BinOp    { op: BinOp::BitAnd, value_kind: VK::Long }),
                    RI::LCmp                             => instructions.push(MiniInstr::BinOp    { op: BinOp::Cmp, value_kind: VK::Long }),
                    RI::LConst0                          => instructions.push(MiniInstr::Constant { value: CV::Long(0) }),
                    RI::LConst1                          => instructions.push(MiniInstr::Constant { value: CV::Long(1) }),
                    RI::LdC { index }                    => instructions.push(MiniInstr::Ldc      { constant: constant_from_item(pool!(index)?)? }),
                    RI::LdCW { index }                   => instructions.push(MiniInstr::Ldc      { constant: constant_from_item(pool!(index)?)? }),
                    RI::LdC2W { index }                  => instructions.push(MiniInstr::Ldc      { constant: constant_from_item(pool!(index)?)? }),
                    RI::LDiv                             => instructions.push(MiniInstr::BinOp    { op: BinOp::Div, value_kind: VK::Long }),
                    RI::LLoad { index }                  => instructions.push(MiniInstr::Load     { kind: VK::Long, index: index.into() }),
                    RI::LLoadW { index }                 => instructions.push(MiniInstr::Load     { kind: VK::Long, index: index.into() }),
                    RI::LLoad0                           => instructions.push(MiniInstr::Load     { kind: VK::Long, index: 0 }),
                    RI::LLoad1                           => instructions.push(MiniInstr::Load     { kind: VK::Long, index: 1 }),
                    RI::LLoad2                           => instructions.push(MiniInstr::Load     { kind: VK::Long, index: 2 }),
                    RI::LLoad3                           => instructions.push(MiniInstr::Load     { kind: VK::Long, index: 3 }),
                    RI::LMul                             => instructions.push(MiniInstr::BinOp    { op: BinOp::Mul, value_kind: VK::Long }),
                    RI::LNeg                             => instructions.push(MiniInstr::UnOp     { op: UnOp::Neg, value_kind: VK::Long }),
                    RI::LOr                              => instructions.push(MiniInstr::BinOp    { op: BinOp::BitOr, value_kind: VK::Long }),
                    RI::LRem                             => instructions.push(MiniInstr::BinOp    { op: BinOp::Rem, value_kind: VK::Long }),
                    RI::LReturn                          => instructions.push(MiniInstr::Return   { kind: Some(VK::Long) }),
                    RI::LShL                             => instructions.push(MiniInstr::BinOp    { op: BinOp::BitShl, value_kind: VK::Long }),
                    RI::LShR                             => instructions.push(MiniInstr::BinOp    { op: BinOp::BitShr, value_kind: VK::Long }),
                    RI::LStore { index }                 => instructions.push(MiniInstr::Store    { kind: VK::Long, index: index.into() }),
                    RI::LStoreW { index }                => instructions.push(MiniInstr::Store    { kind: VK::Long, index: index.into() }),
                    RI::LStore0                          => instructions.push(MiniInstr::Store    { kind: VK::Long, index: 0 }),
                    RI::LStore1                          => instructions.push(MiniInstr::Store    { kind: VK::Long, index: 1 }),
                    RI::LStore2                          => instructions.push(MiniInstr::Store    { kind: VK::Long, index: 2 }),
                    RI::LStore3                          => instructions.push(MiniInstr::Store    { kind: VK::Long, index: 3 }),
                    RI::LSub                             => instructions.push(MiniInstr::BinOp    { op: BinOp::Sub, value_kind: VK::Long }),
                    RI::LUShR                            => instructions.push(MiniInstr::BinOp    { op: BinOp::BitUShr, value_kind: VK::Long }),
                    RI::LXor                             => instructions.push(MiniInstr::BinOp    { op: BinOp::BitXOr, value_kind: VK::Long }),
                    RI::New { index }                    => instructions.push(MiniInstr::New      { class: make_class_ref(pool!(index)?)? }),
                    RI::Nop                              => instructions.push(MiniInstr::Noop),
                    RI::Pop                              => instructions.push(MiniInstr::Pop      { count: 1 }),
                    RI::Pop2                             => instructions.push(MiniInstr::Pop      { count: 2 }),
                    RI::PutField { index }               => instructions.push(MiniInstr::PutField { is_static: false, field: make_field_ref(pool!(index)?)? }),
                    RI::PutStatic { index }              => instructions.push(MiniInstr::PutField { is_static: true, field: make_field_ref(pool!(index)?)? }),
                    RI::Ret { index }                    => instructions.push(MiniInstr::Ret      { index: index.into() }),
                    RI::RetW { index }                   => instructions.push(MiniInstr::Ret      { index: index.into() }),
                    RI::Return                           => instructions.push(MiniInstr::Return   { kind: None }),
                    RI::SIPush { value }                 => instructions.push(MiniInstr::Constant { value: CV::Short(value) }),
                    RI::Swap                             => instructions.push(MiniInstr::Swap),

                    _ => {
                        warn!(?instruction, "Unknown instruction");
                        instructions.push(MiniInstr::Unknown(format!("{instruction:?}")));
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
                found_code = Some(convert_code(code)?);
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

impl super::ExtractorKind for ReadClassExtractor {
    type Output = minijvm::Class;
    
    fn name(&self) -> &'static str {
        "read_class_extractor"
    }

    async fn extract(self, manager: &mut super::ExtractionManager<'_>) -> anyhow::Result<Self::Output> {
        let server_jar_path = manager.extract(super::server_jar::ServerJarExtractor).await?;
        crate::spawn_cpu_bound(move || Self::read_class(&server_jar_path, &self.class)).await?
    }
}
