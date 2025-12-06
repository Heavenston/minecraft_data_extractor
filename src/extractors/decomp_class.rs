use crate::{mappings, minijvm::{self, decomped}};

use anyhow::anyhow;
use tracing::{debug, warn};
use itertools::Itertools;

#[derive(Debug, bincode::Encode)]
pub struct DecompClassExtractor {
    pub class: String,
    pub mappings_brand: mappings::Brand,
}

impl DecompClassExtractor {
    fn decomp_code(code: &minijvm::Code) -> anyhow::Result<Vec<decomped::Statement>> {
        use minijvm::Instruction as Instr;

        let mut stack = Vec::<decomped::Expression>::new();
        let mut statements = Vec::<decomped::Statement>::new();

        macro_rules! pop {
            () => { stack.pop().ok_or_else(|| anyhow!("Missing expression in stack"))? };
        }
        
        for instruction in &code.instructions {
            match instruction {
                Instr::Noop => (),
                Instr::Dup { .. } => unimplemented!(),
                Instr::Pop { count } => {
                    for _ in 0..*count {
                        statements.push(decomped::Statement::Expression { expr: pop!() });
                    }
                },
                Instr::Swap => unimplemented!(),
                Instr::Constant { value } => stack.push(decomped::Expression::Constant {
                    value: match value {
                        minijvm::ConstantValue::Byte(v) => decomped::Constant::Byte(*v),
                        minijvm::ConstantValue::Short(v) => decomped::Constant::Short(*v),
                        minijvm::ConstantValue::Int(v) => decomped::Constant::Int(*v),
                        minijvm::ConstantValue::Long(v) => decomped::Constant::Long(*v),
                        minijvm::ConstantValue::Float(v) => decomped::Constant::Float(*v),
                        minijvm::ConstantValue::Double(v) => decomped::Constant::Double(*v),
                        minijvm::ConstantValue::String(v) => decomped::Constant::String(v.clone()),
                        minijvm::ConstantValue::Null => decomped::Constant::Null,
                    },
                }),
                Instr::Convert { from, to } => {
                    let value = Box::new(pop!());
                    stack.push(decomped::Expression::Convert {
                        from: from.clone(),
                        to: to.clone(),
                        value,
                    });
                },
                Instr::Load { kind, index } => stack.push(decomped::Expression::Load { value_kind: kind.clone(), index: *index }),
                Instr::Store { kind, index } => {
                    statements.push(decomped::Statement::Store {
                        value_kind: kind.clone(),
                        index: *index,
                        value: pop!(),
                    });
                },
                Instr::IncInt { .. } => unimplemented!(),
                Instr::Goto { .. } => unimplemented!(),
                Instr::Jsr { .. } => unimplemented!(),
                Instr::Ret { .. } => unimplemented!(),
                Instr::Ldc { constant } => {
                    stack.push(decomped::Expression::Constant {
                        value: match constant {
                            minijvm::Constant::Int(v) => decomped::Constant::Int(*v),
                            minijvm::Constant::Long(v) => decomped::Constant::Long(*v),
                            minijvm::Constant::Float(v) => decomped::Constant::Float(*v),
                            minijvm::Constant::Double(v) => decomped::Constant::Double(*v),
                            minijvm::Constant::String(v) => decomped::Constant::String(v.clone()),
                            minijvm::Constant::Class(class_ref) => decomped::Constant::Class(class_ref.clone()),
                            minijvm::Constant::MethodHandle(method_ref) => decomped::Constant::MethodHandle(method_ref.clone()),
                            minijvm::Constant::MethodType(method_descriptor) => decomped::Constant::MethodType(method_descriptor.clone()),
                            minijvm::Constant::Null => decomped::Constant::Null,
                        },
                    });
                },
                Instr::BinOp { op, value_kind } => {
                    let rhs = Box::new(pop!());
                    let lhs = Box::new(pop!());

                    stack.push(decomped::Expression::BinOp {
                        op: op.clone(),
                        value_kind: value_kind.clone(),
                        lhs,
                        rhs,
                    });
                },
                Instr::UnOp { op, value_kind } => {
                    let operand = Box::new(pop!());
                    stack.push(decomped::Expression::UnOp {
                        op: op.clone(),
                        value_kind: value_kind.clone(),
                        operand,
                    });
                },
                Instr::Invoke { kind, method } => {
                    let object = match kind {
                        minijvm::InvokeKind::Static => None,
                        minijvm::InvokeKind::Virtual |
                        minijvm::InvokeKind::Interface { .. } |
                        minijvm::InvokeKind::Special => Some(Box::new(pop!())),
                    };

                    let mut args = Vec::new();
                    for _ in 0..method.descriptor.args.len() {
                        args.push(pop!());
                    }
                    args.reverse();
                    
                    let expr = decomped::Expression::Invoke {
                        kind: kind.clone(),
                        method: method.clone(),
                        object,
                        args,
                    };

                    if method.descriptor.return_type.ty.is_void() {
                        statements.push(decomped::Statement::Expression { expr });
                    }
                    else {
                        stack.push(expr);
                    }
                },
                Instr::InvokeDynamic { call_site, name, descriptor } => {
                    let mut args = Vec::new();
                    for _ in 0..descriptor.args.len() {
                        args.push(pop!());
                    }
                    args.reverse();
                    
                    let expr = decomped::Expression::InvokeDynamic {
                        call_site: call_site.clone(),
                        name: name.clone(),
                        descriptor: descriptor.clone(),
                        args,
                    };

                    if descriptor.return_type.ty.is_void() {
                        statements.push(decomped::Statement::Expression { expr });
                    }
                    else {
                        stack.push(expr);
                    }
                },
                Instr::GetField { is_static, field } => {
                    stack.push(decomped::Expression::GetField {
                        is_static: is_static.clone(),
                        field: field.clone(),
                    });
                },
                Instr::PutField { is_static, field } => {
                    let object = if *is_static { None } else { Some(Box::new(pop!())) };
                    statements.push(decomped::Statement::PutField {
                        is_static: is_static.clone(),
                        field: field.clone(),
                        object,
                        value: pop!(),
                    });
                },
                Instr::Return { kind } => {
                    statements.push(decomped::Statement::Return {
                        value: if let Some(kind) = kind {
                            Some((kind.clone(), pop!()))
                        } else {
                            None
                        },
                    });
                },
                Instr::Throw => {
                    statements.push(decomped::Statement::Throw {
                        value: pop!(),
                    });
                },
                Instr::New { class } => {
                    stack.push(decomped::Expression::New {
                        class: class.clone(),
                    });
                },
                Instr::Unknown(instruction) => warn!(%instruction, "Cannot decompile unnkown instruction"),
            }
        }

        if !stack.is_empty() {
            warn!("Stack insn't empty at the end of decompilation, it this a bug?");
        }
        Ok(statements)
    }

    fn decomp(class: &minijvm::Class) -> anyhow::Result<decomped::Class> {
        Ok(decomped::Class {
            name: class.name.clone(),
            super_class: class.super_class.clone(),
            methods: class.methods.iter().map(|method| -> anyhow::Result<decomped::Method> {
                Ok(decomped::Method {
                    name: method.name.clone(),
                    descriptor: method.descriptor.clone(),
                    access_flags: method.access_flags.clone(),
                    code: Self::decomp_code(&method.code)?,
                })
            }).try_collect()?,
            fields: class.fields.iter().map(|field| -> anyhow::Result<decomped::Field> {
                Ok(decomped::Field {
                    name: field.name.clone(),
                    descriptor: field.descriptor.clone(),
                    access_flags: field.access_flags.clone(),
                    // TODO
                    init_value: None,
                })
            }).try_collect()?,
        })
    }
}

impl super::ExtractorKind for DecompClassExtractor {
    type Output = decomped::Class;

    fn name(&self) -> &'static str {
        "decomp_class_extractor"
    }

    async fn extract(self, manager: &mut super::ExtractionManager<'_>) -> anyhow::Result<decomped::Class> {
        let mapped_class = manager.extract(super::mapped_class::MappedClassExtractor{
            class: self.class,
            mappings_brand: self.mappings_brand,
        }).await?;
        
        crate::spawn_cpu_bound(move || Self::decomp(&mapped_class)).await?
    }
}
