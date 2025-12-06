use crate::{mappings, minijvm::{self, decomped}};

use anyhow::anyhow;

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
                Instr::Dup { count, depth } => todo!(),
                Instr::Pop { count } => {
                    for _ in 0..*count {
                        statements.push(decomped::Statement::Expression { expr: pop!() });
                    }
                },
                Instr::Swap => todo!(),
                Instr::Constant { value } => stack.push(decomped::Expression::Constant { value: value.clone() }),
                Instr::Convert { from, to } => todo!(),
                Instr::Load { kind, index } => stack.push(decomped::Expression::Load { value_kind: kind.clone(), index: *index }),
                Instr::Store { kind, index } => {
                    let value = Box::new(pop!());
                    stack.push(decomped::Expression::Store {
                        value_kind: kind.clone(),
                        index: *index,
                        value,
                    });
                },
                Instr::IncInt { index, value } => todo!(),
                Instr::Goto { offset, cond } => todo!(),
                Instr::Jsr { offset } => todo!(),
                Instr::Ret { index } => todo!(),
                Instr::Ldc { constant } => todo!(),
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
                        minijvm::InvokeKind::Static |
                        minijvm::InvokeKind::Virtual => None,
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

                    if method.descriptor.return_type.ty == minijvm::TypeDescriptorKind::Void {
                        statements.push(decomped::Statement::Expression { expr });
                    }
                    else {
                        stack.push(expr);
                    }
                },
                Instr::InvokeDynamic { call_site, name, descriptor } => todo!(),
                Instr::GetField { is_static, field } => {
                    stack.push(decomped::Expression::GetField {
                        is_static: is_static.clone(),
                        field: field.clone(),
                    });
                },
                Instr::PutField { is_static, field } => {
                    statements.push(decomped::Statement::PutField {
                        is_static: is_static.clone(),
                        field: field.clone(),
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
                Instr::Unknown(_) => todo!(),
            }
        }

        Ok(vec![])
    }

    fn decomp(class: &minijvm::Class) -> anyhow::Result<decomped::Class> {
        Ok(decomped::Class {
            name: class.name.clone(),
            super_class: class.super_class.clone(),
            methods: class.methods.iter().map(|method| {
                decomped::Method {
                    name: method.name.clone(),
                    descriptor: method.descriptor.clone(),
                    access_flags: method.access_flags.clone(),
                    code: todo!(),
                }
            }).collect(),
            fields: class.fields.iter().map(|field| {
                decomped::Field {
                    name: field.name.clone(),
                    descriptor: field.descriptor.clone(),
                    access_flags: field.access_flags.clone(),
                    init_value: todo!(),
                }
            }).collect(),
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
