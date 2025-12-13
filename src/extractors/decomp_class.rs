use crate::{mappings, minijvm::{self, decomped::{self, visitor::mut_visitor as mv}}};
use mv::MutVisitor;

use std::collections::HashMap;
use anyhow::anyhow;
use tracing::warn;
use itertools::Itertools;

struct LambdaExtractor;

impl mv::MutVisitor for LambdaExtractor {
    fn visit_expression(&mut self, expr: &mut decomped::Expression) -> anyhow::Result<()> {
        if let decomped::Expression::InvokeDynamic { call_site, name, args, .. } = expr {
            if call_site.bootstrap.class.descriptor.to_string() == "java.lang.invoke.LambdaMetafactory" {
                if let Some(minijvm::Constant::MethodHandle(handle)) = call_site.static_args.get(1) {
                    if let minijvm::MethodHandleRef::Method(target) = &handle.reference {
                        let mut captures = std::mem::take(args);
                        for capture in &mut captures {
                            self.visit_expression(capture)?;
                        }
                        *expr = decomped::Expression::Lambda {
                            target: target.clone(),
                            interface_method: name.clone(),
                            captures,
                        };
                        return Ok(());
                    }
                }
            }
        }

        mv::walk_expression(self, expr)
    }
}

struct UsesLocalExtractor {
    pub uses_a_local: bool,
}

impl mv::MutVisitor for UsesLocalExtractor {
    fn visit_expression(&mut self, expr: &mut decomped::Expression) -> anyhow::Result<()> {
        let uses_a_local = match expr {
            decomped::Expression::Load { .. } |
            decomped::Expression::LoadTemp { .. } => true,
            _ => false,
        };

        if uses_a_local || self.uses_a_local {
            self.uses_a_local = true;
        }
        else {
            mv::walk_expression(self, expr)?;
        }

        Ok(())
    }
}

fn simplify_temps(statements: &mut Vec<decomped::Statement>) -> anyhow::Result<()> {
    struct UsageCounter(HashMap<u16, usize>);

    impl mv::MutVisitor for UsageCounter {
        fn visit_expression(&mut self, expr: &mut decomped::Expression) -> anyhow::Result<()> {
            if let decomped::Expression::LoadTemp { index } = expr {
                *self.0.entry(*index).or_default() += 1;
            }

            mv::walk_expression(self, expr)
        }
    }

    struct TempInliner {
        values: HashMap<u16, decomped::Expression>,
    }

    impl mv::MutVisitor for TempInliner {
        fn visit_expression(&mut self, expr: &mut decomped::Expression) -> anyhow::Result<()> {
            if let decomped::Expression::LoadTemp { index } = expr && let Some(value) = self.values.remove(index) {
                *expr = value;
                self.visit_expression(expr)?;
                Ok(())
            }
            else {
                mv::walk_expression(self, expr)
            }
        }
    }

    let mut usage_counts = UsageCounter(HashMap::new());
    for stmt in statements.iter_mut() {
        usage_counts.visit_statement(stmt)?;
    }

    let mut values_to_inline = HashMap::new();
    let mut indices_to_remove = Vec::new();

    for (idx, stmt) in statements.iter().enumerate() {
        if let decomped::Statement::StoreTemp { index, value } = stmt {
            let usage_count = usage_counts.0.get(index).copied().unwrap_or(0);
            let is_shadow = matches!(value, decomped::Expression::LoadTemp { .. });
            if usage_count <= 1 || is_shadow {
                values_to_inline.insert(*index, value.clone());
                indices_to_remove.push(idx);
            }
        }
    }

    let mut inliner = TempInliner { values: values_to_inline };
    for stmt in statements.iter_mut() {
        inliner.visit_statement(stmt)?;
    }

    for idx in indices_to_remove.into_iter().rev() {
        statements.remove(idx);
    }

    Ok(())
}

struct TempSimplifier;

impl TempSimplifier {
    fn simplify_block(&mut self, statements: &mut Vec<decomped::Statement>) -> anyhow::Result<()> {
        simplify_temps(statements)?;
        for stmt in statements {
            self.visit_statement(stmt)?;
        }
        Ok(())
    }
}

impl mv::MutVisitor for TempSimplifier {
    fn visit_method(&mut self, method: &mut decomped::Method) -> anyhow::Result<()> {
        self.simplify_block(&mut method.code)?;
        Ok(())
    }

    fn visit_statement(&mut self, stmt: &mut decomped::Statement) -> anyhow::Result<()> {
        match stmt {
            decomped::Statement::If { then_branch, else_branch, .. } => {
                self.simplify_block(then_branch)?;
                self.simplify_block(else_branch)?;
            }
            decomped::Statement::While { body, .. } => {
                self.simplify_block(body)?;
            }
            _ => {}
        }

        Ok(())
    }
}

#[derive(Debug, bincode::Encode)]
pub struct DecompClassExtractor {
    pub class: String,
    pub mappings_brand: mappings::Brand,
}

fn convert_constant_value(value: &minijvm::ConstantValue) -> decomped::Constant {
    match value {
        minijvm::ConstantValue::Byte(v) => decomped::Constant::Byte(*v),
        minijvm::ConstantValue::Short(v) => decomped::Constant::Short(*v),
        minijvm::ConstantValue::Int(v) => decomped::Constant::Int(*v),
        minijvm::ConstantValue::Long(v) => decomped::Constant::Long(*v),
        minijvm::ConstantValue::Float(v) => decomped::Constant::Float(*v),
        minijvm::ConstantValue::Double(v) => decomped::Constant::Double(*v),
        minijvm::ConstantValue::String(v) => decomped::Constant::String(v.clone()),
        minijvm::ConstantValue::Null => decomped::Constant::Null,
    }
}

fn convert_constant(constant: &minijvm::Constant) -> decomped::Constant {
    match constant {
        minijvm::Constant::Int(v) => decomped::Constant::Int(*v),
        minijvm::Constant::Long(v) => decomped::Constant::Long(*v),
        minijvm::Constant::Float(v) => decomped::Constant::Float(*v),
        minijvm::Constant::Double(v) => decomped::Constant::Double(*v),
        minijvm::Constant::String(v) => decomped::Constant::String(v.clone()),
        minijvm::Constant::Class(class_ref) => decomped::Constant::Class(class_ref.clone()),
        minijvm::Constant::MethodHandle(method_ref) => decomped::Constant::MethodHandle(method_ref.clone()),
        minijvm::Constant::MethodType(method_descriptor) => decomped::Constant::MethodType(method_descriptor.clone()),
        minijvm::Constant::Null => decomped::Constant::Null,
    }
}

fn pop_args(stack: &mut Vec<decomped::Expression>, count: usize) -> anyhow::Result<Vec<decomped::Expression>> {
    if stack.len() < count {
        return Err(anyhow!("Not enough expressions on stack for arguments"));
    }
    let mut args: Vec<_> = stack.drain(stack.len() - count..).collect();
    args.reverse();
    Ok(args)
}

fn pop_stack(stack: &mut Vec<decomped::Expression>) -> anyhow::Result<decomped::Expression> {
    stack.pop().ok_or_else(|| anyhow!("Missing expression in stack"))
}

fn decomp_block(
    code: &minijvm::Code,
    pc: &mut usize,
    end: usize,
    stack: &mut Vec<decomped::Expression>,
    next_temp: &mut u16,
) -> anyhow::Result<Vec<decomped::Statement>> {
    use minijvm::Instruction as Instr;

    let mut statements = Vec::<decomped::Statement>::new();

    while *pc < end {
        let instr = &code.instructions[*pc];

        if let Instr::Goto { offset, cond } = instr {
            let target = *offset as usize;

            if let Some(cond) = cond {
                if target <= *pc {
                    warn!("Encountered backward conditional goto at pc={}", pc);
                    *pc += 1;
                    continue;
                }

                let rhs_const = match cond.operand {
                    minijvm::IfOperand::Zero => Some(decomped::Constant::Int(0)),
                    minijvm::IfOperand::Null => Some(decomped::Constant::Null),
                    _ => None,
                };

                let (lhs, rhs) = if let Some(c) = rhs_const {
                    (pop_stack(stack)?, decomped::Expression::Constant { value: c })
                } else {
                    let r = pop_stack(stack)?;
                    let l = pop_stack(stack)?;
                    (l, r)
                };

                let condition = decomped::Expression::Compare {
                    cmp: cond.cmp.clone(),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                };

                let then_start = *pc + 1;

                let then_goto = (then_start..target)
                    .filter(|&i| matches!(code.instructions[i], Instr::Goto { cond: None, .. }))
                    .last()
                    .and_then(|i| {
                        if let Instr::Goto { offset, cond: None } = &code.instructions[i] {
                            Some((i, *offset as usize))
                        } else {
                            None
                        }
                    });

                if let Some((then_goto_idx, end_idx)) = then_goto {
                    let mut then_pc = then_start;
                    let mut then_stack = stack.clone();
                    let then_branch = decomp_block(code, &mut then_pc, then_goto_idx, &mut then_stack, next_temp)?;

                    let mut else_pc = target;
                    let mut else_stack = stack.clone();
                    let else_branch = decomp_block(code, &mut else_pc, end_idx, &mut else_stack, next_temp)?;

                    let is_ternary = then_stack.len() == stack.len() + 1
                        && else_stack.len() == stack.len() + 1
                        && then_branch.is_empty()
                        && else_branch.is_empty();

                    if is_ternary {
                        let then_value = then_stack.pop().unwrap();
                        let else_value = else_stack.pop().unwrap();
                        stack.push(decomped::Expression::Ternary {
                            condition: Box::new(condition),
                            then_value: Box::new(then_value),
                            else_value: Box::new(else_value),
                        });
                    } else {
                        if then_stack.len() == stack.len() {
                            *stack = then_stack;
                        }
                        statements.push(decomped::Statement::If { condition, then_branch, else_branch });
                    }

                    *pc = end_idx;
                } else {
                    let mut then_pc = then_start;
                    let mut then_stack = stack.clone();
                    let then_branch = decomp_block(code, &mut then_pc, target, &mut then_stack, next_temp)?;

                    if then_stack.len() == stack.len() {
                        *stack = then_stack;
                    }

                    statements.push(decomped::Statement::If { condition, then_branch, else_branch: Vec::new() });
                    *pc = target;
                }
                continue;
            } else {
                warn!(offset, "Unstructured goto encountered in decompilation");
                break;
            }
        }

        match instr {
            Instr::Noop => {}
            Instr::Dup { count, depth } => {
                let depth = *depth as usize;
                let count = *count as usize;
                if stack.len() < depth + count {
                    return Err(anyhow!("Not enough values on stack for dup"));
                }
                let start = stack.len() - depth - count;
                for i in start..start + count {
                    let temp_idx = *next_temp;
                    *next_temp += 1;
                    let expr = std::mem::replace(
                        &mut stack[i],
                        decomped::Expression::LoadTemp { index: temp_idx },
                    );
                    statements.push(decomped::Statement::StoreTemp { index: temp_idx, value: expr });
                }
                let to_dup: Vec<_> = stack[start..start + count].to_vec();
                stack.extend(to_dup);
            }
            Instr::Pop { count } => {
                for _ in 0..*count {
                    statements.push(decomped::Statement::Expression { expr: pop_stack(stack)? });
                }
            }
            Instr::Swap => {
                let len = stack.len();
                if len < 2 {
                    return Err(anyhow!("Not enough values on stack for swap"));
                }
                stack.swap(len - 1, len - 2);
            }
            Instr::Constant { value } => {
                stack.push(decomped::Expression::Constant { value: convert_constant_value(value) });
            }
            Instr::Convert { from, to } => {
                let value = Box::new(pop_stack(stack)?);
                stack.push(decomped::Expression::Convert { from: from.clone(), to: to.clone(), value });
            }
            Instr::Load { kind, index } => {
                stack.push(decomped::Expression::Load { value_kind: kind.clone(), index: *index });
            }
            Instr::Store { kind, index } => {
                statements.push(decomped::Statement::Store { value_kind: kind.clone(), index: *index, value: pop_stack(stack)? });
            }
            Instr::IncInt { index, value } => {
                let load = decomped::Expression::Load { value_kind: minijvm::ValueKind::Int, index: *index };
                let increment = decomped::Expression::Constant { value: decomped::Constant::Short(*value) };
                let add = decomped::Expression::BinOp {
                    op: minijvm::BinOp::Add,
                    value_kind: minijvm::ValueKind::Int,
                    lhs: Box::new(load),
                    rhs: Box::new(increment),
                };
                statements.push(decomped::Statement::Store {
                    value_kind: minijvm::ValueKind::Int,
                    index: *index,
                    value: add,
                });
            }
            Instr::LoadFromArray { kind } => {
                let index = Box::new(pop_stack(stack)?);
                let array = Box::new(pop_stack(stack)?);
                stack.push(decomped::Expression::LoadFromArray { kind: kind.clone(), array, index });
            }
            Instr::StoreIntoArray { kind } => {
                let value = pop_stack(stack)?;
                let index = pop_stack(stack)?;
                let array = pop_stack(stack)?;
                statements.push(decomped::Statement::StoreIntoArray { kind: kind.clone(), array, index, value });
            }
            Instr::ArrayLength => {
                let array = Box::new(pop_stack(stack)?);
                stack.push(decomped::Expression::ArrayLength { array });
            }
            Instr::Jsr { offset } => {
                warn!(offset, "Jsr instruction not supported (deprecated)");
            }
            Instr::Ret { index } => {
                warn!(index, "Ret instruction not supported (deprecated)");
            }
            Instr::Ldc { constant } => {
                stack.push(decomped::Expression::Constant { value: convert_constant(constant) });
            }
            Instr::BinOp { op, value_kind } => {
                let rhs = Box::new(pop_stack(stack)?);
                let lhs = Box::new(pop_stack(stack)?);
                stack.push(decomped::Expression::BinOp { op: op.clone(), value_kind: value_kind.clone(), lhs, rhs });
            }
            Instr::UnOp { op, value_kind } => {
                let operand = Box::new(pop_stack(stack)?);
                stack.push(decomped::Expression::UnOp { op: op.clone(), value_kind: value_kind.clone(), operand });
            }
            Instr::Invoke { kind, method } => {
                let args = pop_args(stack, method.descriptor.args.len())?;

                let object = match kind {
                    minijvm::InvokeKind::Static => None,
                    _ => Some(Box::new(pop_stack(stack)?)),
                };

                if method.name.0 == "<init>" {
                    if let Some(obj) = object {
                        if let decomped::Expression::LoadTemp { index } = *obj {
                            if let Some(stmt_idx) = statements.iter().rposition(|s| {
                                matches!(s, decomped::Statement::StoreTemp { index: i, .. } if *i == index)
                            }) {
                                if let decomped::Statement::StoreTemp { value: decomped::Expression::New { ref class, args: ref new_args }, .. } = statements[stmt_idx] {
                                    if new_args.is_empty() {
                                        let new_expr = decomped::Expression::New { class: class.clone(), args };
                                        statements[stmt_idx] = decomped::Statement::StoreTemp { index, value: new_expr };
                                        *pc += 1;
                                        continue;
                                    }
                                }
                            }
                        }
                        let expr = decomped::Expression::Invoke { kind: kind.clone(), method: method.clone(), object: Some(obj), args };
                        statements.push(decomped::Statement::Expression { expr });
                    }
                    *pc += 1;
                    continue;
                }

                let expr = decomped::Expression::Invoke { kind: kind.clone(), method: method.clone(), object, args };

                if method.descriptor.return_type.ty.is_void() {
                    statements.push(decomped::Statement::Expression { expr });
                } else {
                    stack.push(expr);
                }
            }
            Instr::InvokeDynamic { call_site, name, descriptor } => {
                let args = pop_args(stack, descriptor.args.len())?;
                let expr = decomped::Expression::InvokeDynamic {
                    call_site: call_site.clone(),
                    name: name.clone(),
                    descriptor: descriptor.clone(),
                    args,
                };

                if descriptor.return_type.ty.is_void() {
                    statements.push(decomped::Statement::Expression { expr });
                } else {
                    stack.push(expr);
                }
            }
            Instr::GetField { is_static, field } => {
                let object = if *is_static { None } else { Some(Box::new(pop_stack(stack)?)) };
                stack.push(decomped::Expression::GetField { is_static: *is_static, field: field.clone(), object });
            }
            Instr::PutField { is_static, field } => {
                let value = pop_stack(stack)?;
                let object = if *is_static { None } else { Some(Box::new(pop_stack(stack)?)) };
                statements.push(decomped::Statement::PutField { is_static: *is_static, field: field.clone(), object, value });
            }
            Instr::Return { kind } => {
                let value = if let Some(kind) = kind {
                    Some((kind.clone(), pop_stack(stack)?))
                } else {
                    None
                };
                statements.push(decomped::Statement::Return { value });
            }
            Instr::Throw => {
                statements.push(decomped::Statement::Throw { value: pop_stack(stack)? });
            }
            Instr::New { class } => {
                stack.push(decomped::Expression::New { class: class.clone(), args: Vec::new() });
            }
            Instr::NewArray { kind } => {
                let count = Box::new(pop_stack(stack)?);
                stack.push(decomped::Expression::NewArray { kind: kind.clone(), count });
            }
            Instr::CheckCast { class } => {
                let value = Box::new(pop_stack(stack)?);
                stack.push(decomped::Expression::Cast { class: class.clone(), value });
            }
            Instr::Unknown(instruction) => warn!(%instruction, "Cannot decompile unknown instruction"),
            Instr::Goto { .. } => unreachable!(),
        }

        *pc += 1;
    }

    Ok(statements)
}

impl DecompClassExtractor {
    fn decomp_code(code: &minijvm::Code) -> anyhow::Result<Vec<decomped::Statement>> {
        let mut stack = Vec::new();
        let mut pc = 0;
        let mut next_temp = 0;
        let statements = decomp_block(code, &mut pc, code.instructions.len(), &mut stack, &mut next_temp)?;

        if !stack.is_empty() {
            warn!("Stack isn't empty at the end of decompilation, is this a bug?");
        }

        Ok(statements)
    }

    fn decomp(class: &minijvm::Class) -> anyhow::Result<decomped::Class> {
        let mut result = decomped::Class {
            name: class.name.clone(),
            super_class: class.super_class.clone(),
            enum_variants: Vec::new(),
            methods: class.methods.iter().map(|method| -> anyhow::Result<decomped::Method> {
                Ok(decomped::Method {
                    name: method.name.clone(),
                    descriptor: method.descriptor.clone(),
                    access_flags: method.access_flags.clone(),
                    code: Self::decomp_code(&method.code)?,
                })
            }).try_collect()?,
            fields: class.fields.iter().map(|field| decomped::Field {
                name: field.name.clone(),
                descriptor: field.descriptor.clone(),
                access_flags: field.access_flags.clone(),
                init_value: None,
            }).collect(),
        };

        LambdaExtractor.visit_class(&mut result)?;
        TempSimplifier.visit_class(&mut result)?;
        Self::extract_field_initializers(&mut result)?;

        if class.access_flags.enum_ {
            Self::extract_enum_variants(&mut result);
        }

        Ok(result)
    }

    fn extract_enum_variants(class: &mut decomped::Class) {
        for field in &class.fields {
            if !field.access_flags.enum_ {
                continue;
            }

            let Some(init) = &field.init_value else { continue };
            let decomped::Expression::New { args, .. } = init else { continue };

            let custom_args = if args.len() >= 2 {
                args[2..].to_vec()
            } else {
                Vec::new()
            };

            class.enum_variants.push(decomped::EnumVariant {
                name: field.name.clone(),
                args: custom_args,
            });
        }

        class.fields.retain(|f| !f.access_flags.enum_ && f.name.0 != "$VALUES");
        class.methods.retain(|m| m.name.0 != "values" && m.name.0 != "valueOf" && m.name.0 != "$values");
    }

    /// Finds and remove `PutField` expressions from `<clinit>` and `<init>`
    /// into the Field itself
    fn extract_field_initializers(class: &mut decomped::Class) -> anyhow::Result<()> {
        Self::extract_from_initializer(class, "<clinit>", true)?;
        Self::extract_from_initializer(class, "<init>", false)?;

        class.methods.retain(|m| {
            if m.name.0 != "<clinit>" && m.name.0 != "<init>" {
                return true;
            }
            !Self::is_empty_initializer(&m.code)
        });

        Ok(())
    }

    fn extract_from_initializer(class: &mut decomped::Class, method_name: &str, is_static: bool) -> anyhow::Result<()> {
        let Some(method) = class.methods.iter_mut().find(|m| m.name.0 == method_name) else {
            return Ok(());
        };

        let mut to_remove = Vec::new();

        for (idx, stmt) in method.code.iter_mut().enumerate() {
            let decomped::Statement::PutField {
                is_static: field_is_static,
                field,
                object,
                value,
            } = stmt else {
                continue;
            };

            if *field_is_static != is_static {
                continue;
            }

            if !is_static {
                let Some(obj) = object else { continue };
                let decomped::Expression::Load { index: 0, .. } = obj.as_ref() else {
                    continue;
                };
            }

            if field.class.descriptor.to_string() != class.name.to_string() {
                continue;
            }

            let Some(target_field) = class.fields.iter_mut().find(|f| f.name == field.name) else {
                continue;
            };

            if target_field.init_value.is_some() {
                continue;
            }

            // If the found expression uses a local, we can't extract it from the method
            let mut uses_local = UsesLocalExtractor { uses_a_local: false };
            uses_local.visit_expression(value)?;
            if uses_local.uses_a_local {
                continue;
            }

            target_field.init_value = Some(value.clone());
            to_remove.push(idx);
        }

        for idx in to_remove.into_iter().rev() {
            method.code.remove(idx);
        }

        Ok(())
    }

    fn is_empty_initializer(code: &[decomped::Statement]) -> bool {
        match code {
            [] => true,
            [decomped::Statement::Return { value: None }] => true,
            [decomped::Statement::Expression { expr }, decomped::Statement::Return { value: None }] => {
                Self::is_super_init_call(expr)
            }
            _ => false,
        }
    }

    fn is_super_init_call(expr: &decomped::Expression) -> bool {
        let decomped::Expression::Invoke { method, object: Some(obj), args, .. } = expr else {
            return false;
        };
        if method.name.0 != "<init>" {
            return false;
        }
        let decomped::Expression::Load { index: 0, .. } = obj.as_ref() else {
            return false;
        };
        args.is_empty()
    }
}

impl super::ExtractorKind for DecompClassExtractor {
    type Output = decomped::Class;

    fn name(&self) -> &'static str {
        "decomp_class_extractor"
    }

    async fn extract(self, manager: &mut super::ExtractionManager<'_>) -> anyhow::Result<decomped::Class> {
        let mapped_class = manager.extract(super::mapped_class::MappedClassExtractor {
            class: self.class,
            mappings_brand: self.mappings_brand,
        }).await?;

        crate::spawn_cpu_bound(move || Self::decomp(&mapped_class)).await?
    }
}
