use crate::minijvm::decomped::{Class, Method, Field, Statement, Expression};

pub trait MutVisitor {
    fn visit_class(&mut self, class: &mut Class) -> anyhow::Result<()> {
        walk_class(self, class)
    }

    fn visit_method(&mut self, method: &mut Method) -> anyhow::Result<()> {
        walk_method(self, method)
    }

    fn visit_field(&mut self, field: &mut Field) -> anyhow::Result<()> {
        walk_field(self, field)
    }

    fn visit_statement(&mut self, stmt: &mut Statement) -> anyhow::Result<()> {
        walk_statement(self, stmt)
    }

    fn visit_expression(&mut self, expr: &mut Expression) -> anyhow::Result<()> {
        walk_expression(self, expr)
    }
}

pub fn walk_class<V: MutVisitor + ?Sized>(v: &mut V, class: &mut Class) -> anyhow::Result<()> {
    for field in &mut class.fields {
        v.visit_field(field)?;
    }
    for method in &mut class.methods {
        v.visit_method(method)?;
    }
    Ok(())
}

pub fn walk_method<V: MutVisitor + ?Sized>(v: &mut V, method: &mut Method) -> anyhow::Result<()> {
    for stmt in &mut method.code {
        v.visit_statement(stmt)?;
    }
    Ok(())
}

pub fn walk_field<V: MutVisitor + ?Sized>(v: &mut V, field: &mut Field) -> anyhow::Result<()> {
    if let Some(expr) = &mut field.init_value {
        v.visit_expression(expr)?;
    }
    Ok(())
}

pub fn walk_statement<V: MutVisitor + ?Sized>(v: &mut V, stmt: &mut Statement) -> anyhow::Result<()> {
    match stmt {
        Statement::Expression { expr } => v.visit_expression(expr)?,
        Statement::Store { value, .. } => v.visit_expression(value)?,
        Statement::StoreTemp { value, .. } => v.visit_expression(value)?,
        Statement::PutField { object, value, .. } => {
            if let Some(obj) = object {
                v.visit_expression(obj)?;
            }
            v.visit_expression(value)?;
        }
        Statement::Return { value } => {
            if let Some((_, expr)) = value {
                v.visit_expression(expr)?;
            }
        }
        Statement::StoreIntoArray { array, index, value, .. } => {
            v.visit_expression(array)?;
            v.visit_expression(index)?;
            v.visit_expression(value)?;
        }
        Statement::If { condition, then_branch, else_branch } => {
            v.visit_expression(condition)?;
            for stmt in then_branch {
                v.visit_statement(stmt)?;
            }
            for stmt in else_branch {
                v.visit_statement(stmt)?;
            }
        }
        Statement::While { condition, body } => {
            v.visit_expression(condition)?;
            for stmt in body {
                v.visit_statement(stmt)?;
            }
        }
        Statement::Switch { value, cases, default } => {
            v.visit_expression(value)?;
            for case in cases {
                for stmt in &mut case.body {
                    v.visit_statement(stmt)?;
                }
            }
            for stmt in default {
                v.visit_statement(stmt)?;
            }
        }
    }

    Ok(())
}

pub fn walk_expression<V: MutVisitor + ?Sized>(v: &mut V, expr: &mut Expression) -> anyhow::Result<()> {
    match expr {
        Expression::Constant { .. }
        | Expression::Load { .. }
        | Expression::LoadTemp { .. } => {}

        Expression::New { args, .. } => {
            for arg in args {
                v.visit_expression(arg)?;
            }
        }

        Expression::BinOp { lhs, rhs, .. } => {
            v.visit_expression(lhs)?;
            v.visit_expression(rhs)?;
        }
        Expression::UnOp { operand, .. } => v.visit_expression(operand)?,
        Expression::Invoke { object, args, .. } => {
            if let Some(obj) = object {
                v.visit_expression(obj)?;
            }
            for arg in args {
                v.visit_expression(arg)?;
            }
        }
        Expression::InvokeDynamic { args, .. } => {
            for arg in args {
                v.visit_expression(arg)?;
            }
        }
        Expression::NewArray { count, .. } => v.visit_expression(count)?,
        Expression::Convert { value, .. } => v.visit_expression(value)?,
        Expression::GetField { object, .. } => {
            if let Some(obj) = object {
                v.visit_expression(obj)?;
            }
        }
        Expression::LoadFromArray { array, index, .. } => {
            v.visit_expression(array)?;
            v.visit_expression(index)?;
        }
        Expression::ArrayLength { array } => v.visit_expression(array)?,
        Expression::Cast { value, .. } => v.visit_expression(value)?,
        Expression::Compare { lhs, rhs, .. } => {
            v.visit_expression(lhs)?;
            v.visit_expression(rhs)?;
        }
        Expression::Ternary { condition, then_value, else_value } => {
            v.visit_expression(condition)?;
            v.visit_expression(then_value)?;
            v.visit_expression(else_value)?;
        }
        Expression::Lambda { captures, .. } => {
            for c in captures {
                v.visit_expression(c)?;
            }
        }
        Expression::Switch { value, cases, default } => {
            v.visit_expression(value)?;
            for case in cases {
                v.visit_expression(&mut case.value)?;
            }
            v.visit_expression(default)?;
        }
        Expression::Throw { value } => v.visit_expression(value)?,
    }

    Ok(())
}
