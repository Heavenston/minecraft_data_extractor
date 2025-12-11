pub mod visitor;
use super::{ Ident, IdentPath, TypeDescriptor, TypeDescriptorKind, MethodDescriptor, AccessFlags };

use std::fmt::Write;

pub struct PrintContext {
    is_static: bool,
    arg_slots: Vec<u16>,
}

impl PrintContext {
    pub fn new(is_static: bool, args: &[TypeDescriptor]) -> Self {
        let mut arg_slots = Vec::new();
        let mut slot = if is_static { 0 } else { 1 };
        for arg in args {
            arg_slots.push(slot);
            slot += match arg.ty {
                TypeDescriptorKind::Long | TypeDescriptorKind::Double => 2,
                _ => 1,
            };
        }
        Self { is_static, arg_slots }
    }

    pub fn local_name(&self, index: u16) -> String {
        if !self.is_static && index == 0 {
            return "this".to_string();
        }
        if let Some(arg_idx) = self.arg_slots.iter().position(|&s| s == index) {
            return format!("arg{arg_idx}");
        }
        format!("local_{index}")
    }
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub enum Constant {
    Byte(i8),
    Short(i16),
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    String(String),
    Class(super::ClassRef),
    MethodHandle(super::MethodHandle),
    MethodType(MethodDescriptor),
    Null,
}

impl Constant {
    pub fn printed(&self) -> String {
        match self {
            Constant::Byte(v) => format!("(byte){v}"),
            Constant::Short(v) => format!("(short){v}"),
            Constant::Int(v) => v.to_string(),
            Constant::Long(v) => format!("{v}L"),
            Constant::Float(v) => format!("{v}f"),
            Constant::Double(v) => v.to_string(),
            Constant::String(s) => format!("{s:?}"),
            Constant::Class(c) => format!("{}.class", c.descriptor),
            Constant::MethodHandle(h) => match &h.reference {
                super::MethodHandleRef::Method(m) => format!("{}::{}", m.class.descriptor, m.name.0),
                super::MethodHandleRef::Field(f) => format!("{}::{}", f.class.descriptor, f.name.0),
            },
            Constant::MethodType(d) => format!("{d}"),
            Constant::Null => "null".to_string(),
        }
    }
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub enum Expression {
    Constant {
        value: Constant,
    },
    Load {
        value_kind: super::ValueKind,
        index: u16,
    },
    LoadTemp {
        index: u16,
    },
    BinOp {
        op: super::BinOp,
        value_kind: super::ValueKind,

        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    UnOp {
        op: super::UnOp,
        value_kind: super::ValueKind,
        operand: Box<Expression>,
    },
    Invoke {
        kind: super::InvokeKind,
        method: super::MethodRef,
        object: Option<Box<Expression>>,
        args: Vec<Expression>,
    },
    InvokeDynamic {
        call_site: super::DynamicCallSite,
        name: Ident,
        descriptor: super::MethodDescriptor,
        args: Vec<Expression>,
    },

    New {
        class: super::ClassRef,
        args: Vec<Expression>,
    },
    NewArray {
        kind: super::ValueKind,
        count: Box<Expression>,
    },

    Convert {
        from: super::ValueKind,
        to: super::ValueKind,
        value: Box<Expression>,
    },

    GetField {
        is_static: bool,
        field: super::FieldRef,
        object: Option<Box<Expression>>,
    },

    LoadFromArray {
        kind: super::ValueKind,
        array: Box<Expression>,
        index: Box<Expression>,
    },

    ArrayLength {
        array: Box<Expression>,
    },

    Cast {
        class: super::ClassRef,
        value: Box<Expression>,
    },

    /// Comparison used in `if` and loop conditions.
    ///
    /// This represents a boolean expression of the form
    /// `lhs <cmp> rhs`.
    Compare {
        cmp: super::IfCmp,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },

    Ternary {
        condition: Box<Expression>,
        then_value: Box<Expression>,
        else_value: Box<Expression>,
    },

    Lambda {
        target: super::MethodRef,
        interface_method: Ident,
        captures: Vec<Expression>,
    },
}

impl Expression {
    pub fn printed(&self, ctx: &PrintContext) -> String {
        self.printed_prec(ctx, 0)
    }

    fn printed_prec(&self, ctx: &PrintContext, parent_prec: u8) -> String {
        let (prec, s) = self.printed_inner(ctx);
        if prec < parent_prec {
            format!("({s})")
        } else {
            s
        }
    }

    fn printed_inner(&self, ctx: &PrintContext) -> (u8, String) {
        match self {
            Expression::Constant { value } => (100, value.printed()),
            Expression::Load { index, .. } => (100, ctx.local_name(*index)),
            Expression::LoadTemp { index } => (100, format!("temp_{index}")),
            Expression::BinOp { op, lhs, rhs, .. } => {
                let (prec, op_str) = op.printed_with_prec();
                (prec, format!("{} {op_str} {}", lhs.printed_prec(ctx, prec), rhs.printed_prec(ctx, prec + 1)))
            }
            Expression::UnOp { op, operand, .. } => {
                let op_str = op.printed();
                (90, format!("{op_str}{}", operand.printed_prec(ctx, 90)))
            }
            Expression::Invoke { method, object, args, .. } => {
                let args_str = args.iter().map(|a| a.printed(ctx)).collect::<Vec<_>>().join(", ");
                let s = match object {
                    Some(obj) => format!("{}.{}({args_str})", obj.printed_prec(ctx, 100), method.name.0),
                    None => format!("{}.{}({args_str})", method.class.descriptor, method.name.0),
                };
                (100, s)
            }
            Expression::InvokeDynamic { name, args, .. } => {
                let args_str = args.iter().map(|a| a.printed(ctx)).collect::<Vec<_>>().join(", ");
                (100, format!("{name}({args_str})"))
            }
            Expression::New { class, args } => {
                let args_str = args.iter().map(|a| a.printed(ctx)).collect::<Vec<_>>().join(", ");
                (100, format!("new {}({args_str})", class.descriptor))
            }
            Expression::NewArray { kind, count } => {
                (100, format!("new {}[{}]", kind.printed(), count.printed(ctx)))
            }
            Expression::Convert { to, value, .. } => {
                (90, format!("({}){}", to.printed(), value.printed_prec(ctx, 90)))
            }
            Expression::GetField { is_static, field, object } => {
                let target = if *is_static {
                    field.class.descriptor.to_string()
                } else {
                    object.as_ref().map(|o| o.printed_prec(ctx, 100)).unwrap_or_else(|| "this".to_string())
                };
                (100, format!("{target}.{}", field.name.0))
            }
            Expression::LoadFromArray { array, index, .. } => {
                (100, format!("{}[{}]", array.printed_prec(ctx, 100), index.printed(ctx)))
            }
            Expression::ArrayLength { array } => {
                (100, format!("{}.length", array.printed_prec(ctx, 100)))
            }
            Expression::Cast { class, value } => {
                (90, format!("({}){}", class.descriptor, value.printed_prec(ctx, 90)))
            }
            Expression::Compare { cmp, lhs, rhs } => {
                let op_str = cmp.printed();
                (30, format!("{} {op_str} {}", lhs.printed_prec(ctx, 30), rhs.printed_prec(ctx, 31)))
            }
            Expression::Ternary { condition, then_value, else_value } => {
                (10, format!("{} ? {} : {}", condition.printed_prec(ctx, 10), then_value.printed_prec(ctx, 10), else_value.printed_prec(ctx, 10)))
            }
            Expression::Lambda { target, interface_method: _, captures } => {
                if captures.is_empty() {
                    (100, format!("{}::{}", target.class.descriptor, target.name.0))
                } else {
                    let captures_str = captures.iter().map(|c| c.printed(ctx)).collect::<Vec<_>>().join(", ");
                    (100, format!("[{}]{}::{}", captures_str, target.class.descriptor, target.name.0))
                }
            }
        }
    }
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub enum Statement {
    Expression {
        expr: Expression,
    },
    Store {
        value_kind: super::ValueKind,
        index: u16,
        value: Expression,
    },
    StoreTemp {
        index: u16,
        value: Expression,
    },
    PutField {
        is_static: bool,
        field: super::FieldRef,
        object: Option<Box<Expression>>,
        value: Expression,
    },
    Return {
        value: Option<(super::ValueKind, Expression)>,
    },
    Throw {
        value: Expression,
    },

    StoreIntoArray {
        kind: super::ValueKind,
        array: Expression,
        index: Expression,
        value: Expression,
    },

    /// Structured `if` statement with optional `else` branch.
    If {
        condition: Expression,
        then_branch: Vec<Statement>,
        else_branch: Vec<Statement>,
    },

    /// Structured `while` loop.
    ///
    /// Represents:
    /// `while (condition) { body }`
    While {
        condition: Expression,
        body: Vec<Statement>,
    },
}

impl Statement {
    pub fn printed(&self, ctx: &PrintContext) -> String {
        self.printed_indent(ctx, 0)
    }

    fn printed_indent(&self, ctx: &PrintContext, indent: usize) -> String {
        let ind = "    ".repeat(indent);
        match self {
            Statement::Expression { expr } => format!("{ind}{};", expr.printed(ctx)),
            Statement::Store { index, value, .. } => {
                format!("{ind}{} = {};", ctx.local_name(*index), value.printed(ctx))
            }
            Statement::StoreTemp { index, value } => {
                format!("{ind}temp_{index} = {};", value.printed(ctx))
            }
            Statement::PutField { is_static, field, object, value } => {
                let target = if *is_static {
                    format!("{}.{}", field.class.descriptor, field.name.0)
                } else {
                    match object {
                        Some(obj) => format!("{}.{}", obj.printed(ctx), field.name.0),
                        None => format!("this.{}", field.name.0),
                    }
                };
                format!("{ind}{target} = {};", value.printed(ctx))
            }
            Statement::Return { value } => match value {
                Some((_, expr)) => format!("{ind}return {};", expr.printed(ctx)),
                None => format!("{ind}return;"),
            },
            Statement::Throw { value } => format!("{ind}throw {};", value.printed(ctx)),
            Statement::StoreIntoArray { array, index, value, .. } => {
                format!("{ind}{}[{}] = {};", array.printed(ctx), index.printed(ctx), value.printed(ctx))
            }
            Statement::If { condition, then_branch, else_branch } => {
                let mut s = format!("{ind}if ({}) {{\n", condition.printed(ctx));
                for stmt in then_branch {
                    let _ = writeln!(s, "{}", stmt.printed_indent(ctx, indent + 1));
                }
                if else_branch.is_empty() {
                    write!(s, "{ind}}}").unwrap();
                } else {
                    writeln!(s, "{ind}}} else {{").unwrap();
                    for stmt in else_branch {
                        let _ = writeln!(s, "{}", stmt.printed_indent(ctx, indent + 1));
                    }
                    write!(s, "{ind}}}").unwrap();
                }
                s
            }
            Statement::While { condition, body } => {
                let mut s = format!("{ind}while ({}) {{\n", condition.printed(ctx));
                for stmt in body {
                    let _ = writeln!(s, "{}", stmt.printed_indent(ctx, indent + 1));
                }
                write!(s, "{ind}}}").unwrap();
                s
            }
        }
    }
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct Field {
    pub name: Ident,
    pub descriptor: TypeDescriptor,
    pub access_flags: AccessFlags,

    pub init_value: Option<Expression>,
}

impl Field {
    pub fn printed(&self) -> String {
        let modifiers = self.access_flags.printed();
        let ctx = PrintContext::new(true, &[]);
        let init = self.init_value.as_ref()
            .map(|v| format!(" = {}", v.printed(&ctx)))
            .unwrap_or_default();
        format!("{modifiers}{} {}{init};", self.descriptor, self.name.0)
    }
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct Method {
    pub name: Ident,
    pub descriptor: MethodDescriptor,
    pub access_flags: AccessFlags,
    pub code: Vec<Statement>,
}

impl Method {
    pub fn printed(&self, class_name: &str) -> String {
        let ctx = PrintContext::new(self.access_flags.static_, &self.descriptor.args);
        let modifiers = self.access_flags.printed();
        let args = self.descriptor.args.iter()
            .enumerate()
            .map(|(i, ty)| format!("{ty} arg{i}"))
            .collect::<Vec<_>>()
            .join(", ");

        let mut s = match self.name.0.as_str() {
            "<clinit>" => "static {\n".to_string(),
            "<init>" => format!("{modifiers}{class_name}({args}) {{\n"),
            _ => format!("{modifiers}{} {}({args}) {{\n", self.descriptor.return_type, self.name.0),
        };

        for stmt in &self.code {
            let _ = writeln!(s, "{}", stmt.printed_indent(&ctx, 1));
        }
        s.push('}');
        s
    }
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct EnumVariant {
    pub name: Ident,
    pub args: Vec<Expression>,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct Class {
    pub name: IdentPath,
    pub super_class: Option<IdentPath>,
    pub enum_variants: Vec<EnumVariant>,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
}

impl Class {
    pub fn is_enum(&self) -> bool {
        !self.enum_variants.is_empty()
    }

    pub fn printed(&self) -> String {
        let simple_name = self.name.0.rsplit('.').next().unwrap_or(&self.name.0);
        let ctx = PrintContext::new(true, &[]);

        if self.is_enum() {
            let mut s = format!("enum {simple_name} {{\n");

            for (i, variant) in self.enum_variants.iter().enumerate() {
                let args_str = if variant.args.is_empty() {
                    String::new()
                } else {
                    let args = variant.args.iter().map(|a| a.printed(&ctx)).collect::<Vec<_>>().join(", ");
                    format!("({args})")
                };
                let suffix = if i + 1 < self.enum_variants.len() { "," } else { ";" };
                let _ = writeln!(s, "    {}{args_str}{suffix}", variant.name.0);
            }

            if !self.fields.is_empty() {
                s.push('\n');
                for field in &self.fields {
                    let _ = writeln!(s, "    {}", field.printed());
                }
            }

            if !self.methods.is_empty() {
                s.push('\n');
                for method in &self.methods {
                    for line in method.printed(simple_name).lines() {
                        let _ = writeln!(s, "    {line}");
                    }
                    s.push('\n');
                }
            }

            s.push('}');
            s
        } else {
            let extends = self.super_class.as_ref()
                .filter(|s| s.0 != "java.lang.Object")
                .map(|s| format!(" extends {}", s.0))
                .unwrap_or_default();
            let mut s = format!("class {simple_name}{extends} {{\n");
            for field in &self.fields {
                let _ = writeln!(s, "    {}", field.printed());
            }
            if !self.fields.is_empty() && !self.methods.is_empty() {
                s.push('\n');
            }
            for method in &self.methods {
                for line in method.printed(simple_name).lines() {
                    let _ = writeln!(s, "    {line}");
                }
                s.push('\n');
            }
            s.push('}');
            s
        }
    }
}
