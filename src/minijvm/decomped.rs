pub mod visitor;

use super::{ Ident, IdentPath, AccessFlags, JavaTypeSignature, MethodSignature };
use super::signatures;

use std::{ borrow::Cow, fmt::Write, ops::Deref };
use convert_case::ccase;

pub struct ClassPrintContext<'a> {
    pub class: &'a Class,
}

impl<'a> ClassPrintContext<'a> {
    pub fn new(class: &'a Class) -> Self {
        Self { class }
    }
}

pub struct MethodPrintContext<'a> {
    pub parent: &'a ClassPrintContext<'a>,
    pub is_static: bool,
    pub arg_slots: Vec<u16>,
    pub arg_names: Vec<String>,
}

impl<'a> Deref for MethodPrintContext<'a> {
    type Target = ClassPrintContext<'a>;

    fn deref(&self) -> &Self::Target {
        self.parent
    }
}

impl<'a> MethodPrintContext<'a> {
    pub fn new(parent: &'a ClassPrintContext<'a>, is_static: bool, args: &[JavaTypeSignature]) -> Self {
        let mut arg_slots = Vec::new();
        let mut arg_names = Vec::new();
        let mut slot = if is_static { 0 } else { 1 };
        for (arg_idx, arg) in args.iter().enumerate() {
            arg_slots.push(slot);
            slot += match arg {
                JavaTypeSignature::Base(signatures::BaseTypeSignature::Long | signatures::BaseTypeSignature::Double) => 2,
                _ => 1,
            };

            let mut pretty_name = match arg {
                JavaTypeSignature::Reference(signatures::ReferenceTypeSignature::Class(c)) => {
                    let last = c.suffix.last().unwrap_or(&c.class);
                    ccase!(snake, last.name.to_string())
                }
                JavaTypeSignature::Reference(signatures::ReferenceTypeSignature::TypeVariable(v)) => format!("{}0", v.name),
                JavaTypeSignature::Reference(signatures::ReferenceTypeSignature::Array(_)) => format!("arg{arg_idx}"),
                JavaTypeSignature::Base(k) => format!("{k}0"),
            };

            let mut i = 0;
            while arg_names.contains(&pretty_name) {
                i += 1;
                pretty_name = format!("{}{i}", pretty_name.trim_end_matches(char::is_numeric));
            }

            arg_names.push(pretty_name);
        }
        Self { parent, is_static, arg_slots, arg_names }
    }

    pub fn arg_name(&self, idx: usize) -> Cow<'_, str> {
        return Cow::Borrowed(&self.arg_names[idx]);
    }

    pub fn local_name(&self, index: u16) -> Cow<'_, str> {
        if !self.is_static && index == 0 {
            return Cow::Borrowed("this");
        }
        if let Some(arg_idx) = self.arg_slots.iter().position(|&s| s == index) {
            return self.arg_name(arg_idx);
        }
        Cow::Owned(format!("local_{index}"))
    }
}

fn format_type_argument(arg: &signatures::TypeArgument) -> String {
    match arg {
        signatures::TypeArgument::Wildcard => "?".to_string(),
        signatures::TypeArgument::Type { wildcard_indicator, ty } => match wildcard_indicator {
            Some(signatures::WildcardIndicator::Plus) => format!("? extends {}", format_reference_type_signature(ty)),
            Some(signatures::WildcardIndicator::Minus) => format!("? super {}", format_reference_type_signature(ty)),
            None => format_reference_type_signature(ty),
        },
    }
}

fn format_simple_class_type_signature(sig: &signatures::SimpleClassTypeSignature) -> String {
    let mut s = sig.name.to_string();
    if !sig.type_arguments.is_empty() {
        let args = sig.type_arguments.iter()
            .map(format_type_argument)
            .collect::<Vec<_>>()
            .join(", ");
        s.push('<');
        s.push_str(&args);
        s.push('>');
    }
    s
}

fn format_class_type_signature(sig: &signatures::ClassTypeSignature) -> String {
    let mut s = String::new();
    if !sig.package.is_empty() {
        s.push_str(&sig.package.iter().map(|p| p.to_string()).collect::<Vec<_>>().join("."));
        s.push('.');
    }
    s.push_str(&format_simple_class_type_signature(&sig.class));
    for suffix in &sig.suffix {
        s.push('.');
        s.push_str(&format_simple_class_type_signature(suffix));
    }
    s
}

fn format_reference_type_signature(sig: &signatures::ReferenceTypeSignature) -> String {
    match sig {
        signatures::ReferenceTypeSignature::Class(c) => format_class_type_signature(c),
        signatures::ReferenceTypeSignature::TypeVariable(v) => v.name.to_string(),
        signatures::ReferenceTypeSignature::Array(a) => format!("{}[]", format_java_type_signature(&a.ty)),
    }
}

fn format_java_type_signature(sig: &JavaTypeSignature) -> String {
    match sig {
        JavaTypeSignature::Reference(r) => format_reference_type_signature(r),
        JavaTypeSignature::Base(b) => b.to_string(),
    }
}

fn format_type_parameter(param: &signatures::TypeParameter) -> String {
    let mut bounds = Vec::new();
    if let Some(class_bound) = &param.class_bound {
        bounds.push(format_reference_type_signature(class_bound));
    }
    bounds.extend(param.interface_bounds.iter().map(format_reference_type_signature));

    if bounds.is_empty() {
        param.name.to_string()
    }
    else {
        format!("{} extends {}", param.name, bounds.join(" & "))
    }
}

fn format_type_parameters(type_parameters: &[signatures::TypeParameter]) -> String {
    if type_parameters.is_empty() {
        String::new()
    }
    else {
        format!("<{}> ", type_parameters.iter().map(format_type_parameter).collect::<Vec<_>>().join(", "))
    }
}

fn format_method_result(result: &signatures::MethodResult) -> String {
    match result {
        signatures::MethodResult::Type(ty) => format_java_type_signature(ty),
        signatures::MethodResult::Void => "void".to_string(),
    }
}

fn format_throws_signature(sig: &signatures::ThrowsSignature) -> String {
    match sig {
        signatures::ThrowsSignature::Class(c) => format_class_type_signature(c),
        signatures::ThrowsSignature::TypeVariable(tv) => tv.name.to_string(),
    }
}

fn format_throws_clause(throws: &[signatures::ThrowsSignature]) -> String {
    if throws.is_empty() {
        String::new()
    }
    else {
        format!(" throws {}", throws.iter().map(format_throws_signature).collect::<Vec<_>>().join(", "))
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
    MethodType(MethodSignature),
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
            Constant::MethodType(d) => {
                let args = d.arguments.iter().map(format_java_type_signature).collect::<Vec<_>>().join(", ");
                let return_type = format_method_result(&d.result);
                format!("{return_type} ({args})")
            }
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

    InstanceOf {
        class: super::ClassRef,
        object: Box<Expression>,
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
        signature: MethodSignature,
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

    Switch {
        value: Box<Expression>,
        cases: Vec<SwitchExprCase>,
        default: Box<Expression>,
    },
    Throw {
        value: Box<Expression>,
    },
}

impl Expression {
    pub fn printed(&self, ctx: &MethodPrintContext) -> String {
        self.printed_prec(ctx, 0)
    }

    fn printed_prec(&self, ctx: &MethodPrintContext, parent_prec: u8) -> String {
        let (prec, s) = self.printed_inner(ctx);
        if prec < parent_prec {
            format!("({s})")
        } else {
            s
        }
    }

    fn printed_inner(&self, ctx: &MethodPrintContext) -> (u8, String) {
        match self {
            Expression::Constant { value } => (100, value.printed()),
            Expression::Load { index, .. } => (100, ctx.local_name(*index).to_string()),
            Expression::LoadTemp { index } => (100, format!("temp_{index}")),
            Expression::BinOp { op, lhs, rhs, .. } => {
                let (prec, op_str) = op.printed_with_prec();
                (prec, format!("{} {op_str} {}", lhs.printed_prec(ctx, prec), rhs.printed_prec(ctx, prec + 1)))
            }
            Expression::UnOp { op, operand, .. } => {
                let op_str = op.printed();
                (90, format!("{op_str}{}", operand.printed_prec(ctx, 90)))
            }
            Expression::InstanceOf { class, object } => {
                (0, format!("{} instanceof {}", object.printed(ctx), class.descriptor))
            }
            Expression::Invoke { kind, method, object, args } => {
                let args_str = args.iter().map(|a| a.printed(ctx)).collect::<Vec<_>>().join(", ");

                // I dont know right now how it works for interfaces so this is
                // probably wrong for this cases, also not sure when going multiple
                // classes up the hierarchy
                let is_super_call = *kind == super::InvokeKind::Special
                    && ctx.class.super_class.as_ref().zip(method.class.descriptor.simple_class_name()).is_some_and(|(a, b)| a == b)
                ;

                let s = if method.name.0 == "<init>" {
                    if is_super_call {
                        format!("super({args_str})")
                    }
                    else {
                        format!("this({args_str})")
                    }
                } else if is_super_call {
                    format!("super.{}({args_str})", method.name.0)
                } else {
                    match object {
                        Some(obj) => format!("{}.{}({args_str})", obj.printed_prec(ctx, 100), method.name.0),
                        None => format!("{}.{}({args_str})", method.class.descriptor, method.name.0),
                    }
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
            Expression::Switch { value, cases, default } => {
                let mut s = format!("switch ({}) {{ ", value.printed(ctx));
                for case in cases {
                    let values = case.values.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(", ");
                    s.push_str(&format!("case {values} -> {}; ", case.value.printed(ctx)));
                }
                s.push_str(&format!("default -> {} ", default.printed(ctx)));
                s.push('}');
                (100, s)
            }
            Expression::Throw { value } => (5, format!("throw {}", value.printed_prec(ctx, 5))),
        }
    }
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct SwitchCase {
    pub values: Vec<i32>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct SwitchExprCase {
    pub values: Vec<i32>,
    pub value: Expression,
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

    Switch {
        value: Expression,
        cases: Vec<SwitchCase>,
        default: Vec<Statement>,
    },
}

impl Statement {
    #[expect(dead_code)]
    pub fn printed(&self, ctx: &MethodPrintContext) -> String {
        self.printed_indent(ctx, 0)
    }

    fn printed_indent(&self, ctx: &MethodPrintContext, indent: usize) -> String {
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
            Statement::Switch { value, cases, default } => {
                let mut s = format!("{ind}switch ({}) {{\n", value.printed(ctx));
                for case in cases {
                    for value in &case.values {
                        let _ = writeln!(s, "{ind}    case {value}:");
                    }
                    for stmt in &case.body {
                        let _ = writeln!(s, "{}", stmt.printed_indent(ctx, indent + 2));
                    }
                }
                if !default.is_empty() {
                    let _ = writeln!(s, "{ind}    default:");
                    for stmt in default {
                        let _ = writeln!(s, "{}", stmt.printed_indent(ctx, indent + 2));
                    }
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
    pub signature: JavaTypeSignature,
    pub access_flags: AccessFlags,

    pub init_value: Option<Expression>,
}

impl Field {
    pub fn printed(&self, ctx: &ClassPrintContext) -> String {
        let modifiers = self.access_flags.printed();
        let ty = format_java_type_signature(&self.signature);
        let init = self.init_value.as_ref()
            .map(|v| format!(" = {}", v.printed(&MethodPrintContext::new(ctx, self.access_flags.static_, &[]))))
            .unwrap_or_default();
        format!("{modifiers}{ty} {}{init};", self.name.0)
    }
}

#[derive(Debug, Clone, bincode::Encode, bincode::Decode)]
pub struct Method {
    pub name: Ident,
    pub signature: MethodSignature,
    pub access_flags: AccessFlags,
    pub code: Option<Vec<Statement>>,
}

impl Method {
    pub fn printed(&self, ctx: &ClassPrintContext) -> String {
        let ctx = MethodPrintContext::new(ctx, self.access_flags.static_, &self.signature.arguments);
        let modifiers = self.access_flags.printed();
        let class_name = ctx.class.name.last_name();

        let type_parameters = format_type_parameters(&self.signature.type_parameters);
        let args = self.signature.arguments.iter()
            .enumerate()
            .map(|(i, ty)| format!("{} {}", format_java_type_signature(ty), ctx.arg_name(i)))
            .collect::<Vec<_>>()
            .join(", ");
        let throws = format_throws_clause(&self.signature.throws_signature);

        let mut s = match self.name.0.as_str() {
            "<clinit>" => "static {\n".to_string(),
            "<init>" => format!("{modifiers}{type_parameters}{class_name}({args}){throws}"),
            _ => {
                let return_type = format_method_result(&self.signature.result);
                format!("{modifiers}{type_parameters}{return_type} {}({args}){throws}", self.name.0)
            }
        };

        if let Some(code) = &self.code {
            let _ = write!(s, " {{\n");
            for stmt in code {
                let _ = writeln!(s, "{}", stmt.printed_indent(&ctx, 1));
            }
            s.push('}');
        }
        else {
            let _ = write!(s, ";\n");
        }

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
    pub access_flags: AccessFlags,
    pub super_class: Option<IdentPath>,
    pub enum_variants: Vec<EnumVariant>,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
}

impl Class {
    pub fn is_enum(&self) -> bool {
        self.access_flags.enum_
    }

    pub fn printed(&self) -> String {
        let simple_name = self.name.last_name();
        let ctx = ClassPrintContext::new(self);

        if self.is_enum() {
            let mut s = format!("enum {simple_name} {{\n");

            for (i, variant) in self.enum_variants.iter().enumerate() {
                let args_str = if variant.args.is_empty() {
                    String::new()
                } else {
                    let args = variant.args.iter().map(|a| a.printed(&MethodPrintContext::new(&ctx, true, &[]))).collect::<Vec<_>>().join(", ");
                    format!("({args})")
                };
                let suffix = if i + 1 < self.enum_variants.len() { "," } else { ";" };
                let _ = writeln!(s, "    {}{args_str}{suffix}", variant.name.0);
            }

            if !self.fields.is_empty() {
                s.push('\n');
                for field in &self.fields {
                    let _ = writeln!(s, "    {}", field.printed(&ctx));
                }
            }

            if !self.methods.is_empty() {
                s.push('\n');
                for method in &self.methods {
                    for line in method.printed(&ctx).lines() {
                        let _ = writeln!(s, "    {line}");
                    }
                    s.push('\n');
                }
            }

            s.push('}');
            s
        } else {
            let extends = self.super_class.as_ref()
                .filter(|&s| s != "java.lang.Object")
                .map(|s| format!(" extends {}", s))
                .unwrap_or_default();
            let mut s = format!("class {simple_name}{extends} {{\n");
            for field in &self.fields {
                let _ = writeln!(s, "    {}", field.printed(&ctx));
            }
            if !self.fields.is_empty() && !self.methods.is_empty() {
                s.push('\n');
            }
            for method in &self.methods {
                for line in method.printed(&ctx).lines() {
                    let _ = writeln!(s, "    {line}");
                }
                s.push('\n');
            }
            s.push('}');
            s
        }
    }
}
