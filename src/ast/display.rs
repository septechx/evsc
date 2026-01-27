use std::fmt::Write;

use colored::Colorize;

use crate::ast::{
    Expr, ExprKind, ImportTree, ImportTreeKind, Literal, Mutability, Stmt, StmtKind, Type,
    TypeKind, Visibility, statements::*,
};

#[derive(Debug, Clone, Copy)]
pub struct DisplayContext {
    color: bool,
    indent: usize,
}

impl DisplayContext {
    pub fn new(color: bool) -> Self {
        Self { color, indent: 0 }
    }

    fn indented(&self) -> Self {
        Self {
            color: self.color,
            indent: self.indent + 1,
        }
    }

    fn indent_str(&self) -> String {
        "  ".repeat(self.indent)
    }
}

trait WithColor {
    fn with_color(&self, color: bool) -> String;
}

impl WithColor for str {
    fn with_color(&self, color: bool) -> String {
        if color {
            self.cyan().to_string()
        } else {
            self.to_string()
        }
    }
}

fn type_with_color(s: &str, color: bool) -> String {
    if color {
        s.magenta().to_string()
    } else {
        s.to_string()
    }
}

fn modifiers_with_color(s: &str, color: bool) -> String {
    if color {
        s.blue().to_string()
    } else {
        s.to_string()
    }
}

fn format_modifiers(modifiers: &[&str]) -> String {
    if modifiers.is_empty() {
        String::new()
    } else {
        format!("{} ", modifiers.join(" "))
    }
}

fn number_with_color(s: &str, color: bool) -> String {
    if color {
        s.green().to_string()
    } else {
        s.to_string()
    }
}

fn punct_with_color(s: &str, color: bool) -> String {
    if color {
        s.white().to_string()
    } else {
        s.to_string()
    }
}

fn write_expr_inline_or_nested(
    out: &mut String,
    label: &str,
    expr: &Expr,
    ctx: &DisplayContext,
) -> std::fmt::Result {
    write!(out, "{}{}", ctx.indent_str(), label)?;
    if expr.kind.is_leaf() {
        write_expr(out, expr, &ctx.clone())?;
    } else {
        writeln!(out)?;
        let child_ctx = ctx.indented();
        write!(out, "{}", child_ctx.indent_str())?;
        write_expr(out, expr, &child_ctx)?;
    }
    Ok(())
}

fn write_expr_inline_or_indented(
    out: &mut String,
    expr: &Expr,
    ctx: &DisplayContext,
) -> std::fmt::Result {
    if expr.kind.is_leaf() {
        write_expr(out, expr, ctx)?;
    } else {
        writeln!(out)?;
        let child_ctx = ctx.indented();
        write!(out, "{}", child_ctx.indent_str())?;
        write_expr(out, expr, &child_ctx)?;
    }
    Ok(())
}

fn string_with_color(s: &str, color: bool) -> String {
    let escaped = escape_string(s);
    if color {
        escaped.yellow().to_string()
    } else {
        escaped
    }
}

fn escape_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len() + 2);
    result.push('"');
    for c in s.chars() {
        match c {
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            '"' => result.push_str("\\\""),
            '\\' => result.push_str("\\\\"),
            c if c.is_control() => {
                result.push_str(&format!("\\u{{{:04x}}}", c as u32));
            }
            c => result.push(c),
        }
    }
    result.push('"');
    result
}

pub fn write_stmt(out: &mut String, stmt: &Stmt, ctx: &DisplayContext) -> std::fmt::Result {
    match &stmt.kind {
        StmtKind::Expr(expr_stmt) => {
            write!(out, "{}:", "ExpressionStmt".with_color(ctx.color),)?;
            if expr_stmt.expr.kind.is_leaf() {
                write!(out, " ")?;
                write_expr(out, &expr_stmt.expr, &ctx.clone())?;
            } else {
                writeln!(out)?;
                let expr_ctx = ctx.indented();
                write!(out, "{}", expr_ctx.indent_str())?;
                write_expr(out, &expr_stmt.expr, &expr_ctx)?;
            }
        }
        StmtKind::Semi(semi_stmt) => {
            write!(out, "{}:", "SemiStmt".with_color(ctx.color),)?;
            if semi_stmt.expr.kind.is_leaf() {
                write!(out, " ")?;
                write_expr(out, &semi_stmt.expr, &ctx.clone())?;
            } else {
                writeln!(out)?;
                let expr_ctx = ctx.indented();
                write!(out, "{}", expr_ctx.indent_str())?;
                write_expr(out, &semi_stmt.expr, &expr_ctx)?;
            }
        }
        StmtKind::VarDecl(var_decl) => {
            let mut modifiers = Vec::new();
            if var_decl.visibility == Visibility::Public {
                modifiers.push("pub");
            }
            if var_decl.mutability == Mutability::Mutable {
                modifiers.push("mut");
            }
            if var_decl.is_static {
                modifiers.push("static");
            }
            let modifiers = format_modifiers(&modifiers);
            write!(
                out,
                "{} {}{}{}{}: ",
                "VarDecl".with_color(ctx.color),
                modifiers_with_color(&modifiers, ctx.color),
                punct_with_color("\"", ctx.color),
                var_decl.variable_name.value,
                punct_with_color("\"", ctx.color)
            )?;
            write!(out, "{}", write_type(&var_decl.ty, ctx))?;
            if let Some(value) = &var_decl.assigned_value {
                write!(out, " =")?;
                if value.kind.is_leaf() {
                    write!(out, " ")?;
                    write_expr(out, value, ctx)?;
                } else {
                    writeln!(out)?;
                    let value_ctx = ctx.indented();
                    write!(out, "{}", value_ctx.indent_str())?;
                    write_expr(out, value, &value_ctx)?;
                }
            } else {
                write!(out, " (uninitialized)")?;
            }
        }
        StmtKind::StructDecl(struct_decl) => {
            let mut modifiers = Vec::new();
            if struct_decl.visibility == Visibility::Public {
                modifiers.push("pub");
            }
            let modifiers = format_modifiers(&modifiers);
            write!(
                out,
                "{} {}{}{}",
                "StructDecl".with_color(ctx.color),
                modifiers_with_color(&modifiers, ctx.color),
                punct_with_color("\"", ctx.color),
                struct_decl.name.value
            )?;
            write!(out, "{}", punct_with_color("\"", ctx.color))?;
            if struct_decl.fields.is_empty() && struct_decl.methods.is_empty() {
                write!(out, ": (empty)")?;
            } else {
                writeln!(out, ":")?;
                let body_ctx = ctx.indented();
                let mut idx = 0;
                for prop in &struct_decl.fields {
                    write!(out, "{}", body_ctx.indent_str())?;
                    write_struct_property(out, prop, &body_ctx)?;
                    if idx > 0 {
                        writeln!(out)?;
                    }
                    idx += 1;
                }
                for method in &struct_decl.methods {
                    if idx > 0 {
                        writeln!(out)?;
                    }
                    write!(out, "{}", body_ctx.indent_str())?;
                    write_struct_method(out, method, &body_ctx)?;
                    idx += 1;
                }
            }
        }
        StmtKind::InterfaceDecl(interface_decl) => {
            let mut modifiers = Vec::new();
            if interface_decl.visibility == Visibility::Public {
                modifiers.push("pub");
            }
            let modifiers = format_modifiers(&modifiers);
            write!(
                out,
                "{} {}{}{}",
                "InterfaceDecl".with_color(ctx.color),
                modifiers_with_color(&modifiers, ctx.color),
                punct_with_color("\"", ctx.color),
                interface_decl.name.value
            )?;
            write!(out, "{}", punct_with_color("\"", ctx.color))?;
            if interface_decl.methods.is_empty() {
                write!(out, ": (empty)")?;
            } else {
                writeln!(out, ":")?;
                let body_ctx = ctx.indented();
                for method in &interface_decl.methods {
                    write!(out, "{}", body_ctx.indent_str())?;
                    write_interface_method(out, method, &body_ctx)?;
                }
            }
        }
        StmtKind::Impl(impl_stmt) => {
            write!(
                out,
                "{} {} {} {}",
                "Impl".with_color(ctx.color),
                write_type(&impl_stmt.self_ty, ctx),
                punct_with_color(":", ctx.color),
                impl_stmt.interface.value
            )?;
            if impl_stmt.items.is_empty() {
                write!(out, ": (empty)")?;
            } else {
                writeln!(out, ":")?;
                let body_ctx = ctx.indented();
                for method in &impl_stmt.items {
                    write!(out, "{}", body_ctx.indent_str())?;
                    write_interface_method(out, method, &body_ctx)?;
                }
            }
        }
        StmtKind::FnDecl(fn_decl) => {
            let mut modifiers = Vec::new();
            if fn_decl.visibility == Visibility::Public {
                modifiers.push("pub");
            }
            if fn_decl.is_extern {
                modifiers.push("extern");
            }
            let modifiers = format_modifiers(&modifiers);
            write!(
                out,
                "{} {}\"{}\"",
                "FnDecl".with_color(ctx.color),
                modifiers_with_color(&modifiers, ctx.color),
                fn_decl.name.value
            )?;
            write!(out, " {} ", punct_with_color("->", ctx.color))?;
            write!(out, "{}", write_type(&fn_decl.return_type, ctx))?;
            write!(out, ":")?;
            if fn_decl.parameters.is_empty() && fn_decl.body.is_none() {
                write!(out, " (empty)")?;
            } else {
                writeln!(out)?;
                let sub_ctx = ctx.indented();
                write!(out, "{}Parameters:", sub_ctx.indent_str())?;
                if fn_decl.parameters.is_empty() {
                    writeln!(out)?;
                    write!(out, "{}  (empty)", sub_ctx.indent_str())?;
                } else {
                    writeln!(out)?;
                    let arg_ctx = sub_ctx.indented();
                    for arg in &fn_decl.parameters {
                        writeln!(
                            out,
                            "{}FnArg \"{}\": {}",
                            arg_ctx.indent_str(),
                            arg.name.value,
                            write_type(&arg.ty, &arg_ctx)
                        )?;
                    }
                }
                writeln!(out)?;
                write_fn_decl(out, fn_decl, &sub_ctx)?;
            }
        }
        StmtKind::Return(return_stmt) => {
            write!(out, "{}:", "Return".with_color(ctx.color),)?;
            if let Some(value) = &return_stmt.value {
                writeln!(out)?;
                let value_ctx = ctx.indented();
                write!(out, "{}", value_ctx.indent_str())?;
                write_expr(out, value, &value_ctx)?;
            } else {
                write!(out, " (empty)")?;
            }
        }
        StmtKind::Import(import_stmt) => {
            write!(out, "{}: ", "Import".with_color(ctx.color),)?;
            write_import_tree(out, &import_stmt.tree, ctx)?;
        }
    }
    Ok(())
}

fn write_struct_property(
    out: &mut String,
    prop: &StructField,
    ctx: &DisplayContext,
) -> std::fmt::Result {
    let mut modifiers = Vec::new();
    if prop.visibility == Visibility::Public {
        modifiers.push("pub");
    }
    let modifiers = format_modifiers(&modifiers);
    write!(
        out,
        "{} {}\"{}\": ",
        "Property".with_color(ctx.color),
        modifiers_with_color(&modifiers, ctx.color),
        prop.name.value
    )?;
    write!(out, "{}", write_type(&prop.ty, ctx))?;
    Ok(())
}

fn write_struct_method(
    out: &mut String,
    method: &StructMethod,
    ctx: &DisplayContext,
) -> std::fmt::Result {
    let mut modifiers = Vec::new();
    if method.visibility == Visibility::Public {
        modifiers.push("pub");
    }
    if method.is_static {
        modifiers.push("static");
    }
    let modifiers = format_modifiers(&modifiers);
    write!(
        out,
        "{} {}{}{}",
        "Method".with_color(ctx.color),
        modifiers_with_color(&modifiers, ctx.color),
        punct_with_color("\"", ctx.color),
        method.fn_decl.name.value
    )?;
    write!(out, "{}", punct_with_color("\"", ctx.color))?;
    write!(out, " {} ", punct_with_color("->", ctx.color))?;
    write!(out, "{}", write_type(&method.fn_decl.return_type, ctx))?;
    writeln!(out, ":")?;
    let sub_ctx = ctx.indented();
    let indent = sub_ctx.indent_str();
    write!(out, "{}Parameters:", indent,)?;
    if method.fn_decl.parameters.is_empty() {
        writeln!(out)?;
        write!(out, "{}  (empty)", indent)?;
    } else {
        writeln!(out)?;
        let arg_ctx = sub_ctx.indented();
        for arg in &method.fn_decl.parameters {
            writeln!(
                out,
                "{}FnArg \"{}\": {}",
                arg_ctx.indent_str(),
                arg.name.value,
                write_type(&arg.ty, &arg_ctx)
            )?;
        }
    }
    write_fn_decl(out, &method.fn_decl, &sub_ctx)?;
    Ok(())
}

fn write_fn_decl(out: &mut String, fn_decl: &FnDeclStmt, ctx: &DisplayContext) -> std::fmt::Result {
    write!(out, "{}Body:", ctx.indent_str())?;
    if let Some(body) = &fn_decl.body {
        if body.body.is_empty() {
            writeln!(out)?;
            write!(out, "{}  (empty)", ctx.indent_str())?;
        } else {
            writeln!(out)?;
            let body_ctx = ctx.indented();
            for s in &body.body {
                write!(out, "{}", body_ctx.indent_str())?;
                write_stmt(out, s, &body_ctx)?;
                writeln!(out)?;
            }
        }
    } else {
        writeln!(out)?;
        write!(out, "{}  (none)", ctx.indent_str())?;
    }
    Ok(())
}

fn write_interface_method(
    out: &mut String,
    method: &InterfaceMethod,
    ctx: &DisplayContext,
) -> std::fmt::Result {
    write!(
        out,
        "{} {}{}",
        "Method".with_color(ctx.color),
        punct_with_color("\"", ctx.color),
        method.fn_decl.name.value
    )?;
    write!(out, "{}", punct_with_color("\"", ctx.color))?;
    write!(out, " {} ", punct_with_color("->", ctx.color))?;
    write!(out, "{}", write_type(&method.fn_decl.return_type, ctx))?;
    writeln!(out, ":")?;
    let sub_ctx = ctx.indented();
    let indent = sub_ctx.indent_str();
    write!(out, "{}Parameters:", indent,)?;
    if method.fn_decl.parameters.is_empty() {
        writeln!(out)?;
        write!(out, "{}  (empty)", indent)?;
    } else {
        writeln!(out)?;
        let arg_ctx = sub_ctx.indented();
        for arg in &method.fn_decl.parameters {
            writeln!(
                out,
                "{}FnArg \"{}\": {}",
                arg_ctx.indent_str(),
                arg.name.value,
                write_type(&arg.ty, &arg_ctx)
            )?;
        }
    }
    writeln!(out)?;
    Ok(())
}

fn write_expr(out: &mut String, expr: &Expr, ctx: &DisplayContext) -> std::fmt::Result {
    match &expr.kind {
        ExprKind::Block(block) => {
            write!(out, "{}", "BlockExpr".with_color(ctx.color),)?;
            write!(out, ":")?;
            if block.block.body.is_empty() {
                writeln!(out)?;
                write!(out, "{}  (empty)", ctx.indent_str())?;
            } else {
                writeln!(out)?;
                let body_ctx = ctx.indented();
                for s in &block.block.body {
                    write!(out, "{}", body_ctx.indent_str())?;
                    write_stmt(out, s, &body_ctx)?;
                    writeln!(out)?;
                }
            }
        }
        ExprKind::Literal(lit) => {
            write!(
                out,
                "{}: {}",
                "Literal".with_color(ctx.color),
                match lit {
                    Literal::Integer(i) => number_with_color(&i.to_string(), ctx.color),
                    Literal::Float(f) => number_with_color(&f.to_string(), ctx.color),
                    Literal::String(s) => string_with_color(s, ctx.color),
                    Literal::Char(c) => string_with_color(&c.to_string(), ctx.color),
                    Literal::Bool(b) => string_with_color(&b.to_string(), ctx.color),
                }
            )?;
        }
        ExprKind::Symbol(s) => {
            write!(
                out,
                "{} {}{}{}",
                "Symbol".with_color(ctx.color),
                punct_with_color("\"", ctx.color),
                s.value.value,
                punct_with_color("\"", ctx.color)
            )?;
        }
        ExprKind::Binary(b) => {
            writeln!(out, "{}", "Binary".with_color(ctx.color))?;
            let expr_ctx = ctx.indented();
            writeln!(out, "{}Left:", expr_ctx.indent_str())?;
            write!(out, "{}", expr_ctx.indent_str())?;
            write_expr(out, &b.left, &expr_ctx)?;
            writeln!(out)?;
            write!(
                out,
                "{}Operator: {}",
                expr_ctx.indent_str(),
                punct_with_color(&b.operator.value, ctx.color)
            )?;
            writeln!(out)?;
            write!(out, "{}Right:", expr_ctx.indent_str())?;
            writeln!(out)?;
            write!(out, "{}", expr_ctx.indent_str())?;
            write_expr(out, &b.right, &expr_ctx)?;
        }
        ExprKind::Postfix(p) => {
            writeln!(out, "{}", "Postfix".with_color(ctx.color),)?;
            let expr_ctx = ctx.indented();
            writeln!(out, "{}Left:", expr_ctx.indent_str())?;
            write!(out, "{}", expr_ctx.indent_str())?;
            write_expr(out, &p.left, &expr_ctx)?;
            writeln!(out)?;
            write!(
                out,
                "{}Operator: {}",
                expr_ctx.indent_str(),
                punct_with_color(&p.operator.value, ctx.color)
            )?;
        }
        ExprKind::Prefix(p) => {
            writeln!(out, "{}", "Prefix".with_color(ctx.color),)?;
            let expr_ctx = ctx.indented();
            writeln!(out, "{}Operator:", expr_ctx.indent_str())?;
            write!(
                out,
                "{}  {}",
                expr_ctx.indent_str(),
                punct_with_color(&p.operator.value, ctx.color)
            )?;
            writeln!(out)?;
            write!(out, "{}Right:", expr_ctx.indent_str())?;
            writeln!(out)?;
            write!(out, "{}", expr_ctx.indent_str())?;
            write_expr(out, &p.right, &expr_ctx)?;
        }
        ExprKind::Assignment(a) => {
            writeln!(out, "{}", "Assignment".with_color(ctx.color),)?;
            let expr_ctx = ctx.indented();
            writeln!(out, "{}Assignee:", expr_ctx.indent_str())?;
            write!(out, "{}", expr_ctx.indent_str())?;
            write_expr(out, &a.assigne, &expr_ctx)?;
            writeln!(out)?;
            write!(
                out,
                "{}Operator: {}",
                expr_ctx.indent_str(),
                punct_with_color(&a.operator.value, ctx.color)
            )?;
            writeln!(out)?;
            write!(out, "{}Value:", expr_ctx.indent_str())?;
            writeln!(out)?;
            write!(out, "{}", expr_ctx.indent_str())?;
            write_expr(out, &a.value, &expr_ctx)?;
        }
        ExprKind::StructInstantiation(s) => {
            write!(out, "{}", "StructInstantiation".with_color(ctx.color),)?;
            write!(out, " \"{}\"", s.name.value)?;
            if s.fields.is_empty() {
                write!(out, ": (empty)")?;
            } else {
                writeln!(out, ":")?;
                let prop_ctx = ctx.indented();
                for (i, (name, expr)) in s.fields.iter().enumerate() {
                    if i > 0 {
                        writeln!(out)?;
                    }
                    write!(out, "{}\"{}\": ", prop_ctx.indent_str(), name.value)?;
                    write_expr_inline_or_indented(out, expr, &prop_ctx)?;
                }
            }
        }
        ExprKind::ArrayLiteral(a) => {
            writeln!(out, "{}", "ArrayLiteral".with_color(ctx.color),)?;
            let expr_ctx = ctx.indented();
            write!(
                out,
                "{}Type: []{}",
                expr_ctx.indent_str(),
                write_type(&a.underlying, &expr_ctx)
            )?;
            writeln!(out)?;
            write!(out, "{}Contents:", expr_ctx.indent_str())?;
            if a.contents.is_empty() {
                write!(out, " (empty)")?;
            } else {
                writeln!(out)?;
                let child_ctx = expr_ctx.indented();
                for (i, elem) in a.contents.iter().enumerate() {
                    if i > 0 {
                        writeln!(out)?;
                    }
                    write!(out, "{}", child_ctx.indent_str())?;
                    write_expr(out, elem, &child_ctx)?;
                }
            }
        }
        ExprKind::FunctionCall(call) => {
            writeln!(out, "{}", "FunctionCall".with_color(ctx.color),)?;
            let expr_ctx = ctx.indented();
            write_expr_inline_or_nested(out, "Callee: ", &call.callee, &expr_ctx)?;
            writeln!(out)?;
            write!(out, "{}Parameters:", expr_ctx.indent_str())?;
            if call.parameters.is_empty() {
                write!(out, " (empty)")?;
            } else {
                writeln!(out)?;
                let child_ctx = expr_ctx.indented();
                for (i, arg) in call.parameters.iter().enumerate() {
                    if i > 0 {
                        writeln!(out)?;
                    }
                    write!(out, "{}", child_ctx.indent_str())?;
                    write_expr(out, arg, &child_ctx)?;
                }
            }
        }
        ExprKind::MemberAccess(m) => {
            writeln!(out, "{}", "MemberAccess".with_color(ctx.color),)?;
            let expr_ctx = ctx.indented();
            write_expr_inline_or_nested(out, "Base: ", &m.base, &expr_ctx)?;
            writeln!(out)?;
            write!(
                out,
                "{}Operator: {}",
                expr_ctx.indent_str(),
                punct_with_color(&m.operator.value, ctx.color)
            )?;
            writeln!(out)?;
            write!(
                out,
                "{}Member: \"{}\"",
                expr_ctx.indent_str(),
                m.member.value
            )?;
        }
        ExprKind::Type(t) => {
            write!(out, "{}", "Type".with_color(ctx.color),)?;
            write!(out, ": ")?;
            write!(out, "{}", write_type(&t.underlying, ctx))?;
        }
        ExprKind::As(a) => {
            writeln!(out, "{}", "As".with_color(ctx.color),)?;
            let expr_ctx = ctx.indented();
            writeln!(out, "{}Expr:", expr_ctx.indent_str())?;
            write!(out, "{}", expr_ctx.indent_str())?;
            write_expr(out, &a.expr, &expr_ctx)?;
            writeln!(out)?;
            writeln!(out, "{}Type:", expr_ctx.indent_str())?;
            write!(out, "{}", expr_ctx.indent_str())?;
            write!(out, "{}", write_type(&a.ty, &expr_ctx))?;
        }
        ExprKind::TupleLiteral(t) => {
            write!(out, "{}", "TupleLiteral".with_color(ctx.color),)?;
            if t.elements.is_empty() {
                write!(out, ": (empty)")?;
            } else {
                writeln!(out, ":")?;
                let expr_ctx = ctx.indented();
                for (i, elem) in t.elements.iter().enumerate() {
                    if i > 0 {
                        writeln!(out)?;
                    }
                    write!(out, "{}", expr_ctx.indent_str())?;
                    write_expr(out, elem, &expr_ctx)?;
                }
            }
        }
    }
    Ok(())
}

fn write_type(ty: &Type, ctx: &DisplayContext) -> String {
    match &ty.kind {
        TypeKind::Symbol(s) => type_with_color(&s.name.value, ctx.color),
        TypeKind::Pointer(p) => {
            let inner = write_type(p.underlying.as_ref(), ctx);
            if p.mutability == Mutability::Mutable {
                format!("&mut {}", inner)
            } else {
                format!("&{}", inner)
            }
        }
        TypeKind::Slice(s) => {
            let inner = write_type(s.underlying.as_ref(), ctx);
            format!("[]{}", inner)
        }
        TypeKind::FixedArray(f) => {
            let inner = write_type(f.underlying.as_ref(), ctx);
            format!("[{}]{}", f.length, inner)
        }
        TypeKind::Function(ft) => {
            let params: Vec<String> = ft.parameters.iter().map(|p| write_type(p, ctx)).collect();
            let ret = write_type(ft.return_type.as_ref(), ctx);
            format!("({}) -> {}", params.join(", "), ret)
        }
        TypeKind::Tuple(t) => {
            let elems: Vec<String> = t.elements.iter().map(|e| write_type(e, ctx)).collect();
            format!("({})", elems.join(", "))
        }
        TypeKind::Infer => type_with_color("_", ctx.color),
        TypeKind::Never => type_with_color("!", ctx.color),
    }
}

fn write_import_tree(
    out: &mut String,
    tree: &ImportTree,
    ctx: &DisplayContext,
) -> std::fmt::Result {
    let path: Vec<String> = tree
        .prefix
        .segments
        .iter()
        .map(|s| s.value.to_string())
        .collect();
    let path_str = path.join("::");
    match &tree.kind {
        ImportTreeKind::Simple(rename) => {
            if let Some(r) = rename {
                write!(out, "{} as {}", path_str, r.value)?;
            } else {
                write!(out, "{}", path_str)?;
            }
        }
        ImportTreeKind::Nested { items, .. } => {
            write!(out, "{}::{}", path_str, punct_with_color("{", ctx.color))?;
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    write!(out, ", ")?;
                }
                write_import_tree(out, item, ctx)?;
            }
            write!(out, "{}", punct_with_color("}", ctx.color))?;
        }
        ImportTreeKind::Glob => {
            write!(out, "{}::*", path_str)?;
        }
    }
    Ok(())
}

impl ExprKind {
    pub fn is_leaf(&self) -> bool {
        matches!(self, ExprKind::Literal(_) | ExprKind::Symbol(_))
    }
}
