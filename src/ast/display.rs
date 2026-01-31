use std::fmt::Write;

use colored::Colorize;

use crate::ast::{
    AssocItem, AssocItemKind, Expr, ExprKind, Fn, Ident, ImportTree, ImportTreeKind, Item,
    ItemKind, Literal, Mutability, Stmt, StmtKind, Type, TypeKind, Visibility,
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

pub fn write_item(out: &mut String, item: &Item, ctx: &DisplayContext) -> std::fmt::Result {
    match &item.kind {
        ItemKind::Static { value, name, ty } => {
            let mut modifiers = Vec::new();
            if item.visibility == Visibility::Public {
                modifiers.push("pub");
            }
            let modifiers = format_modifiers(&modifiers);
            write!(
                out,
                "{} {}{}{}{}: ",
                "Static".with_color(ctx.color),
                modifiers_with_color(&modifiers, ctx.color),
                punct_with_color("\"", ctx.color),
                name.value,
                punct_with_color("\"", ctx.color)
            )?;
            write!(out, "{}", write_type(ty, ctx))?;
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
        }
        ItemKind::Struct {
            name,
            fields,
            items,
        } => {
            let mut modifiers = Vec::new();
            if item.visibility == Visibility::Public {
                modifiers.push("pub");
            }
            let modifiers = format_modifiers(&modifiers);
            write!(
                out,
                "{} {}{}{}{}",
                "StructDecl".with_color(ctx.color),
                modifiers_with_color(&modifiers, ctx.color),
                punct_with_color("\"", ctx.color),
                name.value,
                punct_with_color("\"", ctx.color)
            )?;
            if fields.is_empty() && items.is_empty() {
                write!(out, ": (empty)")?;
            } else {
                writeln!(out, ":")?;
                let body_ctx = ctx.indented();
                let mut idx = 0;
                for field in fields {
                    write!(out, "{}", body_ctx.indent_str())?;
                    write_struct_field(out, field, &body_ctx)?;
                    if idx > 0 {
                        writeln!(out)?;
                    }
                    idx += 1;
                }
                for item in items {
                    if idx > 0 {
                        writeln!(out)?;
                    }
                    write!(out, "{}", body_ctx.indent_str())?;
                    write_assoc_item(out, item, &body_ctx)?;
                    idx += 1;
                }
            }
        }
        ItemKind::Interface { name, items } => {
            let mut modifiers = Vec::new();
            if item.visibility == Visibility::Public {
                modifiers.push("pub");
            }
            let modifiers = format_modifiers(&modifiers);
            write!(
                out,
                "{} {}{}{}{}",
                "InterfaceDecl".with_color(ctx.color),
                modifiers_with_color(&modifiers, ctx.color),
                punct_with_color("\"", ctx.color),
                name.value,
                punct_with_color("\"", ctx.color)
            )?;
            if items.is_empty() {
                write!(out, ": (empty)")?;
            } else {
                writeln!(out, ":")?;
                let body_ctx = ctx.indented();
                for item in items {
                    write!(out, "{}", body_ctx.indent_str())?;
                    write_assoc_item(out, item, &body_ctx)?;
                }
            }
        }
        ItemKind::Impl {
            self_ty,
            interface,
            items,
        } => {
            write!(
                out,
                "{} {} {} {}",
                "Impl".with_color(ctx.color),
                write_type(self_ty, ctx),
                punct_with_color(":", ctx.color),
                interface.value
            )?;
            if items.is_empty() {
                write!(out, ": (empty)")?;
            } else {
                writeln!(out, ":")?;
                let body_ctx = ctx.indented();
                for item in items {
                    write!(out, "{}", body_ctx.indent_str())?;
                    write_assoc_item(out, item, &body_ctx)?;
                }
            }
        }
        ItemKind::Fn(f) => {
            let mut modifiers = Vec::new();
            if item.visibility == Visibility::Public {
                modifiers.push("pub");
            }
            if f.is_extern {
                modifiers.push("extern");
            }
            let modifiers = format_modifiers(&modifiers);
            write!(
                out,
                "{} {}\"{}\"",
                "FnDecl".with_color(ctx.color),
                modifiers_with_color(&modifiers, ctx.color),
                f.name.value
            )?;
            write!(out, " {} ", punct_with_color("->", ctx.color))?;
            write!(out, "{}", write_type(&f.return_type, ctx))?;
            write!(out, ":")?;
            if f.parameters.is_empty() && f.body.is_none() {
                write!(out, " (empty)")?;
            } else {
                writeln!(out)?;
                let sub_ctx = ctx.indented();
                write!(out, "{}Parameters:", sub_ctx.indent_str())?;
                if f.parameters.is_empty() {
                    writeln!(out)?;
                    write!(out, "{}  (empty)", sub_ctx.indent_str())?;
                } else {
                    writeln!(out)?;
                    let arg_ctx = sub_ctx.indented();
                    for arg in &f.parameters {
                        writeln!(
                            out,
                            "{}FnArg \"{}\": {}",
                            arg_ctx.indent_str(),
                            arg.0.value,
                            write_type(&arg.1, &arg_ctx)
                        )?;
                    }
                }
                writeln!(out)?;
                write_fn_body(out, f, &sub_ctx)?;
            }
        }
        ItemKind::Import(i) => {
            let mut modifiers = Vec::new();
            if item.visibility == Visibility::Public {
                modifiers.push("pub");
            }
            let modifiers = format_modifiers(&modifiers);
            write!(
                out,
                "{} {}: ",
                "Import".with_color(ctx.color),
                modifiers_with_color(&modifiers, ctx.color)
            )?;
            write_import_tree(out, i, ctx)?;
        }
    }
    Ok(())
}

pub fn write_stmt(out: &mut String, stmt: &Stmt, ctx: &DisplayContext) -> std::fmt::Result {
    match &stmt.kind {
        StmtKind::Expr(expr) => {
            write!(out, "{}:", "ExpressionStmt".with_color(ctx.color),)?;
            if expr.kind.is_leaf() {
                write!(out, " ")?;
                write_expr(out, expr, &ctx.clone())?;
            } else {
                writeln!(out)?;
                let expr_ctx = ctx.indented();
                write!(out, "{}", expr_ctx.indent_str())?;
                write_expr(out, expr, &expr_ctx)?;
            }
        }
        StmtKind::Semi(expr) => {
            write!(out, "{}:", "SemiStmt".with_color(ctx.color),)?;
            if expr.kind.is_leaf() {
                write!(out, " ")?;
                write_expr(out, expr, &ctx.clone())?;
            } else {
                writeln!(out)?;
                let expr_ctx = ctx.indented();
                write!(out, "{}", expr_ctx.indent_str())?;
                write_expr(out, expr, &expr_ctx)?;
            }
        }
        StmtKind::Let {
            name,
            ty,
            value,
            mutability,
        } => {
            let mut modifiers = Vec::new();
            if *mutability == Mutability::Mutable {
                modifiers.push("mut");
            }
            let modifiers = format_modifiers(&modifiers);
            write!(
                out,
                "{} {}{}{}{}: ",
                "Let".with_color(ctx.color),
                modifiers_with_color(&modifiers, ctx.color),
                punct_with_color("\"", ctx.color),
                name.value,
                punct_with_color("\"", ctx.color)
            )?;
            write!(out, "{}", write_type(ty, ctx))?;
            if let Some(val) = value {
                write!(out, " =")?;
                if val.kind.is_leaf() {
                    write!(out, " ")?;
                    write_expr(out, val, ctx)?;
                } else {
                    writeln!(out)?;
                    let value_ctx = ctx.indented();
                    write!(out, "{}", value_ctx.indent_str())?;
                    write_expr(out, val, &value_ctx)?;
                }
            } else {
                write!(out, " (uninitialized)")?;
            }
        }
    }
    Ok(())
}

fn write_struct_field(
    out: &mut String,
    prop: &(Ident, Type, Visibility),
    ctx: &DisplayContext,
) -> std::fmt::Result {
    let mut modifiers = Vec::new();
    if prop.2 == Visibility::Public {
        modifiers.push("pub");
    }
    let modifiers = format_modifiers(&modifiers);
    write!(
        out,
        "{} {}\"{}\": ",
        "Property".with_color(ctx.color),
        modifiers_with_color(&modifiers, ctx.color),
        prop.0.value
    )?;
    write!(out, "{}", write_type(&prop.1, ctx))?;
    Ok(())
}

fn write_assoc_item(out: &mut String, item: &AssocItem, ctx: &DisplayContext) -> std::fmt::Result {
    match &item.kind {
        AssocItemKind::Fn(f) => {
            let mut modifiers = Vec::new();
            if item.visibility == Visibility::Public {
                modifiers.push("pub");
            }
            if item.is_static {
                modifiers.push("static");
            }
            let modifiers = format_modifiers(&modifiers);
            write!(
                out,
                "{} {}{}{}",
                "Method".with_color(ctx.color),
                modifiers_with_color(&modifiers, ctx.color),
                punct_with_color("\"", ctx.color),
                f.name.value
            )?;
            write!(out, "{}", punct_with_color("\"", ctx.color))?;
            write!(out, " {} ", punct_with_color("->", ctx.color))?;
            write!(out, "{}", write_type(&f.return_type, ctx))?;
            writeln!(out, ":")?;
            let sub_ctx = ctx.indented();
            let indent = sub_ctx.indent_str();
            write!(out, "{}Parameters:", indent,)?;
            if f.parameters.is_empty() {
                writeln!(out)?;
                write!(out, "{}  (empty)", indent)?;
            } else {
                writeln!(out)?;
                let arg_ctx = sub_ctx.indented();
                for (name, ty) in &f.parameters {
                    writeln!(
                        out,
                        "{}FnArg \"{}\": {}",
                        arg_ctx.indent_str(),
                        name.value,
                        write_type(ty, &arg_ctx)
                    )?;
                }
            }
            writeln!(out)?;
            write_fn_body(out, f, &sub_ctx)?;
            Ok(())
        }
    }
}

fn write_fn_body(out: &mut String, f: &Fn, ctx: &DisplayContext) -> std::fmt::Result {
    write!(out, "{}Body:", ctx.indent_str())?;
    if let Some(body) = &f.body {
        if body.stmts.is_empty() {
            writeln!(out)?;
            write!(out, "{}  (empty)", ctx.indent_str())?;
        } else {
            writeln!(out)?;
            let body_ctx = ctx.indented();
            for s in &body.stmts {
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

fn write_expr(out: &mut String, expr: &Expr, ctx: &DisplayContext) -> std::fmt::Result {
    match &expr.kind {
        ExprKind::Block(block) => {
            write!(out, "{}", "BlockExpr".with_color(ctx.color),)?;
            write!(out, ":")?;
            if block.stmts.is_empty() {
                writeln!(out)?;
                write!(out, "{}  (empty)", ctx.indent_str())?;
            } else {
                writeln!(out)?;
                let body_ctx = ctx.indented();
                for s in &block.stmts {
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
                s.value,
                punct_with_color("\"", ctx.color)
            )?;
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            writeln!(out, "{}", "If".with_color(ctx.color))?;
            let expr_ctx = ctx.indented();
            write_expr_inline_or_nested(out, "Condition: ", condition, &expr_ctx)?;
            writeln!(out)?;
            write!(out, "{}Then:", expr_ctx.indent_str())?;
            if then_branch.stmts.is_empty() {
                writeln!(out)?;
                write!(out, "{}  (empty)", expr_ctx.indent_str())?;
            } else {
                writeln!(out)?;
                let body_ctx = expr_ctx.indented();
                for s in &then_branch.stmts {
                    write!(out, "{}", body_ctx.indent_str())?;
                    write_stmt(out, s, &body_ctx)?;
                    writeln!(out)?;
                }
            }
            write!(out, "{}Else:", expr_ctx.indent_str())?;
            if let Some(else_branch) = else_branch {
                writeln!(out)?;
                let else_ctx = expr_ctx.indented();
                write!(out, "{}", else_ctx.indent_str())?;
                write_expr(out, else_branch, &else_ctx)?;
            } else {
                writeln!(out)?;
                write!(out, "{}  (empty)", expr_ctx.indent_str())?;
            }
        }
        ExprKind::While { condition, body } => {
            writeln!(out, "{}", "While".with_color(ctx.color))?;
            let expr_ctx = ctx.indented();
            write_expr_inline_or_nested(out, "Condition: ", condition, &expr_ctx)?;
            writeln!(out)?;
            write!(out, "{}Body:", expr_ctx.indent_str())?;
            if body.stmts.is_empty() {
                writeln!(out)?;
                write!(out, "{}  (empty)", expr_ctx.indent_str())?;
            } else {
                writeln!(out)?;
                let body_ctx = expr_ctx.indented();
                for s in &body.stmts {
                    write!(out, "{}", body_ctx.indent_str())?;
                    write_stmt(out, s, &body_ctx)?;
                    writeln!(out)?;
                }
            }
        }
        ExprKind::Loop(l) => {
            writeln!(out, "{}", "Loop".with_color(ctx.color))?;
            let expr_ctx = ctx.indented();
            write!(out, "{}Body:", expr_ctx.indent_str())?;
            if l.stmts.is_empty() {
                writeln!(out)?;
                write!(out, "{}  (empty)", expr_ctx.indent_str())?;
            } else {
                writeln!(out)?;
                let body_ctx = expr_ctx.indented();
                for s in &l.stmts {
                    write!(out, "{}", body_ctx.indent_str())?;
                    write_stmt(out, s, &body_ctx)?;
                    writeln!(out)?;
                }
            }
        }
        ExprKind::Break(break_expr) => {
            write!(out, "{}", "Break".with_color(ctx.color),)?;
            if let Some(value) = break_expr {
                writeln!(out, ":")?;
                let value_ctx = ctx.indented();
                write!(out, "{}", value_ctx.indent_str())?;
                write_expr(out, value, &value_ctx)?;
            }
        }
        ExprKind::Return(return_expr) => {
            write!(out, "{}", "Return".with_color(ctx.color),)?;
            if let Some(value) = return_expr {
                writeln!(out, ":")?;
                let value_ctx = ctx.indented();
                write!(out, "{}", value_ctx.indent_str())?;
                write_expr(out, value, &value_ctx)?;
            }
        }
        ExprKind::Binary {
            left,
            operator,
            right,
        } => {
            writeln!(out, "{}", "Binary".with_color(ctx.color))?;
            let expr_ctx = ctx.indented();
            writeln!(out, "{}Left:", expr_ctx.indent_str())?;
            write!(out, "{}", expr_ctx.indent_str())?;
            write_expr(out, left, &expr_ctx)?;
            writeln!(out)?;
            write!(
                out,
                "{}Operator: {}",
                expr_ctx.indent_str(),
                punct_with_color(&operator.value, ctx.color)
            )?;
            writeln!(out)?;
            write!(out, "{}Right:", expr_ctx.indent_str())?;
            writeln!(out)?;
            write!(out, "{}", expr_ctx.indent_str())?;
            write_expr(out, right, &expr_ctx)?;
        }
        ExprKind::Postfix { left, operator } => {
            writeln!(out, "{}", "Postfix".with_color(ctx.color),)?;
            let expr_ctx = ctx.indented();
            writeln!(out, "{}Left:", expr_ctx.indent_str())?;
            write!(out, "{}", expr_ctx.indent_str())?;
            write_expr(out, left, &expr_ctx)?;
            writeln!(out)?;
            write!(
                out,
                "{}Operator: {}",
                expr_ctx.indent_str(),
                punct_with_color(&operator.value, ctx.color)
            )?;
        }
        ExprKind::Prefix { operator, right } => {
            writeln!(out, "{}", "Prefix".with_color(ctx.color),)?;
            let expr_ctx = ctx.indented();
            writeln!(out, "{}Operator:", expr_ctx.indent_str())?;
            write!(
                out,
                "{}  {}",
                expr_ctx.indent_str(),
                punct_with_color(&operator.value, ctx.color)
            )?;
            writeln!(out)?;
            write!(out, "{}Right:", expr_ctx.indent_str())?;
            writeln!(out)?;
            write!(out, "{}", expr_ctx.indent_str())?;
            write_expr(out, right, &expr_ctx)?;
        }
        ExprKind::Assignment {
            assignee,
            operator,
            value,
        } => {
            writeln!(out, "{}", "Assignment".with_color(ctx.color),)?;
            let expr_ctx = ctx.indented();
            writeln!(out, "{}Assignee:", expr_ctx.indent_str())?;
            write!(out, "{}", expr_ctx.indent_str())?;
            write_expr(out, assignee, &expr_ctx)?;
            writeln!(out)?;
            write!(
                out,
                "{}Operator: {}",
                expr_ctx.indent_str(),
                punct_with_color(&operator.value, ctx.color)
            )?;
            writeln!(out)?;
            write!(out, "{}Value:", expr_ctx.indent_str())?;
            writeln!(out)?;
            write!(out, "{}", expr_ctx.indent_str())?;
            write_expr(out, value, &expr_ctx)?;
        }
        ExprKind::StructInstantiation { name, fields } => {
            write!(out, "{}", "StructInstantiation".with_color(ctx.color),)?;
            write!(out, " \"{}\"", name.value)?;
            if fields.is_empty() {
                write!(out, ": (empty)")?;
            } else {
                writeln!(out, ":")?;
                let prop_ctx = ctx.indented();
                for (i, (name, expr)) in fields.iter().enumerate() {
                    if i > 0 {
                        writeln!(out)?;
                    }
                    write!(out, "{}\"{}\": ", prop_ctx.indent_str(), name.value)?;
                    write_expr_inline_or_indented(out, expr, &prop_ctx)?;
                }
            }
        }
        ExprKind::ArrayLiteral {
            underlying,
            contents,
        } => {
            writeln!(out, "{}", "ArrayLiteral".with_color(ctx.color),)?;
            let expr_ctx = ctx.indented();
            write!(
                out,
                "{}Type: []{}",
                expr_ctx.indent_str(),
                write_type(underlying, &expr_ctx)
            )?;
            writeln!(out)?;
            write!(out, "{}Contents:", expr_ctx.indent_str())?;
            if contents.is_empty() {
                write!(out, " (empty)")?;
            } else {
                writeln!(out)?;
                let child_ctx = expr_ctx.indented();
                for (i, elem) in contents.iter().enumerate() {
                    if i > 0 {
                        writeln!(out)?;
                    }
                    write!(out, "{}", child_ctx.indent_str())?;
                    write_expr(out, elem, &child_ctx)?;
                }
            }
        }
        ExprKind::FunctionCall { callee, parameters } => {
            writeln!(out, "{}", "FunctionCall".with_color(ctx.color),)?;
            let expr_ctx = ctx.indented();
            write_expr_inline_or_nested(out, "Callee: ", callee, &expr_ctx)?;
            writeln!(out)?;
            write!(out, "{}Parameters:", expr_ctx.indent_str())?;
            if parameters.is_empty() {
                write!(out, " (empty)")?;
            } else {
                writeln!(out)?;
                let child_ctx = expr_ctx.indented();
                for (i, arg) in parameters.iter().enumerate() {
                    if i > 0 {
                        writeln!(out)?;
                    }
                    write!(out, "{}", child_ctx.indent_str())?;
                    write_expr(out, arg, &child_ctx)?;
                }
            }
        }
        ExprKind::MemberAccess {
            base,
            member,
            operator,
        } => {
            writeln!(out, "{}", "MemberAccess".with_color(ctx.color),)?;
            let expr_ctx = ctx.indented();
            write_expr_inline_or_nested(out, "Base: ", base, &expr_ctx)?;
            writeln!(out)?;
            write!(
                out,
                "{}Operator: {}",
                expr_ctx.indent_str(),
                punct_with_color(&operator.value, ctx.color)
            )?;
            writeln!(out)?;
            write!(out, "{}Member: \"{}\"", expr_ctx.indent_str(), member.value)?;
        }
        ExprKind::Type(t) => {
            write!(out, "{}", "Type".with_color(ctx.color),)?;
            write!(out, ": ")?;
            write!(out, "{}", write_type(t, ctx))?;
        }
        ExprKind::As { expr, ty } => {
            writeln!(out, "{}", "As".with_color(ctx.color),)?;
            let expr_ctx = ctx.indented();
            writeln!(out, "{}Expr:", expr_ctx.indent_str())?;
            write!(out, "{}", expr_ctx.indent_str())?;
            write_expr(out, expr, &expr_ctx)?;
            writeln!(out)?;
            writeln!(out, "{}Type:", expr_ctx.indent_str())?;
            write!(out, "{}", expr_ctx.indent_str())?;
            write!(out, "{}", write_type(ty, &expr_ctx))?;
        }
        ExprKind::TupleLiteral { elements } => {
            write!(out, "{}", "TupleLiteral".with_color(ctx.color),)?;
            if elements.is_empty() {
                write!(out, ": (empty)")?;
            } else {
                writeln!(out, ":")?;
                let expr_ctx = ctx.indented();
                for (i, elem) in elements.iter().enumerate() {
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
