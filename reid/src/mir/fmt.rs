use std::fmt::{Debug, Display, Write};

use crate::pad_adapter::PadAdapter;

use super::*;

impl Display for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (_, module) in &self.modules {
            Display::fmt(&module, f)?;
        }
        Ok(())
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let is_alternate = f.alternate();

        writeln!(f, "Module({}) ({}) {{", self.name, self.module_id)?;

        let mut state = Default::default();
        let mut inner_f = PadAdapter::wrap(f, &mut state);

        for import in &self.imports {
            writeln!(inner_f, "{}", import)?;
        }

        let intrinsic_binops = self
            .binop_defs
            .iter()
            .filter(|b| matches!(b.fn_kind, FunctionDefinitionKind::Intrinsic(_)));

        for binop in self
            .binop_defs
            .iter()
            .filter(|b| !matches!(b.fn_kind, FunctionDefinitionKind::Intrinsic(_)))
        {
            writeln!(inner_f, "{}", binop)?;
        }

        if is_alternate {
            writeln!(inner_f, "... <{}> intrinsic binary operators", intrinsic_binops.count())?;
        } else {
            for binop in intrinsic_binops {
                writeln!(inner_f, "{}", binop)?;
            }
        }
        for typedef in &self.typedefs {
            writeln!(inner_f, "{}", typedef)?;
        }
        for global in &self.globals {
            writeln!(inner_f, "global {} = {}", global.name, global.kind)?;
        }
        for (ty, fun) in &self.associated_functions {
            writeln!(inner_f, "(Assoc {}) {}", ty, fun)?;
        }
        for fun in &self.functions {
            writeln!(inner_f, "{}", fun)?;
        }
        writeln!(f, "}}")
    }
}

impl Display for GlobalKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GlobalKind::Literal(literal) => Display::fmt(literal, f),
            GlobalKind::Array(global_kinds) => {
                f.write_char('[')?;
                let mut iter = global_kinds.iter();
                if let Some(global) = iter.next() {
                    Display::fmt(global, f)?;
                    while let Some(global) = iter.next() {
                        write!(f, ", ")?;
                        Display::fmt(global, f)?;
                    }
                }
                f.write_char(']')
            }
        }
    }
}

impl Display for Import {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "import {}",
            self.0.iter().map(|(s, _)| s.clone()).collect::<Vec<_>>().join("::")
        )
    }
}

impl Display for BinopDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}impl binop ({}: {:#}) {} ({}: {:#}) -> {:#} ",
            if self.exported { "exported " } else { "" },
            self.lhs.name,
            self.lhs.ty,
            self.op,
            self.rhs.name,
            self.rhs.ty,
            self.return_type
        )?;
        Display::fmt(&self.fn_kind, f)
    }
}

impl Display for TypeDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "type {} (mod {}{}) = ",
            self.name,
            self.source_module,
            if let Some(mod_id) = self.importer {
                format!("; imported to {}", mod_id)
            } else {
                String::new()
            }
        )?;
        Display::fmt(&self.kind, f)
    }
}

impl Display for TypeDefinitionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeDefinitionKind::Struct(items) => {
                write!(f, "struct ")?;
                f.write_char('{')?;
                writeln!(f)?;
                let mut state = Default::default();
                let mut inner_f = PadAdapter::wrap(f, &mut state);
                for field in &items.0 {
                    writeln!(inner_f, "{},", field)?;
                }
                f.write_char('}')
            }
        }
    }
}

impl Display for StructField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {:?}", self.0, self.1)
    }
}

impl Display for FunctionDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}fn {}({}) -> {:#} ",
            if self.is_pub { "pub " } else { "" },
            self.name,
            self.parameters
                .iter()
                .map(|FunctionParam { name, ty, .. }| format!("{}: {:#}", name, ty))
                .collect::<Vec<_>>()
                .join(", "),
            self.return_type
        )?;
        Display::fmt(&self.kind, f)
    }
}

impl Display for FunctionDefinitionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionDefinitionKind::Local(block, _) => {
                write!(f, "{}", block)?;
                Ok(())
            }
            FunctionDefinitionKind::Extern(true) => write!(f, "<Imported Extern>"),
            FunctionDefinitionKind::Extern(false) => write!(f, "<Linked Extern>"),
            FunctionDefinitionKind::Intrinsic(_) => write!(f, "<Intrinsic>"),
        }
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{{")?;
        let mut state = Default::default();
        let mut inner_f = PadAdapter::wrap(f, &mut state);
        for statement in &self.statements {
            write!(inner_f, "{}", statement)?;
        }
        if let Some(ret) = &self.return_expression {
            let ret_fmt = if let Some(ret) = &ret.1 {
                format!("{}", ret)
            } else {
                String::from("void")
            };

            match ret.0 {
                ReturnKind::Hard => writeln!(inner_f, "Return(Hard): {}", ret_fmt),
                ReturnKind::Soft => writeln!(inner_f, "Return(Soft): {}", ret_fmt),
            }?;
        } else {
            writeln!(inner_f, "No Return")?;
        }
        writeln!(f, "}}")
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.0)
    }
}

impl Display for StmtKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StmtKind::Let(var, mutable, block) => {
                write!(f, "let{} {} = {}", if *mutable { " mut" } else { "" }, var, block)
            }
            StmtKind::Set(var, expr) => write!(f, "{} = {}", var, expr),
            StmtKind::Import(n) => write!(f, "import {}", n),
            StmtKind::Expression(exp) => Display::fmt(exp, f),

            StmtKind::While(while_statement) => {
                write!(f, "while {} {}", while_statement.condition, while_statement.block,)
            }
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('(')?;
        Display::fmt(&self.0, f)?;
        f.write_char(')')?;
        Ok(())
    }
}

impl Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprKind::Variable(var) => Display::fmt(var, f),
            ExprKind::Literal(lit) => Display::fmt(lit, f),
            ExprKind::BinOp(op, lhs, rhs, ty) => {
                write!(f, "{} {} {} (= ", lhs, op, rhs)?;
                Debug::fmt(ty, f)?;
                f.write_char(')')?;
                Ok(())
            }
            ExprKind::FunctionCall(fc) => Display::fmt(fc, f),
            ExprKind::If(if_exp) => Display::fmt(&if_exp, f),
            ExprKind::Block(block) => Display::fmt(block, f),
            ExprKind::Indexed(expression, elem_ty, idx_expr) => {
                Display::fmt(&expression, f)?;
                write!(f, "<{:#}>", elem_ty)?;
                write_index(f, idx_expr)
            }
            ExprKind::Array(expressions) => {
                f.write_char('[')?;

                let mut state = Default::default();
                let mut inner_f = PadAdapter::wrap(f, &mut state);

                let mut iter = expressions.iter();
                if let Some(item) = iter.next() {
                    write!(inner_f, "\n{}", item)?;
                    while let Some(item) = iter.next() {
                        writeln!(inner_f, ",")?;
                        write!(inner_f, "{}", item)?;
                    }
                    writeln!(inner_f, "")?;
                }
                f.write_char(']')
            }
            ExprKind::Struct(key, items) => {
                write!(f, "{:?} ", key)?;

                f.write_char('{')?;
                let mut state = Default::default();
                let mut inner_f = PadAdapter::wrap(f, &mut state);
                let mut iter = items.iter();
                if let Some((name, expr, _)) = iter.next() {
                    write!(inner_f, "\n{}: {}", name, expr)?;
                    while let Some((name, expr, _)) = iter.next() {
                        writeln!(inner_f, ",")?;
                        write!(inner_f, "{}: {}", name, expr)?;
                    }
                    writeln!(inner_f, "")?;
                }
                f.write_char('}')
            }
            ExprKind::Accessed(expression, type_kind, name, _) => {
                Display::fmt(&expression, f)?;
                write_access(f, name)?;
                write!(f, "<{}>", type_kind)
            }
            ExprKind::Borrow(var_ref, false) => write!(f, "&{}", var_ref),
            ExprKind::Borrow(var_ref, true) => write!(f, "&mut {}", var_ref),
            ExprKind::Deref(var_ref) => write!(f, "*{}", var_ref),
            ExprKind::CastTo(expression, type_kind) => write!(f, "{} as {}", expression, type_kind),
            ExprKind::AssociatedFunctionCall(type_kind, function_call) => {
                Display::fmt(type_kind, f)?;
                write!(f, "::")?;
                Display::fmt(function_call, f)
            }
            ExprKind::GlobalRef(global_value, type_kind) => write!(f, "global<{}>(${})", type_kind, global_value),
        }
    }
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {} ", self.0)?;
        Display::fmt(&self.1, f)?;
        if let Some(e) = self.2.as_ref() {
            Display::fmt(&e, f)?;
        }
        Ok(())
    }
}

impl Display for FunctionCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}<{:#}>(", self.name, self.return_type)?;
        for (i, param) in self.parameters.iter().enumerate() {
            Display::fmt(param, f)?;
            if i < (self.parameters.len() - 1) {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}

impl Display for NamedVariableRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "v(\"{}\", {:#})", &self.1, &self.0)
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::I8(val) => write!(f, "{}i8", val),
            Literal::I16(val) => write!(f, "{}i16", val),
            Literal::I32(val) => write!(f, "{}i32", val),
            Literal::I64(val) => write!(f, "{}i64", val),
            Literal::I128(val) => write!(f, "{}i128", val),
            Literal::U8(val) => write!(f, "{}u8", val),
            Literal::U16(val) => write!(f, "{}u16", val),
            Literal::U32(val) => write!(f, "{}u32", val),
            Literal::U64(val) => write!(f, "{}u64", val),
            Literal::U128(val) => write!(f, "{}u128", val),
            Literal::Bool(val) => write!(f, "{}", val),
            Literal::String(val) => std::fmt::Debug::fmt(val, f),
            Literal::Vague(val) => val.fmt(f),
            Literal::F16(val) => write!(f, "{}f16", val),
            Literal::F32B(val) => write!(f, "{}f16b", val),
            Literal::F32(val) => write!(f, "{}f32", val),
            Literal::F64(val) => write!(f, "{}f64", val),
            Literal::F80(val) => write!(f, "{}f80", val),
            Literal::F128(val) => write!(f, "{}f128", val),
            Literal::F128PPC(val) => write!(f, "{}f128ppc", val),
            Literal::Char(c) => std::fmt::Debug::fmt(c, f),
        }
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "+"),
            BinaryOperator::Minus => write!(f, "-"),
            BinaryOperator::Mult => write!(f, "*"),
            BinaryOperator::And => write!(f, "&&"),
            BinaryOperator::Cmp(op) => Display::fmt(op, f),
            BinaryOperator::Div => write!(f, "/"),
            BinaryOperator::Mod => write!(f, "%"),
            BinaryOperator::Or => write!(f, "||"),
            BinaryOperator::Xor => write!(f, "^"),
            BinaryOperator::BitOr => write!(f, "|"),
            BinaryOperator::BitAnd => write!(f, "&"),
            BinaryOperator::BitshiftRight => write!(f, ">>"),
            BinaryOperator::BitshiftLeft => write!(f, "<<"),
        }
    }
}

impl Display for CmpOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CmpOperator::LT => write!(f, "<"),
            CmpOperator::LE => write!(f, "<="),
            CmpOperator::GT => write!(f, ">"),
            CmpOperator::GE => write!(f, ">="),
            CmpOperator::EQ => write!(f, "=="),
            CmpOperator::NE => write!(f, "!="),
        }
    }
}

impl Display for Metadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} ({})", self.range, self.source_module_id)
    }
}

impl Display for SourceModuleId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

fn write_index(f: &mut std::fmt::Formatter<'_>, idx: impl std::fmt::Display) -> std::fmt::Result {
    f.write_char('[')?;
    Display::fmt(&idx, f)?;
    f.write_char(']')
}

fn write_access(f: &mut std::fmt::Formatter<'_>, name: &String) -> std::fmt::Result {
    f.write_char('.')?;
    Display::fmt(name, f)
}

impl Display for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::I8 => write!(f, "i8"),
            TypeKind::I16 => write!(f, "i16"),
            TypeKind::I32 => write!(f, "i32"),
            TypeKind::I64 => write!(f, "i64"),
            TypeKind::I128 => write!(f, "i128"),
            TypeKind::U8 => write!(f, "u8"),
            TypeKind::U16 => write!(f, "u16"),
            TypeKind::U32 => write!(f, "u32"),
            TypeKind::U64 => write!(f, "u64"),
            TypeKind::U128 => write!(f, "u128"),
            TypeKind::Void => write!(f, "void"),
            TypeKind::Char => write!(f, "char"),
            TypeKind::Array(type_kind, len) => {
                f.write_char('[')?;
                Display::fmt(type_kind, f)?;
                write!(f, "; ")?;
                Display::fmt(len, f)?;
                f.write_char(']')
            }
            TypeKind::CustomType(CustomTypeKey(name, mod_id)) => write!(f, "{}@{}", name, mod_id),
            TypeKind::Borrow(type_kind, false) => {
                write!(f, "&")?;
                Display::fmt(type_kind, f)
            }
            TypeKind::Borrow(type_kind, true) => {
                write!(f, "&mut ")?;
                Display::fmt(type_kind, f)
            }
            TypeKind::UserPtr(type_kind) => {
                write!(f, "*")?;
                Display::fmt(type_kind, f)
            }
            TypeKind::CodegenPtr(type_kind) => {
                write!(f, "CodegenPtr ")?;
                Display::fmt(type_kind, f)
            }
            TypeKind::Vague(vague_type) => Display::fmt(vague_type, f),
            TypeKind::F16 => write!(f, "f16"),
            TypeKind::F32B => write!(f, "f16b"),
            TypeKind::F32 => write!(f, "f32"),
            TypeKind::F64 => write!(f, "f64"),
            TypeKind::F128 => write!(f, "f128"),
            TypeKind::F80 => write!(f, "f80"),
            TypeKind::F128PPC => write!(f, "f128ppc"),
        }
    }
}

impl Display for VagueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            match self {
                VagueType::Unknown => write!(f, "Unknown"),
                VagueType::Integer => write!(f, "Number"),
                VagueType::TypeRef(id) => write!(f, "TypeRef({0})", id),
                VagueType::Decimal => write!(f, "Decimal"),
            }
        } else {
            match self {
                VagueType::Unknown => write!(f, "{{unknown}}"),
                VagueType::Integer => write!(f, "Number"),
                VagueType::TypeRef(_) => write!(f, "{{unknown}}"),
                VagueType::Decimal => write!(f, "Decimal"),
            }
        }
    }
}
