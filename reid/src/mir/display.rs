use std::fmt::{Debug, Display, Write};

use crate::pad_adapter::PadAdapter;

use super::*;

impl Display for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for module in &self.modules {
            Display::fmt(&module, f)?;
        }
        Ok(())
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Module({}) {{", self.name)?;

        let mut state = Default::default();
        let mut inner_f = PadAdapter::wrap(f, &mut state);

        for import in &self.imports {
            writeln!(inner_f, "{}", import)?;
        }
        for fun in &self.functions {
            writeln!(inner_f, "{}", fun)?;
        }
        writeln!(f, "}}")
    }
}

impl Display for Import {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "import {}", self.0.join("::"))
    }
}

impl Display for FunctionDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "fn {}({}) -> {} ",
            self.name,
            self.parameters
                .iter()
                .map(|(n, t)| format!("{}: {}", n, t))
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
            Self::Local(block, _) => {
                write!(f, "{}", block)?;
                Ok(())
            }
            Self::Extern => write!(f, "<External>"),
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
            match ret.0 {
                ReturnKind::Hard => writeln!(inner_f, "Return(Hard): {}", ret.1),
                ReturnKind::Soft => writeln!(inner_f, "Return(Soft): {}", ret.1),
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
            Self::Let(var, mutable, block) => {
                write!(
                    f,
                    "let{} {} = {}",
                    if *mutable { " mut" } else { "" },
                    var,
                    block
                )
            }
            Self::Set(var, expr) => write!(f, "{} = {}", var, expr),
            Self::Import(n) => write!(f, "import {}", n),
            Self::Expression(exp) => Display::fmt(exp, f),
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
            ExprKind::BinOp(op, lhs, rhs) => write!(f, "{} {} {}", lhs, op, rhs),
            ExprKind::FunctionCall(fc) => Display::fmt(fc, f),
            ExprKind::If(if_exp) => Display::fmt(&if_exp, f),
            ExprKind::Block(block) => Display::fmt(block, f),
            ExprKind::Index(expression, elem_ty, idx) => {
                Display::fmt(&expression, f)?;
                write!(f, "<{}>", elem_ty)?;
                write_index(f, *idx)
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
        }
    }
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {} ", self.0)?;
        Display::fmt(&self.1, f)?;
        if let Some(e) = &self.2 {
            Display::fmt(&e, f)?;
        }
        Ok(())
    }
}

impl Display for FunctionCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}<{}>(", self.name, self.return_type)?;
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
        write!(f, "v(\"{}\", {})", &self.1, &self.0)
    }
}

impl Display for IndexedVariableReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            IndexedVariableReferenceKind::Named(name) => Display::fmt(name, f),
            IndexedVariableReferenceKind::Index(variable_reference_kind, idx) => {
                Display::fmt(&variable_reference_kind, f)?;
                write_index(f, *idx)
            }
        }
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
        write!(f, "{:?}", self.range)
    }
}

fn write_index(f: &mut std::fmt::Formatter<'_>, idx: u64) -> std::fmt::Result {
    f.write_char('[')?;
    Display::fmt(&idx, f)?;
    f.write_char(']')
}
