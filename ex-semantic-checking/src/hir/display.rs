use super::{
    HIRBinaryOperatorKind, HIRBlock, HIRExpression, HIRExpressionKind, HIRFunction,
    HIRFunctionParam, HIRFunctionReturnType, HIRProgram, HIRStatementKind, HIRUnaryOperatorKind,
};
use crate::resolve::{TypeKind, TypeReference};
use std::fmt::Display;

pub struct HIRSourceBuilder {
    indent: usize,
    elements: Vec<HIRSourceBuilderElement>,
}

impl HIRSourceBuilder {
    pub fn new(indent: usize) -> Self {
        Self {
            indent,
            elements: Vec::new(),
        }
    }

    pub fn push_str(&mut self, str: impl Into<String>) {
        self.elements
            .push(HIRSourceBuilderElement::String(str.into()));
    }

    pub fn push_line(&mut self, str: impl Into<String>) {
        self.elements
            .push(HIRSourceBuilderElement::String(format!("{}\n", str.into())));
    }

    pub fn push_builder(&mut self, builder: HIRSourceBuilder) {
        self.elements
            .push(HIRSourceBuilderElement::Builder(builder.into()));
    }
}

impl<T> From<T> for HIRSourceBuilder
where
    T: Into<String>,
{
    fn from(value: T) -> Self {
        let mut builder = Self::new(0);
        builder.push_str(value);
        builder
    }
}

impl Display for HIRSourceBuilder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let stringified = self
            .elements
            .iter()
            .map(|element| match element {
                HIRSourceBuilderElement::String(str) => str.clone(),
                HIRSourceBuilderElement::Builder(builder) => builder.to_string(),
            })
            .collect::<Vec<_>>()
            .join("");
        let indent = "  ".repeat(self.indent);
        let lines = stringified
            .lines()
            .map(|line| format!("{}{}", indent, line))
            .collect::<Vec<_>>()
            .join("\n");
        write!(f, "{}", lines)?;

        if stringified.ends_with("\n") {
            writeln!(f)?;
        }

        Ok(())
    }
}

pub enum HIRSourceBuilderElement {
    String(String),
    Builder(HIRSourceBuilder),
}

pub trait SourceBuilder {
    fn build_source(&self, indent: usize) -> HIRSourceBuilder;
}

impl SourceBuilder for HIRProgram {
    fn build_source(&self, indent: usize) -> HIRSourceBuilder {
        let mut builder = HIRSourceBuilder::new(0);
        for function in &self.functions {
            builder.push_builder(function.build_source(indent));
            builder.push_line("");
        }
        builder
    }
}

impl SourceBuilder for HIRFunction {
    fn build_source(&self, indent: usize) -> HIRSourceBuilder {
        let mut builder = HIRSourceBuilder::new(indent);

        if self.signature.params.is_empty() {
            builder.push_str(format!("fn {}()", self.signature.name.symbol.to_str()));
        } else {
            builder.push_line(format!("fn {}(", self.signature.name.symbol.to_str()));
            for param in &self.signature.params {
                builder.push_builder(param.build_source(1));
                builder.push_line(",");
            }
            builder.push_str(")");
        }

        match &self.signature.return_type {
            Some(return_type) => {
                builder.push_str(" -> ");
                builder.push_builder(return_type.build_source(0));
            }
            None => {}
        }

        builder.push_str(" ");
        builder.push_builder(self.body_block.build_source(indent));
        builder.push_line("");
        builder
    }
}

impl SourceBuilder for HIRFunctionParam {
    fn build_source(&self, indent: usize) -> HIRSourceBuilder {
        let mut builder = HIRSourceBuilder::new(indent);
        builder.push_str(self.name.symbol.to_str());
        builder.push_str(": ");
        builder.push_builder(self.type_ref.build_source(0));
        builder
    }
}

impl SourceBuilder for HIRFunctionReturnType {
    fn build_source(&self, indent: usize) -> HIRSourceBuilder {
        self.type_ref.build_source(indent)
    }
}

impl SourceBuilder for HIRBlock {
    fn build_source(&self, indent: usize) -> HIRSourceBuilder {
        let mut outer_builder = HIRSourceBuilder::new(indent);
        outer_builder.push_line("{");

        let mut builder = HIRSourceBuilder::new(1);
        for statement in &self.statements {
            match &statement.kind {
                HIRStatementKind::Block(ast) => {
                    builder.push_builder(ast.build_source(0));
                    builder.push_line("");
                }
                HIRStatementKind::Let(ast) => {
                    builder.push_str("let ");
                    builder.push_str(ast.name.symbol.to_str());

                    if let Some(let_type) = &ast.let_type {
                        builder.push_str(": ");
                        builder.push_builder(let_type.type_ref.build_source(0));
                    }

                    if let Some(let_assignment) = &ast.let_assignment {
                        builder.push_str(" = ");
                        builder.push_builder(let_assignment.expression.build_source(0));
                    }

                    builder.push_line(";");
                }
                HIRStatementKind::If(ast) => {
                    builder.push_str("if ");
                    builder.push_builder(ast.expression.build_source(0));
                    builder.push_str(" ");
                    builder.push_builder(ast.body_block.build_source(0));

                    for ast in &ast.single_else_ifs {
                        builder.push_str(" else if ");
                        builder.push_builder(ast.expression.build_source(0));
                        builder.push_str(" ");
                        builder.push_builder(ast.body_block.build_source(0));
                    }

                    if let Some(ast) = &ast.single_else {
                        builder.push_str(" else ");
                        builder.push_builder(ast.body_block.build_source(0));
                    }

                    builder.push_line("");
                }
                HIRStatementKind::Loop(ast) => {
                    builder.push_str("loop ");
                    builder.push_builder(ast.body_block.build_source(0));
                    builder.push_line("");
                }
                HIRStatementKind::Break(_) => {
                    builder.push_line("break;");
                }
                HIRStatementKind::Continue(_) => {
                    builder.push_line("continue;");
                }
                HIRStatementKind::Return(ast) => match &ast.expression {
                    Some(ast) => {
                        builder.push_str("return ");
                        builder.push_builder(ast.build_source(0));
                        builder.push_line(";");
                    }
                    None => {
                        builder.push_line("return;");
                    }
                },
                HIRStatementKind::Assignment(ast) => {
                    builder.push_builder(ast.left.build_source(0));
                    builder.push_str(" = ");
                    builder.push_builder(ast.right.build_source(0));
                    builder.push_line(";");
                }
                HIRStatementKind::Row(ast) => {
                    builder.push_builder(ast.expression.build_source(0));
                    builder.push_line(";");
                }
            }
        }

        outer_builder.push_builder(builder);
        outer_builder.push_str("}");
        outer_builder
    }
}

impl SourceBuilder for HIRExpression {
    fn build_source(&self, indent: usize) -> HIRSourceBuilder {
        let mut builder = HIRSourceBuilder::new(indent);
        builder.push_builder(match &self.kind {
            HIRExpressionKind::Binary(ast) => {
                let mut builder = HIRSourceBuilder::new(indent);
                builder.push_str("(");
                builder.push_builder(ast.left.build_source(0));
                builder.push_str(") ");
                builder.push_builder(ast.operator_kind.build_source(0));
                builder.push_str(" (");
                builder.push_builder(ast.right.build_source(0));
                builder.push_str(")");
                builder
            }
            HIRExpressionKind::Unary(ast) => {
                let mut builder = HIRSourceBuilder::new(indent);
                builder.push_builder(ast.operator_kind.build_source(0));
                builder.push_str(" (");
                builder.push_builder(ast.right.build_source(0));
                builder.push_str(")");
                builder
            }
            HIRExpressionKind::As(ast) => {
                let mut builder = HIRSourceBuilder::new(indent);
                builder.push_str("(");
                builder.push_builder(ast.expression.build_source(0));
                builder.push_str(") as ");
                builder.push_builder(ast.type_ref.build_source(0));
                builder
            }
            HIRExpressionKind::Call(ast) => {
                let mut builder = HIRSourceBuilder::new(indent);
                builder.push_str("(");
                builder.push_builder(ast.expression.build_source(0));

                if ast.args.is_empty() {
                    builder.push_str(")()");
                    return builder;
                }

                builder.push_line(")(");
                builder.push_builder({
                    let mut builder = HIRSourceBuilder::new(1);
                    for arg in &ast.args {
                        builder.push_builder(arg.build_source(0));
                        builder.push_line(",");
                    }
                    builder
                });
                builder.push_str(")");
                builder
            }
            HIRExpressionKind::Member(ast) => {
                let mut builder = HIRSourceBuilder::new(indent);
                builder.push_str("(");
                builder.push_builder(ast.expression.build_source(0));
                builder.push_str(").");
                builder.push_str(ast.member.symbol.to_str());
                builder
            }
            HIRExpressionKind::FunctionRef(ast) => {
                let mut builder = HIRSourceBuilder::new(indent);
                builder.push_str(ast.reference.symbol.to_str());
                builder
            }
            HIRExpressionKind::ParamRef(ast) => {
                let mut builder = HIRSourceBuilder::new(indent);
                builder.push_str(ast.reference.symbol.to_str());
                builder
            }
            HIRExpressionKind::VariableRef(ast) => {
                let mut builder = HIRSourceBuilder::new(indent);
                builder.push_str(ast.reference.symbol.to_str());
                builder
            }
            HIRExpressionKind::UnknownRef(ast) => {
                let mut builder = HIRSourceBuilder::new(indent);
                builder.push_str(ast.reference.symbol.to_str());
                builder
            }
            HIRExpressionKind::StructLiteral(ast) => {
                let mut builder = HIRSourceBuilder::new(indent);
                builder.push_line("{");
                builder.push_builder({
                    let mut builder = HIRSourceBuilder::new(1);
                    for field in &ast.fields {
                        builder.push_str(field.name.symbol.to_str());
                        builder.push_str(": ");
                        builder.push_builder(field.expression.build_source(0));
                        builder.push_line(",");
                    }
                    builder
                });
                builder.push_str("}");
                builder
            }
            HIRExpressionKind::Literal(ast) => {
                let mut builder = HIRSourceBuilder::new(indent);
                builder.push_str(ast.content.to_str());
                builder
            }
        });
        builder
    }
}

impl SourceBuilder for HIRBinaryOperatorKind {
    fn build_source(&self, indent: usize) -> HIRSourceBuilder {
        let mut builder = HIRSourceBuilder::new(indent);
        builder.push_str(match self {
            HIRBinaryOperatorKind::Eq => "==",
            HIRBinaryOperatorKind::Ne => "!=",
            HIRBinaryOperatorKind::Lt => "<",
            HIRBinaryOperatorKind::Gt => ">",
            HIRBinaryOperatorKind::Le => "<=",
            HIRBinaryOperatorKind::Ge => ">=",
            HIRBinaryOperatorKind::LogOr => "||",
            HIRBinaryOperatorKind::LogAnd => "&&",
            HIRBinaryOperatorKind::Add => "+",
            HIRBinaryOperatorKind::Sub => "-",
            HIRBinaryOperatorKind::Mul => "*",
            HIRBinaryOperatorKind::Div => "/",
            HIRBinaryOperatorKind::Mod => "%",
            HIRBinaryOperatorKind::Pow => "^",
            HIRBinaryOperatorKind::Shl => "<<",
            HIRBinaryOperatorKind::Shr => ">>",
            HIRBinaryOperatorKind::BitOr => "|",
            HIRBinaryOperatorKind::BitAnd => "&",
            HIRBinaryOperatorKind::BitXor => "^",
        });
        builder
    }
}

impl SourceBuilder for HIRUnaryOperatorKind {
    fn build_source(&self, indent: usize) -> HIRSourceBuilder {
        let mut builder = HIRSourceBuilder::new(indent);
        builder.push_str(match self {
            HIRUnaryOperatorKind::Minus => "-",
            HIRUnaryOperatorKind::BitNot => "~",
            HIRUnaryOperatorKind::LogNot => "!",
            HIRUnaryOperatorKind::AddressOf => "&",
            HIRUnaryOperatorKind::Dereference => "*",
        });
        builder
    }
}

impl SourceBuilder for TypeKind {
    fn build_source(&self, indent: usize) -> HIRSourceBuilder {
        let mut builder = HIRSourceBuilder::new(indent);
        builder.push_builder(match self {
            TypeKind::Unknown => "?".into(),
            TypeKind::Empty => "()".into(),
            TypeKind::Bool => "bool".into(),
            TypeKind::Int => "int".into(),
            TypeKind::Float => "float".into(),
            TypeKind::String => "string".into(),
            TypeKind::Callable {
                params,
                return_type,
            } => {
                let params = params
                    .iter()
                    .map(|param| param.build_source(0).to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                let return_type = return_type.build_source(0).to_string();
                format!("fn ({}) -> {}", params, return_type).into()
            }
            TypeKind::UserStruct { id } => format!("[struct id={}]", id.get()).into(),
            TypeKind::Pointer { inner } => {
                format!("({}) ptr", inner.build_source(0).to_string()).into()
            }
            TypeKind::Reference { inner } => {
                format!("({}) ref", inner.build_source(0).to_string()).into()
            }
        });
        builder
    }
}

impl SourceBuilder for TypeReference {
    fn build_source(&self, indent: usize) -> HIRSourceBuilder {
        self.kind.build_source(indent)
    }
}

impl SourceBuilder for Option<TypeReference> {
    fn build_source(&self, indent: usize) -> HIRSourceBuilder {
        match self {
            Some(type_ref) => type_ref.build_source(indent),
            None => "?".into(),
        }
    }
}
