use ex_parser::{ASTProgram, ASTTopLevelKind, Id, NodeId, Typename};
use ex_span::Span;
use ex_symbol::Symbol;
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct UnresolvedTopLevelTable {
    pub functions: HashMap<Symbol, UnresolvedFunction>,
    pub user_types: HashMap<Symbol, UnresolvedUserType>,
}

impl UnresolvedTopLevelTable {
    pub fn new() -> Self {
        Default::default()
    }
}

#[derive(Debug, Clone)]
pub struct UnresolvedFunction {
    pub id: NodeId,
    pub name: Id,
    pub params: Vec<Typename>,
    pub param_names: Vec<Symbol>,
    pub return_type: Option<Typename>,
    pub span: Span,
}

impl UnresolvedFunction {
    pub fn new(
        id: NodeId,
        name: Id,
        params: Vec<Typename>,
        param_names: Vec<Symbol>,
        return_type: Option<Typename>,
        span: Span,
    ) -> Self {
        Self {
            id,
            name,
            params,
            param_names,
            return_type,
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnresolvedUserType {
    UserStruct(UnresolvedUserStruct),
}

impl UnresolvedUserType {
    pub fn user_struct(user_struct: UnresolvedUserStruct) -> Self {
        Self::UserStruct(user_struct)
    }
}

#[derive(Debug, Clone)]
pub struct UnresolvedUserStruct {
    pub id: NodeId,
    pub name: Id,
    pub fields: Vec<Typename>,
    pub field_names: HashMap<Symbol, usize>,
    pub span: Span,
}

impl UnresolvedUserStruct {
    pub fn new(
        id: NodeId,
        name: Id,
        fields: Vec<Typename>,
        field_names: HashMap<Symbol, usize>,
        span: Span,
    ) -> Self {
        Self {
            id,
            name,
            fields,
            field_names,
            span,
        }
    }
}

pub fn build_unresolved_top_levels(ast: &ASTProgram) -> UnresolvedTopLevelTable {
    let mut unresolved_top_level_table = UnresolvedTopLevelTable::new();

    for top_level in &ast.top_levels {
        match &top_level.kind {
            ASTTopLevelKind::Function(ast) => {
                let params = ast
                    .signature
                    .parameters
                    .iter()
                    .map(|param| param.typename.clone())
                    .collect();
                let param_names = ast
                    .signature
                    .parameters
                    .iter()
                    .map(|param| param.name.symbol.clone())
                    .collect();
                let unresolved_function = UnresolvedFunction::new(
                    top_level.id,
                    ast.signature.name.clone(),
                    params,
                    param_names,
                    ast.signature
                        .return_type
                        .as_ref()
                        .map(|return_type| return_type.typename.clone()),
                    top_level.span,
                );
                unresolved_top_level_table
                    .functions
                    .insert(unresolved_function.name.symbol.clone(), unresolved_function);
            }
            ASTTopLevelKind::Struct(ast) => {
                let fields = ast
                    .fields
                    .iter()
                    .map(|field| field.typename.clone())
                    .collect();
                let field_names = HashMap::from_iter(
                    ast.fields
                        .iter()
                        .enumerate()
                        .map(|(index, field)| (field.name.symbol.clone(), index)),
                );
                let unresolved_user_struct = UnresolvedUserStruct::new(
                    top_level.id,
                    ast.name.clone(),
                    fields,
                    field_names,
                    top_level.span,
                );
                unresolved_top_level_table.user_types.insert(
                    unresolved_user_struct.name.symbol.clone(),
                    UnresolvedUserType::user_struct(unresolved_user_struct),
                );
            }
        }
    }

    unresolved_top_level_table
}
