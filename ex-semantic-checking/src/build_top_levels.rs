use crate::{TypeKind, UnresolvedTopLevelTable, UnresolvedUserType};
use ex_diagnostics::DiagnosticsSender;
use ex_parser::{Id, NodeId, Typename, TypenameKind};
use ex_span::Span;
use ex_symbol::Symbol;
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct TopLevelTable {
    pub functions: HashMap<Symbol, Function>,
    pub user_types: HashMap<Symbol, UserType>,
}

impl TopLevelTable {
    pub fn new() -> Self {
        Default::default()
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub id: NodeId,
    pub name: Id,
    pub params: Vec<TypeKind>,
    pub param_names: Vec<Symbol>,
    pub return_type: TypeKind,
    pub span: Span,
}

impl Function {
    pub fn new(
        id: NodeId,
        name: Id,
        params: Vec<TypeKind>,
        param_names: Vec<Symbol>,
        return_type: TypeKind,
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
pub enum UserType {
    UserStruct(UserStruct),
}

impl UserType {
    pub fn user_struct(user_struct: UserStruct) -> Self {
        Self::UserStruct(user_struct)
    }
}

#[derive(Debug, Clone)]
pub struct UserStruct {
    pub id: NodeId,
    pub name: Id,
    pub fields: Vec<TypeKind>,
    pub field_names: HashMap<Symbol, usize>,
    pub span: Span,
}

impl UserStruct {
    pub fn new(
        id: NodeId,
        name: Id,
        fields: Vec<TypeKind>,
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

pub fn build_top_levels(
    unresolved_table: &UnresolvedTopLevelTable,
    diagnostics: &DiagnosticsSender,
) -> TopLevelTable {
    let mut table = TopLevelTable::new();

    for function in unresolved_table.functions.values() {
        let params = function
            .params
            .iter()
            .map(|param| typename_to_type_kind(param, unresolved_table, diagnostics))
            .collect();
        let param_names = function.param_names.clone();
        let return_type = function
            .return_type
            .as_ref()
            .map(|return_type| typename_to_type_kind(return_type, unresolved_table, diagnostics))
            .unwrap_or_else(|| TypeKind::Empty);
        let function = Function::new(
            function.id,
            function.name.clone(),
            params,
            param_names,
            return_type,
            function.span,
        );
        table
            .functions
            .insert(function.name.symbol.clone(), function);
    }

    for user_type in unresolved_table.user_types.values() {
        match user_type {
            UnresolvedUserType::UserStruct(user_struct) => {
                let fields = user_struct
                    .fields
                    .iter()
                    .map(|field| typename_to_type_kind(field, unresolved_table, diagnostics))
                    .collect();
                let field_names = user_struct
                    .field_names
                    .iter()
                    .map(|(name, index)| (name.clone(), *index))
                    .collect();
                let user_struct = UserStruct::new(
                    user_struct.id,
                    user_struct.name.clone(),
                    fields,
                    field_names,
                    user_struct.span,
                );
                table.user_types.insert(
                    user_struct.name.symbol.clone(),
                    UserType::user_struct(user_struct),
                );
            }
        }
    }

    table
}

pub fn typename_to_type_kind(
    typename: &Typename,
    unresolved_table: &UnresolvedTopLevelTable,
    diagnostics: &DiagnosticsSender,
) -> TypeKind {
    match &typename.kind {
        TypenameKind::Id(id) => match id.symbol {
            symbol if symbol == *ex_parser::TYPENAME_BOOL => TypeKind::bool(),
            symbol if symbol == *ex_parser::TYPENAME_INT => TypeKind::int(),
            symbol if symbol == *ex_parser::TYPENAME_FLOAT => TypeKind::float(),
            symbol if symbol == *ex_parser::TYPENAME_STRING => TypeKind::string(),
            symbol => match unresolved_table.user_types.get(&symbol) {
                Some(user_type) => match user_type {
                    UnresolvedUserType::UserStruct(user_struct) => {
                        TypeKind::user_struct(user_struct.id)
                    }
                },
                None => {
                    diagnostics.error(typename.span, format!("unresolved type {}", symbol));
                    TypeKind::unknown()
                }
            },
        },
        TypenameKind::Callable(function) => {
            let params = function
                .parameters
                .iter()
                .map(|param| typename_to_type_kind(&param.typename, unresolved_table, diagnostics))
                .collect();
            let return_type = function
                .return_type
                .as_ref()
                .map(|return_type| {
                    typename_to_type_kind(&return_type.typename, unresolved_table, diagnostics)
                })
                .unwrap_or_else(|| TypeKind::empty());
            TypeKind::callable(params, return_type)
        }
        TypenameKind::Pointer(pointer) => {
            let inner = typename_to_type_kind(&pointer.typename, unresolved_table, diagnostics);
            TypeKind::pointer(inner)
        }
        TypenameKind::Reference(reference) => {
            let inner = typename_to_type_kind(&reference.typename, unresolved_table, diagnostics);
            TypeKind::reference(inner)
        }
    }
}
