use crate::{TypeKind, UnresolvedTopLevelTable, UnresolvedUserType};
use ex_diagnostics::DiagnosticsSender;
use ex_parser::{Id, NodeId, Typename, TypenameKind};
use ex_span::Span;
use ex_symbol::Symbol;
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct TopLevelTable {
    pub functions: HashMap<NodeId, Function>,
    pub function_symbols: HashMap<Symbol, NodeId>,
    pub user_types: HashMap<NodeId, UserType>,
    pub user_type_symbols: HashMap<Symbol, NodeId>,
}

impl TopLevelTable {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn lookup_function(&self, symbol: Symbol) -> Option<&Function> {
        self.function_symbols
            .get(&symbol)
            .and_then(|node_id| self.functions.get(node_id))
    }

    pub fn lookup_user_type(&self, symbol: Symbol) -> Option<&UserType> {
        self.user_type_symbols
            .get(&symbol)
            .and_then(|node_id| self.user_types.get(node_id))
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
            .map(|param| typename_to_type_kind(unresolved_table, param, diagnostics))
            .collect();
        let param_names = function.param_names.clone();
        let return_type = function
            .return_type
            .as_ref()
            .map(|return_type| typename_to_type_kind(unresolved_table, return_type, diagnostics))
            .unwrap_or_else(|| TypeKind::Empty);
        let function = Function::new(
            function.id,
            function.name.clone(),
            params,
            param_names,
            return_type,
            function.span,
        );
        table.functions.insert(function.id, function);
    }

    for user_type in unresolved_table.user_types.values() {
        match user_type {
            UnresolvedUserType::UserStruct(user_struct) => {
                let fields = user_struct
                    .fields
                    .iter()
                    .map(|field| typename_to_type_kind(unresolved_table, field, diagnostics))
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
                table
                    .user_types
                    .insert(user_struct.id, UserType::user_struct(user_struct));
            }
        }
    }

    table
}

pub fn typename_to_type_kind(
    unresolved_table: &UnresolvedTopLevelTable,
    typename: &Typename,
    diagnostics: &DiagnosticsSender,
) -> TypeKind {
    match &typename.kind {
        TypenameKind::Id(id) => match id.symbol {
            symbol if symbol == *ex_parser::TYPENAME_BOOL => TypeKind::bool(),
            symbol if symbol == *ex_parser::TYPENAME_INT => TypeKind::int(),
            symbol if symbol == *ex_parser::TYPENAME_FLOAT => TypeKind::float(),
            symbol if symbol == *ex_parser::TYPENAME_STRING => TypeKind::string(),
            symbol => match unresolved_table.lookup_user_type(symbol) {
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
                .map(|param| typename_to_type_kind(unresolved_table, &param.typename, diagnostics))
                .collect();
            let return_type = function
                .return_type
                .as_ref()
                .map(|return_type| {
                    typename_to_type_kind(unresolved_table, &return_type.typename, diagnostics)
                })
                .unwrap_or_else(|| TypeKind::empty());
            TypeKind::callable(params, return_type)
        }
        TypenameKind::Pointer(pointer) => {
            let inner = typename_to_type_kind(unresolved_table, &pointer.typename, diagnostics);
            TypeKind::pointer(inner)
        }
        TypenameKind::Reference(reference) => {
            let inner = typename_to_type_kind(unresolved_table, &reference.typename, diagnostics);
            TypeKind::reference(inner)
        }
    }
}
