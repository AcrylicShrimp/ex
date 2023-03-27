use ex_diagnostics::DiagnosticsSender;
use ex_parser::{ASTFunction, ASTProgram, ASTStruct, ASTTopLevelKind, Id, NodeId, Typename};
use ex_span::Span;
use ex_symbol::Symbol;
use std::collections::{hash_map::Entry, HashMap};

#[derive(Default, Debug, Clone)]
pub struct UnresolvedTopLevelTable {
    pub functions: HashMap<NodeId, UnresolvedFunction>,
    pub function_symbols: HashMap<Symbol, NodeId>,
    pub user_types: HashMap<NodeId, UnresolvedUserType>,
    pub user_type_symbols: HashMap<Symbol, NodeId>,
}

impl UnresolvedTopLevelTable {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn lookup_function(&self, symbol: Symbol) -> Option<&UnresolvedFunction> {
        self.function_symbols
            .get(&symbol)
            .and_then(|node_id| self.functions.get(node_id))
    }

    pub fn lookup_user_type(&self, symbol: Symbol) -> Option<&UnresolvedUserType> {
        self.user_type_symbols
            .get(&symbol)
            .and_then(|node_id| self.user_types.get(node_id))
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
    pub field_spans: Vec<Span>,
    pub field_names: HashMap<Symbol, usize>,
    pub span: Span,
}

impl UnresolvedUserStruct {
    pub fn new(
        id: NodeId,
        name: Id,
        fields: Vec<Typename>,
        field_spans: Vec<Span>,
        field_names: HashMap<Symbol, usize>,
        span: Span,
    ) -> Self {
        Self {
            id,
            name,
            fields,
            field_spans,
            field_names,
            span,
        }
    }
}

pub fn build_unresolved_top_levels(
    ast: &ASTProgram,
    diagnostics: &DiagnosticsSender,
) -> UnresolvedTopLevelTable {
    let mut unresolved_top_level_table = UnresolvedTopLevelTable::new();

    for top_level in &ast.top_levels {
        match &top_level.kind {
            ASTTopLevelKind::Function(ast) => {
                check_duplicated_params(ast, diagnostics);

                let params = ast
                    .signature
                    .params
                    .iter()
                    .map(|param| param.typename.clone())
                    .collect();
                let param_names = ast
                    .signature
                    .params
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
                    ast.signature.span,
                );
                unresolved_top_level_table
                    .function_symbols
                    .insert(unresolved_function.name.symbol, unresolved_function.id);
                unresolved_top_level_table
                    .functions
                    .insert(unresolved_function.id, unresolved_function);
            }
            ASTTopLevelKind::Struct(ast) => {
                check_duplicated_fields(ast, diagnostics);

                let fields = ast
                    .fields
                    .iter()
                    .map(|field| field.typename.clone())
                    .collect();
                let field_spans = ast.fields.iter().map(|field| field.span).collect();
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
                    field_spans,
                    field_names,
                    top_level.span,
                );
                unresolved_top_level_table.user_type_symbols.insert(
                    unresolved_user_struct.name.symbol,
                    unresolved_user_struct.id,
                );
                unresolved_top_level_table.user_types.insert(
                    unresolved_user_struct.id,
                    UnresolvedUserType::user_struct(unresolved_user_struct),
                );
            }
        }
    }

    unresolved_top_level_table
}

fn check_duplicated_params(ast: &ASTFunction, diagnostics: &DiagnosticsSender) {
    let mut param_names = HashMap::<Symbol, Span>::new();

    for param in &ast.signature.params {
        match param_names.entry(param.name.symbol) {
            Entry::Occupied(entry) => {
                diagnostics.error_sub(
                    param.name.span,
                    format!("duplicated param name {}", param.name.symbol),
                    vec![diagnostics.sub_hint(*entry.get(), format!("previous param name here"))],
                );
            }
            Entry::Vacant(entry) => {
                entry.insert(param.name.span);
            }
        }
    }
}

fn check_duplicated_fields(ast: &ASTStruct, diagnostics: &DiagnosticsSender) {
    let mut field_names = HashMap::<Symbol, Span>::new();

    for field in &ast.fields {
        match field_names.entry(field.name.symbol) {
            Entry::Occupied(entry) => {
                diagnostics.error_sub(
                    field.name.span,
                    format!("duplicated field name {}", field.name.symbol),
                    vec![diagnostics.sub_hint(*entry.get(), format!("previous field name here"))],
                );
            }
            Entry::Vacant(entry) => {
                entry.insert(field.name.span);
            }
        }
    }
}
