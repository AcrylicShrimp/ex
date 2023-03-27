use crate::resolve::{
    typename_to_type_kind, TypeKind, UnresolvedFunction, UnresolvedTopLevelTable,
};
use ex_diagnostics::DiagnosticsSender;
use ex_parser::{
    ASTBlock, ASTExpression, ASTExpressionKind, ASTFunction, ASTIdReference, ASTProgram,
    ASTStatementKind, ASTTopLevelKind, Id, NodeId, Typename,
};
use ex_span::Span;
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
pub struct ReferenceTable {
    pub type_references: HashMap<NodeId, TypeReference>,
    pub symbol_references: HashMap<NodeId, SymbolReference>,
    pub function_scopes: HashMap<NodeId, FunctionScope>,
}

impl ReferenceTable {
    pub fn new() -> Self {
        Default::default()
    }
}

#[derive(Debug, Clone, Hash)]
pub struct TypeReference {
    pub id: NodeId,
    pub kind: TypeKind,
    pub span: Span,
}

impl TypeReference {
    pub fn new(id: NodeId, kind: TypeKind, span: Span) -> Self {
        Self { id, kind, span }
    }
}

#[derive(Debug, Clone, Hash)]
pub struct SymbolReference {
    pub id: NodeId,
    pub name: Id,
    pub kind: SymbolReferenceKind,
    pub span: Span,
}

impl SymbolReference {
    pub fn new(id: NodeId, name: Id, kind: SymbolReferenceKind, span: Span) -> Self {
        Self {
            id,
            name,
            kind,
            span,
        }
    }
}

#[derive(Debug, Clone, Hash)]
pub enum SymbolReferenceKind {
    Function {
        function: NodeId,
    },
    Param {
        function: NodeId,
        index: usize,
    },
    Variable {
        function: NodeId,
        scope: ScopeId,
        index: usize,
    },
}

impl SymbolReferenceKind {
    pub fn function(function: NodeId) -> Self {
        Self::Function { function }
    }

    pub fn param(function: NodeId, index: usize) -> Self {
        Self::Param { function, index }
    }

    pub fn variable(function: NodeId, scope: ScopeId, index: usize) -> Self {
        Self::Variable {
            function,
            scope,
            index,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionScope {
    pub id: NodeId,
    pub scope_table: ScopeTable,
    pub root_scope: ScopeId,
}

impl FunctionScope {
    pub fn new(id: NodeId) -> Self {
        let mut scope_table = ScopeTable::new();
        let root_scope = scope_table.new_scope(id, None);

        Self {
            id,
            scope_table,
            root_scope,
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct ScopeTable {
    pub scopes: HashMap<ScopeId, Scope>,
}

impl ScopeTable {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn new_scope(&mut self, id: NodeId, parent: Option<ScopeId>) -> ScopeId {
        let id = ScopeId::new(id);
        self.scopes.insert(id, Scope::new(id, parent));
        id
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScopeId(NodeId);

impl ScopeId {
    pub fn new(id: NodeId) -> Self {
        Self(id)
    }
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub id: ScopeId,
    pub parent: Option<ScopeId>,
    pub variables: Vec<ScopeVariable>,
}

impl Scope {
    pub fn new(id: ScopeId, parent: Option<ScopeId>) -> Self {
        Self {
            id,
            parent,
            variables: Vec::new(),
        }
    }

    pub fn new_variable(&mut self, variable: ScopeVariable) {
        self.variables.push(variable);
    }
}

#[derive(Debug, Clone)]
pub struct ScopeVariable {
    pub id: NodeId,
    pub name: Id,
    pub span: Span,
}

impl ScopeVariable {
    pub fn new(id: NodeId, name: Id, span: Span) -> Self {
        Self { id, name, span }
    }
}

pub fn build_references(
    unresolved_table: &UnresolvedTopLevelTable,
    ast: &ASTProgram,
    diagnostics: &DiagnosticsSender,
) -> ReferenceTable {
    let mut table = ReferenceTable::new();

    for top_level in &ast.top_levels {
        match &top_level.kind {
            ASTTopLevelKind::Function(ast) => {
                for param in &ast.signature.params {
                    let type_ref =
                        resolve_type_reference(unresolved_table, &param.typename, diagnostics);
                    table.type_references.insert(param.typename.id, type_ref);
                }

                if let Some(return_type) = &ast.signature.return_type {
                    let type_ref = resolve_type_reference(
                        unresolved_table,
                        &return_type.typename,
                        diagnostics,
                    );
                    table
                        .type_references
                        .insert(return_type.typename.id, type_ref);
                }

                let function = unresolved_table.functions.get(&top_level.id).unwrap();
                let function_scope = build_references_and_scopes(
                    &mut table,
                    unresolved_table,
                    function,
                    ast,
                    diagnostics,
                );
                table.function_scopes.insert(function.id, function_scope);
            }
            ASTTopLevelKind::Struct(ast) => {
                for field in &ast.fields {
                    let type_ref =
                        resolve_type_reference(unresolved_table, &field.typename, diagnostics);
                    table.type_references.insert(field.typename.id, type_ref);
                }
            }
        }
    }

    table
}

pub fn build_references_and_scopes(
    reference_table: &mut ReferenceTable,
    unresolved_table: &UnresolvedTopLevelTable,
    function: &UnresolvedFunction,
    ast: &ASTFunction,
    diagnostics: &DiagnosticsSender,
) -> FunctionScope {
    let mut function_scope = FunctionScope::new(function.id);

    build_references_and_scopes_stmt_block(
        function_scope.root_scope,
        &mut function_scope.scope_table,
        reference_table,
        unresolved_table,
        function,
        &ast.body_block,
        diagnostics,
    );

    function_scope
}

pub fn build_references_and_scopes_stmt_block(
    scope: ScopeId,
    scope_table: &mut ScopeTable,
    reference_table: &mut ReferenceTable,
    unresolved_table: &UnresolvedTopLevelTable,
    function: &UnresolvedFunction,
    ast: &ASTBlock,
    diagnostics: &DiagnosticsSender,
) {
    for statement in &ast.statements {
        match &statement.kind {
            ASTStatementKind::Block(ast) => {
                let new_scope = scope_table.new_scope(statement.id, Some(scope));
                build_references_and_scopes_stmt_block(
                    new_scope,
                    scope_table,
                    reference_table,
                    unresolved_table,
                    function,
                    ast,
                    diagnostics,
                );
            }
            ASTStatementKind::Let(ast) => {
                if let Some(let_type) = &ast.let_type {
                    let type_ref =
                        resolve_type_reference(unresolved_table, &let_type.typename, diagnostics);
                    reference_table
                        .type_references
                        .insert(let_type.typename.id, type_ref);
                }

                if let Some(let_assignment) = &ast.let_assignment {
                    build_references_and_scopes_expression(
                        scope,
                        scope_table,
                        reference_table,
                        unresolved_table,
                        function,
                        &let_assignment.expression,
                        diagnostics,
                    );
                }

                let variable = ScopeVariable::new(statement.id, ast.name, statement.span);
                scope_table
                    .scopes
                    .get_mut(&scope)
                    .unwrap()
                    .new_variable(variable);
            }
            ASTStatementKind::If(ast) => {
                build_references_and_scopes_expression(
                    scope,
                    scope_table,
                    reference_table,
                    unresolved_table,
                    function,
                    &ast.expression,
                    diagnostics,
                );
                build_references_and_scopes_stmt_block(
                    scope,
                    scope_table,
                    reference_table,
                    unresolved_table,
                    function,
                    &ast.body_block,
                    diagnostics,
                );

                for ast in &ast.single_else_ifs {
                    build_references_and_scopes_expression(
                        scope,
                        scope_table,
                        reference_table,
                        unresolved_table,
                        function,
                        &ast.expression,
                        diagnostics,
                    );
                    build_references_and_scopes_stmt_block(
                        scope,
                        scope_table,
                        reference_table,
                        unresolved_table,
                        function,
                        &ast.body_block,
                        diagnostics,
                    );
                }

                if let Some(ast) = &ast.single_else {
                    build_references_and_scopes_stmt_block(
                        scope,
                        scope_table,
                        reference_table,
                        unresolved_table,
                        function,
                        &ast.body_block,
                        diagnostics,
                    );
                }
            }
            ASTStatementKind::Loop(ast) => {
                build_references_and_scopes_stmt_block(
                    scope,
                    scope_table,
                    reference_table,
                    unresolved_table,
                    function,
                    &ast.body_block,
                    diagnostics,
                );
            }
            ASTStatementKind::While(ast) => {
                build_references_and_scopes_expression(
                    scope,
                    scope_table,
                    reference_table,
                    unresolved_table,
                    function,
                    &ast.expression,
                    diagnostics,
                );
                build_references_and_scopes_stmt_block(
                    scope,
                    scope_table,
                    reference_table,
                    unresolved_table,
                    function,
                    &ast.body_block,
                    diagnostics,
                );
            }
            ASTStatementKind::Break(..) => {}
            ASTStatementKind::Continue(..) => {}
            ASTStatementKind::Return(ast) => {
                if let Some(expression) = &ast.expression {
                    build_references_and_scopes_expression(
                        scope,
                        scope_table,
                        reference_table,
                        unresolved_table,
                        function,
                        expression,
                        diagnostics,
                    );
                }
            }
            ASTStatementKind::Assignment(ast) => {
                build_references_and_scopes_expression(
                    scope,
                    scope_table,
                    reference_table,
                    unresolved_table,
                    function,
                    &ast.left,
                    diagnostics,
                );
                build_references_and_scopes_expression(
                    scope,
                    scope_table,
                    reference_table,
                    unresolved_table,
                    function,
                    &ast.right,
                    diagnostics,
                );
            }
            ASTStatementKind::Row(ast) => {
                build_references_and_scopes_expression(
                    scope,
                    scope_table,
                    reference_table,
                    unresolved_table,
                    function,
                    &ast.expression,
                    diagnostics,
                );
            }
        }
    }
}

pub fn build_references_and_scopes_expression(
    scope: ScopeId,
    scope_table: &ScopeTable,
    reference_table: &mut ReferenceTable,
    unresolved_table: &UnresolvedTopLevelTable,
    function: &UnresolvedFunction,
    ast: &ASTExpression,
    diagnostics: &DiagnosticsSender,
) {
    match &ast.kind {
        ASTExpressionKind::Binary(ast) => {
            build_references_and_scopes_expression(
                scope,
                scope_table,
                reference_table,
                unresolved_table,
                function,
                &ast.left,
                diagnostics,
            );
            build_references_and_scopes_expression(
                scope,
                scope_table,
                reference_table,
                unresolved_table,
                function,
                &ast.right,
                diagnostics,
            );
        }
        ASTExpressionKind::Unary(ast) => {
            build_references_and_scopes_expression(
                scope,
                scope_table,
                reference_table,
                unresolved_table,
                function,
                &ast.right,
                diagnostics,
            );
        }
        ASTExpressionKind::As(ast) => {
            let type_ref = resolve_type_reference(unresolved_table, &ast.typename, diagnostics);
            reference_table
                .type_references
                .insert(ast.typename.id, type_ref);

            build_references_and_scopes_expression(
                scope,
                scope_table,
                reference_table,
                unresolved_table,
                function,
                &ast.expression,
                diagnostics,
            );
        }
        ASTExpressionKind::Call(ast) => {
            build_references_and_scopes_expression(
                scope,
                scope_table,
                reference_table,
                unresolved_table,
                function,
                &ast.expression,
                diagnostics,
            );

            for arg in &ast.args {
                build_references_and_scopes_expression(
                    scope,
                    scope_table,
                    reference_table,
                    unresolved_table,
                    function,
                    &arg.expression,
                    diagnostics,
                );
            }
        }
        ASTExpressionKind::Member(ast) => {
            build_references_and_scopes_expression(
                scope,
                scope_table,
                reference_table,
                unresolved_table,
                function,
                &ast.expression,
                diagnostics,
            );
        }
        ASTExpressionKind::Paren(ast) => {
            build_references_and_scopes_expression(
                scope,
                scope_table,
                reference_table,
                unresolved_table,
                function,
                &ast.expression,
                diagnostics,
            );
        }
        ASTExpressionKind::Literal(..) => {}
        ASTExpressionKind::IdReference(ast) => {
            if let Some(symbol_reference) = resolve_symbol_reference(
                scope,
                scope_table,
                unresolved_table,
                function,
                ast,
                diagnostics,
            ) {
                reference_table
                    .symbol_references
                    .insert(ast.id, symbol_reference);
            }
        }
        ASTExpressionKind::StructLiteral(ast) => {
            let type_ref = resolve_type_reference(unresolved_table, &ast.typename, diagnostics);

            if type_ref.kind.is_user_struct() {
                reference_table
                    .type_references
                    .insert(ast.typename.id, type_ref);
            } else {
                diagnostics.error(
                    ast.typename.span,
                    format!("expected a struct type, found `{}`", type_ref.kind),
                );
            }

            for field in &ast.fields {
                build_references_and_scopes_expression(
                    scope,
                    scope_table,
                    reference_table,
                    unresolved_table,
                    function,
                    &field.expression,
                    diagnostics,
                );
            }
        }
    }
}

fn resolve_type_reference(
    unresolved_table: &UnresolvedTopLevelTable,
    ast: &Typename,
    diagnostics: &DiagnosticsSender,
) -> TypeReference {
    let type_kind = typename_to_type_kind(unresolved_table, ast, diagnostics);
    TypeReference::new(ast.id, type_kind, ast.span)
}

fn resolve_symbol_reference(
    scope: ScopeId,
    scope_table: &ScopeTable,
    unresolved_table: &UnresolvedTopLevelTable,
    function: &UnresolvedFunction,
    ast: &ASTIdReference,
    diagnostics: &DiagnosticsSender,
) -> Option<SymbolReference> {
    let mut next_scope_id = Some(scope);

    while let Some(scope_id) = next_scope_id {
        let scope = scope_table.scopes.get(&scope_id).unwrap();

        if let Some(index) = scope
            .variables
            .iter()
            .rposition(|variable| variable.name.symbol == ast.reference.symbol)
        {
            return Some(SymbolReference::new(
                ast.id,
                ast.reference,
                SymbolReferenceKind::variable(function.id, scope_id, index),
                ast.span,
            ));
        }

        next_scope_id = scope.parent;
    }

    if let Some(index) = function
        .param_names
        .iter()
        .position(|param| *param == ast.reference.symbol)
    {
        return Some(SymbolReference::new(
            ast.id,
            ast.reference,
            SymbolReferenceKind::param(function.id, index),
            ast.span,
        ));
    }

    if let Some(function) = unresolved_table.lookup_function(ast.reference.symbol) {
        return Some(SymbolReference::new(
            ast.id,
            ast.reference,
            SymbolReferenceKind::function(function.id),
            ast.span,
        ));
    }

    diagnostics.error(
        ast.span,
        format!("{} is not declared", ast.reference.symbol),
    );
    None
}
