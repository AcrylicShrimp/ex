use ex_codegen::{codegen, Program};
use ex_control_flow_checking::check_control_flow;
use ex_diagnostics::{Diagnostics, DiagnosticsSender};
use ex_optimization::eliminate_dead_code;
use ex_parser::{parse_ast, ASTProgram};
use ex_resolve_ref::{
    resolve_ast, AssignmentLhsTable, FunctionTable, SymbolReferenceTable, TypeReferenceTable,
    UserTypeTable,
};
use ex_span::SourceFile;
use ex_type_checking::{check_types, propagate_type_variables, TypeTable};
use std::sync::{mpsc::Sender, Arc};

pub struct Context {
    pub ast: ASTProgram,
    pub function_table: FunctionTable,
    pub user_type_table: UserTypeTable,
    pub symbol_reference_table: SymbolReferenceTable,
    pub type_reference_table: TypeReferenceTable,
    pub assignment_lhs_table: AssignmentLhsTable,
    pub type_table: TypeTable,
}

impl Context {
    pub fn compile(file: Arc<SourceFile>, diagnostics: Sender<Diagnostics>) -> Self {
        let old_diagnostics = Arc::new(diagnostics.clone());
        let (ast, mut id_alloc) = parse_ast(file.clone(), old_diagnostics.clone());

        {
            use ex_semantic_checking::{
                build_cfg, build_hir, build_references, build_top_levels,
                build_type_constraint_table, build_type_table, build_unresolved_top_levels,
                check_cfg, check_lvalues, check_types,
            };

            let diagnostics = DiagnosticsSender::new(file.clone(), diagnostics);
            let unresolved_top_level_table = build_unresolved_top_levels(&ast, &diagnostics);
            let top_level_table = build_top_levels(&unresolved_top_level_table, &diagnostics);
            let reference_table = build_references(&unresolved_top_level_table, &ast, &diagnostics);
            let hir = build_hir(
                &mut id_alloc,
                &top_level_table,
                &reference_table,
                &ast,
                &diagnostics,
            );

            let type_constraint_table =
                build_type_constraint_table(&top_level_table, &reference_table, &hir);
            let type_table = build_type_table(&top_level_table, type_constraint_table);
            check_types(&type_table, &top_level_table, &hir, &diagnostics);
            check_lvalues(&type_table, &hir, &diagnostics);

            let mut cfg = build_cfg(&type_table, &hir, &diagnostics);
            check_cfg(&mut cfg, &top_level_table, &diagnostics);
        }

        // TODO: Replace below with a newly designed semantic checking module.
        let (
            function_table,
            user_type_table,
            symbol_reference_table,
            type_reference_table,
            assignment_lhs_table,
        ) = resolve_ast(&ast, &file, &old_diagnostics);

        let type_table_builder = propagate_type_variables(
            &function_table,
            &user_type_table,
            &symbol_reference_table,
            &type_reference_table,
            &ast,
        );

        let type_table = type_table_builder.resolve(&user_type_table, &type_reference_table);

        check_types(
            &type_table,
            &type_reference_table,
            &user_type_table,
            &ast,
            &file,
            &old_diagnostics,
        );

        check_control_flow(
            &function_table,
            &type_table,
            &user_type_table,
            &assignment_lhs_table,
            &symbol_reference_table,
            &ast,
            &file,
            &old_diagnostics,
        );

        Self {
            ast,
            function_table,
            user_type_table,
            symbol_reference_table,
            type_reference_table,
            assignment_lhs_table,
            type_table,
        }
    }

    pub fn codegen(self) -> Program {
        let mut program = codegen(
            &self.ast,
            &self.function_table,
            &self.type_table,
            &self.type_reference_table,
            &self.symbol_reference_table,
            &self.assignment_lhs_table,
        );

        for (_, function) in &mut program.functions {
            eliminate_dead_code(function);
        }

        program
    }
}
