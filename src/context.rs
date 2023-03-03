use ex_codegen::{codegen, Program};
use ex_control_flow_checking::check_control_flow;
use ex_diagnostics::Diagnostics;
use ex_optimization::eliminate_dead_code;
use ex_parser::{parse_ast, ASTProgram};
use ex_resolve_ref::{
    resolve_ast, AssignmentLhsTable, FunctionTable, SymbolReferenceTable, TypeReferenceTable,
};
use ex_span::SourceFile;
use ex_type_checking::{check_types, propagate_type_variables, TypeTable};
use std::sync::{mpsc::Sender, Arc};

pub struct Context {
    pub ast: ASTProgram,
    pub function_table: FunctionTable,
    pub symbol_reference_table: SymbolReferenceTable,
    pub type_reference_table: TypeReferenceTable,
    pub assignment_lhs_table: AssignmentLhsTable,
    pub type_table: TypeTable,
}

impl Context {
    pub fn compile(file: Arc<SourceFile>, diagnostics: Arc<Sender<Diagnostics>>) -> Self {
        let ast = parse_ast(file.clone(), diagnostics.clone());

        let (function_table, symbol_reference_table, type_reference_table, assignment_lhs_table) =
            resolve_ast(&ast, &file, &diagnostics);

        let type_table_builder = propagate_type_variables(
            &function_table,
            &symbol_reference_table,
            &type_reference_table,
            &ast,
            &file,
            &diagnostics,
        );

        let type_table = type_table_builder.resolve();

        check_types(
            &type_table,
            &type_reference_table,
            &ast,
            &file,
            &diagnostics,
        );

        check_control_flow(
            &function_table,
            &assignment_lhs_table,
            &symbol_reference_table,
            &ast,
            &file,
            &diagnostics,
        );

        Self {
            ast,
            function_table,
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
