mod cfg_checking;
mod check_types;
pub mod hir;
mod lvalue_checking;
mod resolve;
mod type_inferencing;

use cfg_checking::build_cfg;
use check_types::check_types;
use ex_diagnostics::DiagnosticsSender;
use ex_parser::ASTProgram;
use lvalue_checking::build_lvalues;
use resolve::{build_references, build_top_levels, build_unresolved_top_levels};
use type_inferencing::{build_type_contraint_table, build_type_table};

pub fn check_semantics(ast: &ASTProgram, diagnostics: &DiagnosticsSender) {
    let unresolved_top_level_table = build_unresolved_top_levels(ast, diagnostics);
    let top_level_table = build_top_levels(&unresolved_top_level_table, diagnostics);
    let reference_table = build_references(&unresolved_top_level_table, ast, diagnostics);
    let type_constraint_table = build_type_contraint_table(&top_level_table, &reference_table, ast);
    let type_table = build_type_table(&top_level_table, type_constraint_table);
    check_types(&type_table, &top_level_table, ast, diagnostics);
    let lvalue_table = build_lvalues(&reference_table, &type_table, ast, diagnostics);
    let cfg = build_cfg(
        &top_level_table,
        &reference_table,
        &type_table,
        &lvalue_table,
        ast,
        diagnostics,
    );
}
