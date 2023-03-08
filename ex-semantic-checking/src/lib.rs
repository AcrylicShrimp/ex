mod build_reference_table;
mod build_top_levels;
mod build_unresolved_top_levels;
mod type_kind;

pub use build_reference_table::*;
pub use build_top_levels::*;
pub use build_unresolved_top_levels::*;
pub use type_kind::*;

use ex_diagnostics::DiagnosticsSender;
use ex_parser::ASTProgram;

pub fn check_semantics(diagnostics: &DiagnosticsSender, ast: &ASTProgram) {
    let unresolved_top_level_table = build_unresolved_top_levels(ast, diagnostics);
    let top_level_table = build_top_levels(&unresolved_top_level_table, diagnostics);
    let reference_table = build_reference_table(&unresolved_top_level_table, &ast, diagnostics);
}
