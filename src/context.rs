use ex_codegen::{codegen, Program};
use ex_diagnostics::{Diagnostics, DiagnosticsSender};
use ex_optimization::eliminate_dead_code;
use ex_parser::{parse_ast, ASTProgram};
use ex_semantic_checking::{
    build_cfg, build_hir, build_references, build_top_levels, build_type_constraint_table,
    build_type_table, build_unresolved_top_levels, check_cfg, check_lvalues, check_types,
    ControlFlowGraph, HIRProgram, ReferenceTable, TopLevelTable, TypeTable,
};
use ex_span::SourceFile;
use std::sync::{mpsc::Sender, Arc};

pub struct Context {
    pub ast: ASTProgram,
    pub top_level_table: TopLevelTable,
    pub reference_table: ReferenceTable,
    pub hir: HIRProgram,
    pub type_table: TypeTable,
    pub cfg: ControlFlowGraph,
}

impl Context {
    pub fn compile(file: Arc<SourceFile>, diagnostics: Sender<Diagnostics>) -> Option<Self> {
        let diagnostics = DiagnosticsSender::new(file.clone(), diagnostics);
        let (ast, mut id_alloc) = parse_ast(file.clone(), &diagnostics);

        if diagnostics.has_error() {
            return None;
        }

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

        Some(Self {
            ast,
            top_level_table,
            reference_table,
            hir,
            type_table,
            cfg,
        })
    }

    pub fn codegen(self) -> Program {
        let mut program = codegen(
            &self.top_level_table,
            &self.reference_table,
            &self.type_table,
            &self.hir,
        );

        for (_, function) in &mut program.functions {
            eliminate_dead_code(function);
        }

        program
    }
}
