mod context;
mod write_diagnostics;

use colored::Colorize;
use context::Context;
use ex_backend_llvm::Backend;
use ex_diagnostics::DiagnosticsLevel;
use ex_span::SourceMap;
use std::sync::mpsc::channel;
use write_diagnostics::write_diagnostics;

fn main() {
    let mut source = SourceMap::new();
    let (diagnostics, contexts) = {
        let content = include_str!("../test/basic.ex");
        let file = source.add_source_file(content, "basic.ex", Some("../test/basic.ex"));

        let (sender, receiver) = channel();
        let diagnostics = sender;

        let context = Context::compile(file, diagnostics);
        (receiver, vec![context])
    };

    let mut has_error = false;

    while let Ok(diagnostics) = diagnostics.recv() {
        if diagnostics.level == DiagnosticsLevel::Error {
            has_error = true;
        }

        write_diagnostics(&diagnostics);
    }

    if has_error {
        eprintln!("{}", "compilation failed".red());
        return;
    }

    for context in contexts {
        let program = context.codegen();
        let backend = Backend::new();
        let module = backend.compile(&program);

        if let Err(err) = module.verify() {
            eprintln!("{}", err);
        }

        // backend.optimize(&module);
        module.print_to_stderr();
        // backend.execute(&module, "main");
    }
}
