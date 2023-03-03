mod context;
mod write_diagnostics;

use colored::Colorize;
use context::Context;
use ex_diagnostics::DiagnosticsLevel;
use ex_span::SourceMap;
use ex_vm::execute;
use std::sync::{mpsc::channel, Arc};
use write_diagnostics::write_diagnostics;

fn main() {
    let mut source = SourceMap::new();
    let (diagnostics, contexts) = {
        let content = include_str!("../test/test.ex");
        let file = source.add_source_file(content, "test.ex", Some("../test/test.ex"));

        let (sender, receiver) = channel();
        let diagnostics = Arc::new(sender);

        let context = Context::compile(file, diagnostics.clone());
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
        execute(&program);
    }
}
