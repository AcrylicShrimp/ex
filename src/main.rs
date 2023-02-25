use colored::{ColoredString, Colorize};
use ex_diagnostics::{Diagnostics, DiagnosticsLevel, DiagnosticsOrigin};
use ex_parser::parse_ast;
use ex_resolve_ref::resolve_ast;
use ex_span::SourceMap;
use regex::Regex;
use std::sync::{mpsc::channel, Arc};

fn main() {
    let content = include_str!("../test/test.ex");
    let mut source = SourceMap::new();
    let file = source.add_source_file(content, "test.ex", Some("../test/test.ex"));

    let (sender, receiver) = channel();

    {
        let diagnostics = Arc::new(sender);
        let ast = parse_ast(file.clone(), diagnostics.clone());

        let ast_string = format!("{:#?}", ast);
        let regex = Regex::new(",\\s*span:\\s*Span\\s*\\{[\\s\\S]+?\\}").unwrap();
        let clean_ast_string = regex.replace_all(&ast_string, "");

        let (function_table, symbol_reference_table) = resolve_ast(&ast, &file, &diagnostics);
    }

    while let Ok(diagnostics) = receiver.recv() {
        write_diagnostics(&diagnostics);
    }

    // println!("{}", clean_ast_string);
}

fn write_diagnostics(diagnostics: &Diagnostics) {
    eprintln!("{}", select_color(diagnostics.level, &diagnostics.message));

    if let Some(origin) = &diagnostics.origin {
        write_diagnostics_origin(origin);
    }

    for sub in &diagnostics.sub_diagnostics {
        eprintln!("{}", select_color(sub.level, &sub.message));

        if let Some(origin) = &sub.origin {
            write_diagnostics_origin(origin);
        }
    }
}

fn write_diagnostics_origin(origin: &DiagnosticsOrigin) {
    if let Some(path) = origin.file.path() {
        let line_col = origin.file.find_line_col(origin.span.low);
        eprintln!(
            "at {}:{}:{}",
            path.display(),
            line_col.line + 1,
            line_col.column + 1
        )
    } else {
        let line_col = origin.file.find_line_col(origin.span.low);
        eprintln!("at ?:{}:{}", line_col.line + 1, line_col.column + 1)
    }

    let line_low = origin.file.find_line(origin.span.low);
    let line_high = origin.file.find_line(origin.span.high);

    let max_line = (origin.file.line_positions().len() - 1) as u32;
    let line_low = if line_low == 0 { 0 } else { line_low - 1 };
    let line_high = if line_high == max_line {
        max_line
    } else {
        line_high + 1
    };

    let max_line_number_width = ((line_high + 1) as f64).log(10f64).ceil() as usize;

    for line in line_low..=line_high {
        eprintln!(
            "{:>width$} | {}",
            line + 1,
            origin.file.slice_line(line),
            width = max_line_number_width + 1
        );
    }
}

fn select_color(level: DiagnosticsLevel, str: &str) -> ColoredString {
    match level {
        DiagnosticsLevel::Error => format!("{} {}", "error:".bold(), str).red(),
        DiagnosticsLevel::Warning => format!("{} {}", " warn:".bold(), str).yellow(),
        DiagnosticsLevel::Hint => format!("{} {}", " hint:".bold(), str).bright_green(),
    }
}
