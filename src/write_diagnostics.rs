use colored::{ColoredString, Colorize};
use ex_diagnostics::{Diagnostics, DiagnosticsLevel, DiagnosticsOrigin};

pub fn write_diagnostics(diagnostics: &Diagnostics) {
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

    eprintln!();
}

fn write_diagnostics_origin(origin: &DiagnosticsOrigin) {
    if let Some(path) = origin.file.path() {
        let line_col = origin.file.find_line_col(origin.span.low);
        eprintln!(
            "{}",
            format!(
                "at {}:{}:{}",
                path.display(),
                line_col.line + 1,
                line_col.column + 1
            )
            .bold()
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

    // let visual_span = Span::new(
    //     origin.file.line_positions()[line_low as usize],
    //     origin.file.line_positions()[line_high as usize + 1],
    // );

    for line in line_low..=line_high {
        let line_str = origin.file.slice_line(line);
        let line_low_pos = origin.file.line_positions()[line as usize];

        // let line_str = if origin.span.contains_span(line_span) {
        //     &[line_str.bold()]
        // } else if line_span.contains_span(origin.span) {
        //     &[line_str[(origin.span.low - line_span.low).get() as usize
        //         ..(origin.span.high - line_span.low).get() as usize]
        //         .bold()]
        // } else {
        //     &[line_str.normal()]
        // };

        eprint!("{:>width$} | ", line + 1, width = max_line_number_width + 1);

        for (i, c) in line_str.chars().enumerate() {
            if origin.span.contains_pos(line_low_pos + i as u32) {
                eprint!("{}", c.to_string().magenta());
            } else {
                eprint!("{}", c.to_string().normal());
            }
        }

        eprintln!();

        // eprintln!(
        //     "{:>width$} | {}",
        //     line + 1,
        //     line_str,
        //     width = max_line_number_width + 1
        // );
    }
}

fn select_color(level: DiagnosticsLevel, str: &str) -> ColoredString {
    match level {
        DiagnosticsLevel::Error => format!("{} {}", "error:".bold(), str).red(),
        DiagnosticsLevel::Warning => format!("{} {}", " warn:".bold(), str).yellow(),
        DiagnosticsLevel::Hint => format!("{} {}", " hint:".bold(), str).bright_green(),
    }
}
