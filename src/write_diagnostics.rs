use colored::{ColoredString, Colorize};
use ex_diagnostics::{Diagnostics, DiagnosticsLevel, DiagnosticsOrigin};

pub fn write_diagnostics(diagnostics: &Diagnostics) {
    eprintln!("{}", apply_level(diagnostics.level, &diagnostics.message));

    if let Some(origin) = &diagnostics.origin {
        write_diagnostics_origin(diagnostics.level, &diagnostics.message, origin);
    }

    for sub in &diagnostics.sub_diagnostics {
        eprintln!("{}", apply_level(sub.level, &sub.message));

        if let Some(origin) = &sub.origin {
            write_diagnostics_origin(sub.level, &sub.message, origin);
        }
    }

    eprintln!();
}

fn write_diagnostics_origin(level: DiagnosticsLevel, message: &str, origin: &DiagnosticsOrigin) {
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

    let line_col_low = origin.file.find_line_col(origin.span.low);
    let line_col_high = origin.file.find_line_col(origin.span.high);

    let max_line = (origin.file.line_positions().len() - 1) as u32;

    let line_low = if line_col_low.line == 0 {
        0
    } else {
        line_col_low.line - 1
    };
    let line_high = if line_col_high.line == max_line {
        max_line
    } else {
        line_col_high.line + 1
    };

    let max_line_number_width = ((line_high + 1) as f64).log(10f64).ceil() as usize;

    if line_low != line_col_low.line {
        eprintln!(
            "{:>width$} | {}",
            line_low + 1,
            origin.file.slice_line(line_low),
            width = max_line_number_width + 1
        );
    }

    if line_col_low.line != line_col_high.line {
        let line = origin.file.slice_line(line_col_low.line);
        eprintln!(
            "{:>width$} | {}",
            line_col_low.line + 1,
            line,
            width = max_line_number_width + 1
        );
        eprintln!(
            "{}{}",
            " ".repeat(max_line_number_width + 4 + line_col_low.column as usize),
            apply_level_color(
                level,
                &"^".repeat(line.len() - line_col_low.column as usize)
            )
        );
    }

    if 2 <= line_col_high.line - line_col_low.line {
        eprintln!(" ...");
    }

    if line_col_low.line == line_col_high.line {
        let line = origin.file.slice_line(line_col_high.line);
        eprintln!(
            "{:>width$} | {}",
            line_col_high.line + 1,
            line,
            width = max_line_number_width + 1
        );
        eprintln!(
            "{}{}{}",
            " ".repeat(max_line_number_width + 4 + line_col_low.column as usize),
            apply_level_color(
                level,
                &"^".repeat(line_col_high.column as usize - line_col_low.column as usize)
            ),
            apply_level_color(level, &format!(" {}", message))
        );
    } else {
        let line = origin.file.slice_line(line_col_high.line);
        eprintln!(
            "{:>width$} | {}",
            line_col_high.line + 1,
            line,
            width = max_line_number_width + 1
        );
        eprintln!(
            "{}{}{}",
            " ".repeat(max_line_number_width + 4),
            apply_level_color(
                level,
                &"^".repeat(usize::max(line_col_high.column as usize, 1))
            ),
            apply_level_color(level, &format!(" {}", message))
        );
    }

    if line_high != line_col_high.line {
        eprintln!(
            "{:>width$} | {}",
            line_high + 1,
            origin.file.slice_line(line_high),
            width = max_line_number_width + 1
        );
    }
}

fn apply_level(level: DiagnosticsLevel, str: &str) -> ColoredString {
    match level {
        DiagnosticsLevel::Error => {
            apply_level_color(level, &format!("{} {}", "error:".bold(), str))
        }
        DiagnosticsLevel::Warning => {
            apply_level_color(level, &format!("{} {}", " warn:".bold(), str))
        }
        DiagnosticsLevel::Hint => apply_level_color(level, &format!("{} {}", " hint:".bold(), str)),
    }
}

fn apply_level_color(level: DiagnosticsLevel, str: &str) -> ColoredString {
    match level {
        DiagnosticsLevel::Error => str.red(),
        DiagnosticsLevel::Warning => str.yellow(),
        DiagnosticsLevel::Hint => str.bright_green(),
    }
}
