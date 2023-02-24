use crate::{LineCol, Pos, Span};
use std::{fmt::Display, path::PathBuf};

#[derive(Debug)]
pub struct SourceFile {
    span: Span,
    content: String,
    line_positions: Vec<Pos>,
    name: String,
    path: Option<PathBuf>,
}

impl SourceFile {
    pub fn new(
        span: Span,
        content: impl Into<String>,
        name: impl Into<String>,
        path: Option<impl Into<PathBuf>>,
    ) -> Self {
        let content = content.into();
        let mut line_positions = vec![span.low];
        line_positions.extend(
            content
                .match_indices('\n')
                .map(|(pos, _)| span.low + Pos::new(pos as u32) + 1),
        );

        Self {
            span,
            content,
            line_positions,
            name: name.into(),
            path: path.map(|path| path.into()),
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn span_begin(&self) -> Span {
        Span::new(self.span.low, self.span.low)
    }

    pub fn span_end(&self) -> Span {
        Span::new(self.span.high, self.span.high)
    }

    pub fn content(&self) -> &String {
        &self.content
    }

    pub fn line_positions(&self) -> &Vec<Pos> {
        &self.line_positions
    }

    pub fn name(&self) -> &String {
        &self.name
    }

    pub fn path(&self) -> Option<&PathBuf> {
        self.path.as_ref()
    }

    pub fn line_span(&self, line: u32) -> Span {
        debug_assert!(line < self.line_positions.len() as u32);

        Span::new(
            self.line_positions[line as usize],
            if line as usize + 1 == self.line_positions.len() {
                self.span.high
            } else {
                self.line_positions[line as usize + 1]
            },
        )
    }

    pub fn find_line(&self, pos: Pos) -> u32 {
        debug_assert!(self.span.contains_pos(pos));

        match self.line_positions.binary_search(&pos) {
            Ok(line) => line as u32,
            Err(line) => line as u32 - 1,
        }
    }

    pub fn find_line_col(&self, pos: Pos) -> LineCol {
        let line = self.find_line(pos);
        let line_span = self.line_span(line);
        LineCol::new(
            line,
            self.slice(line_span)[..(pos - line_span.low).get() as usize]
                .chars()
                .count() as _,
        )
    }

    pub fn slice(&self, span: Span) -> &str {
        debug_assert!(self.span.contains_span(span));

        &self.content
            [(span.low - self.span.low).get() as usize..(span.high - self.span.low).get() as usize]
    }

    pub fn slice_line(&self, line: u32) -> &str {
        debug_assert!(line < self.line_positions.len() as u32);

        let chars = &['\n', '\r'];
        self.slice(self.line_span(line)).trim_end_matches(chars)
    }
}

impl Display for SourceFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.path {
            Some(path) => {
                write!(f, "{} ({})", self.name, path.display())
            }
            None => {
                write!(f, "{}", self.name)
            }
        }
    }
}
