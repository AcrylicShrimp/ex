use crate::{Pos, SourceFile, Span};
use std::{path::PathBuf, sync::Arc};

#[derive(Default, Debug)]
pub struct SourceMap {
    files: Vec<Arc<SourceFile>>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn end_pos(&self) -> Pos {
        self.files
            .last()
            .map_or(Pos::ZERO, |source| source.span().high)
    }

    pub fn add_source_file(
        &mut self,
        content: impl Into<String>,
        name: impl Into<String>,
        path: Option<PathBuf>,
    ) -> Arc<SourceFile> {
        let content = content.into();
        debug_assert!(content.len() < u32::MAX as usize);

        let low = self.end_pos();
        let high = low + content.len() as u32;
        let file = Arc::new(SourceFile::new(
            Span::new(low, high),
            content,
            name.into(),
            path,
        ));
        self.files.push(file.clone());
        file
    }
}
