use ex_span::{SourceFile, Span};
use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct Diagnostics {
    pub level: DiagnosticsLevel,
    pub message: String,
    pub origin: Option<DiagnosticsOrigin>,
    pub sub_diagnostics: Vec<SubDiagnostics>,
}

#[derive(Debug, Clone)]
pub struct SubDiagnostics {
    pub level: DiagnosticsLevel,
    pub message: String,
    pub origin: Option<DiagnosticsOrigin>,
}

#[derive(Debug, Clone)]
pub struct DiagnosticsOrigin {
    pub file: Arc<SourceFile>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DiagnosticsLevel {
    Hint,
    Warning,
    Error,
}
