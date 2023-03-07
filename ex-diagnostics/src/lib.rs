use ex_span::{SourceFile, Span};
use std::sync::{mpsc::Sender, Arc};

#[derive(Debug, Clone)]
pub struct DiagnosticsSender {
    file: Arc<SourceFile>,
    sender: Sender<Diagnostics>,
}

impl DiagnosticsSender {
    pub fn new(file: Arc<SourceFile>, sender: Sender<Diagnostics>) -> Self {
        Self { file, sender }
    }

    pub fn hint(&self, span: Span, message: String) {
        self.sender
            .send(Diagnostics {
                level: DiagnosticsLevel::Hint,
                message,
                origin: Some(DiagnosticsOrigin {
                    file: self.file.clone(),
                    span,
                }),
                sub_diagnostics: vec![],
            })
            .unwrap();
    }

    pub fn hint_sub(&self, span: Span, message: String, sub_diagnostics: Vec<SubDiagnostics>) {
        self.sender
            .send(Diagnostics {
                level: DiagnosticsLevel::Hint,
                message,
                origin: Some(DiagnosticsOrigin {
                    file: self.file.clone(),
                    span,
                }),
                sub_diagnostics,
            })
            .unwrap();
    }

    pub fn hint_simple(&self, message: String) {
        self.sender
            .send(Diagnostics {
                level: DiagnosticsLevel::Hint,
                message,
                origin: None,
                sub_diagnostics: vec![],
            })
            .unwrap()
    }

    pub fn warning(&self, span: Span, message: String) {
        self.sender
            .send(Diagnostics {
                level: DiagnosticsLevel::Warning,
                message,
                origin: Some(DiagnosticsOrigin {
                    file: self.file.clone(),
                    span,
                }),
                sub_diagnostics: vec![],
            })
            .unwrap()
    }

    pub fn warning_sub(&self, span: Span, message: String, sub_diagnostics: Vec<SubDiagnostics>) {
        self.sender
            .send(Diagnostics {
                level: DiagnosticsLevel::Warning,
                message,
                origin: Some(DiagnosticsOrigin {
                    file: self.file.clone(),
                    span,
                }),
                sub_diagnostics,
            })
            .unwrap()
    }

    pub fn warning_simple(&self, message: String) {
        self.sender
            .send(Diagnostics {
                level: DiagnosticsLevel::Warning,
                message,
                origin: None,
                sub_diagnostics: vec![],
            })
            .unwrap()
    }

    pub fn error(&self, span: Span, message: String) {
        self.sender
            .send(Diagnostics {
                level: DiagnosticsLevel::Error,
                message,
                origin: Some(DiagnosticsOrigin {
                    file: self.file.clone(),
                    span,
                }),
                sub_diagnostics: vec![],
            })
            .unwrap()
    }

    pub fn error_sub(&self, span: Span, message: String, sub_diagnostics: Vec<SubDiagnostics>) {
        self.sender
            .send(Diagnostics {
                level: DiagnosticsLevel::Error,
                message,
                origin: Some(DiagnosticsOrigin {
                    file: self.file.clone(),
                    span,
                }),
                sub_diagnostics,
            })
            .unwrap()
    }

    pub fn error_simple(&self, message: String) {
        self.sender
            .send(Diagnostics {
                level: DiagnosticsLevel::Error,
                message,
                origin: None,
                sub_diagnostics: vec![],
            })
            .unwrap()
    }

    pub fn sub_hint(&self, span: Span, message: String) -> SubDiagnostics {
        SubDiagnostics {
            level: DiagnosticsLevel::Hint,
            message,
            origin: Some(DiagnosticsOrigin {
                file: self.file.clone(),
                span,
            }),
        }
    }

    pub fn sub_hint_simple(&self, message: String) -> SubDiagnostics {
        SubDiagnostics {
            level: DiagnosticsLevel::Hint,
            message,
            origin: None,
        }
    }

    pub fn sub_warning(&self, span: Span, message: String) -> SubDiagnostics {
        SubDiagnostics {
            level: DiagnosticsLevel::Warning,
            message,
            origin: Some(DiagnosticsOrigin {
                file: self.file.clone(),
                span,
            }),
        }
    }

    pub fn sub_warning_simple(&self, message: String) -> SubDiagnostics {
        SubDiagnostics {
            level: DiagnosticsLevel::Warning,
            message,
            origin: None,
        }
    }

    pub fn sub_error(&self, span: Span, message: String) -> SubDiagnostics {
        SubDiagnostics {
            level: DiagnosticsLevel::Error,
            message,
            origin: Some(DiagnosticsOrigin {
                file: self.file.clone(),
                span,
            }),
        }
    }

    pub fn sub_error_simple(&self, message: String) -> SubDiagnostics {
        SubDiagnostics {
            level: DiagnosticsLevel::Error,
            message,
            origin: None,
        }
    }
}

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
