use std::{error::Error, fmt::{Display, Formatter}};

use itertools::Itertools;

use crate::ast::LanternType;

#[macro_export]
macro_rules! diagnostic {
    ($kind: expr, $span: expr => $err: expr) => {{
        use $crate::error::DiagnosticKind::*;
        $crate::error::Diagnostic::new($kind, $span.into(), $err.into())
    }};

    ($kind: expr, $span: expr, $($tt: tt)+) => {{
        use $crate::error::DiagnosticKind::*;
        $crate::error::Diagnostic::new($kind, $span.into(), anyhow::anyhow!($($tt)+).into())
    }};
}

#[non_exhaustive]
#[derive(thiserror::Error, Default, Debug)]
pub struct Diagnostics {
    pub diagnostics: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn new(diagnostics: Vec<Diagnostic>) -> Self {
        Self { diagnostics }
    }

    pub fn extend(&mut self, other: Diagnostics) {
        self.diagnostics.extend(other.diagnostics);
    }
}

impl Display for Diagnostics {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.diagnostics.iter()
            .map(|err| err.to_string())
            .intersperse("\n".to_string())
            .try_for_each(|err| f.write_str(&err))
    }
}

impl From<Diagnostic> for Diagnostics {
    fn from(value: Diagnostic) -> Self {
        Self { diagnostics: vec![value] }
    }
}

impl From<Vec<Diagnostic>> for Diagnostics {
    fn from(value: Vec<Diagnostic>) -> Self {
        Self { diagnostics: value }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DiagnosticKind {
    Error,
    Warning,
    Hint,
}

#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub struct Diagnostic {
    pub kind: DiagnosticKind,
    pub span: Span,
    pub error: Box<dyn Error>,
}

impl Diagnostic {
    pub fn new(kind: DiagnosticKind, span: Span, error: Box<dyn Error>) -> Self {
        Self { kind, span, error }
    }
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = match self.kind {
            DiagnosticKind::Error => "error",
            DiagnosticKind::Warning => "warning",
            DiagnosticKind::Hint => "hint",
        };

        // TODO: formatting
        let line = match self.span {
            Span::Point(Some(location)) => location.line.to_string(),
            Span::Point(None) => "<eof>".to_string(),
            Span::Range(from, ..) => from.line.to_string(),
        };
        write!(f, "{name} on line {}: {}", line, self.error)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Span {
    Point(Option<Location>),
    Range(Location, Option<Location>),
}

impl From<Location> for Span {
    fn from(value: Location) -> Self {
        Self::Point(Some(value))
    }
}

// TODO: ord impl
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub struct Location {
    pub line: usize,
    pub col: usize,
}

impl Location {
    pub fn new(line: usize, col: usize) -> Location {
        Self { line, col }
    }
}

impl Default for Location {
    fn default() -> Self {
        Self { line: 1, col: 1 }
    }
}

impl From<(usize, usize)> for Location {
    fn from(value: (usize, usize)) -> Self {
        Self { line: value.0, col: value.1 }
    }
}

#[derive(thiserror::Error, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[error("unexpected token")]
pub struct UnexpectedTokenError;

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[error("expected {0}")]
pub struct ExpectedError(pub String);

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[error("invalid {0}")]
pub struct InvalidTokenError(pub String);

#[derive(thiserror::Error, Debug, Clone, PartialEq)]
#[error("mismatched types, expected {expected}, but found {found}")]
pub struct MismatchedTypesError {
    pub expected: LanternType,
    pub found: LanternType,
}

#[derive(thiserror::Error, Debug)]
pub enum Recoverable<E: Error> {
    #[error(transparent)]
    Fatal(E),
    #[error(transparent)]
    Nonfatal(E),
}

impl<E: Error> Recoverable<E> {
    pub fn into_fatal(self) -> Self {
        Self::Fatal(self.into_err())
    }

    pub fn err(&self) -> &E {
        match self {
            Recoverable::Fatal(err) => err,
            Recoverable::Nonfatal(err) => err,
        }
    }

    pub fn into_err(self) -> E {
        match self {
            Recoverable::Fatal(err) => err,
            Recoverable::Nonfatal(err) => err,
        }
    }
}

