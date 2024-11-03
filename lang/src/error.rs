use std::{error::Error, fmt::{Display, Formatter}};

use crate::{LanternType, ReturnType};

#[macro_export]
macro_rules! runtime_error {
    ($($arg: tt)*) => {
        RuntimeError::new(anyhow::anyhow!($($arg)*))
    };
}

#[derive(thiserror::Error, Debug)]
#[error("Runtime error: {err}")]
pub struct RuntimeError {
    pub err: Box<dyn Error>,
}

impl RuntimeError {
    pub fn new(err: impl Into<Box<dyn Error>>) -> Self {
        Self { err: err.into() }
    }
}

#[derive(thiserror::Error, Debug, Clone, PartialEq)]
pub struct InvalidReturnType(pub ReturnType);

impl Display for InvalidReturnType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            ReturnType::None => f.write_str("expected return"),
            ret_type => write!(f, "`{}` not allowed here", ret_type.keyword().expect("return type is not none")),
        }
    }
}

#[derive(thiserror::Error, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnknownItem {
    Variable,
    Function,
    Type,
    Field,
    Method,
    Record,
    Module,
}

impl Display for UnknownItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let item = match self {
            Self::Variable => "variable",
            Self::Function => "function",
            Self::Type => "type",
            Self::Field => "field",
            Self::Method => "method",
            Self::Record => "record",
            Self::Module => "module",
        };
        write!(f, "unknown {item}")
    }
}

#[derive(thiserror::Error, Debug, Clone, PartialEq)]
#[error("mismatched types: expected {0}, but got {1}")]
pub struct MismatchedTypes(pub LanternType, pub LanternType);

