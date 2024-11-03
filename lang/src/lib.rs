use std::{cell::RefCell, fmt::{Display, Formatter}, rc::Rc};

use error::RuntimeError;
use lantern_parse::{ast::{Block, LanternType}, tokenizer::KeywordKind};
use record::{LanternAny, LanternCustomRecord, LanternMethod, LanternRecord};
use scope::Scope;

pub mod record;
pub mod module;
pub mod scope;
pub mod error;

pub type ScopeMut = Rc<RefCell<Scope>>;

#[derive(Debug, Clone, PartialEq)]
pub enum LanternValue {
    String(String),
    Num(f64),
    Bool(bool),
    Option(Option<Box<LanternValue>>),
    Result(Result<Box<LanternValue>, Box<LanternValue>>),
    Custom(LanternCustomRecord),
    Null,
}

impl Display for LanternValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(str) => f.write_str(str),
            Self::Num(num) => num.fmt(f),
            Self::Bool(bool) => bool.fmt(f),
            Self::Custom(custom) => custom.fmt(f),
            Self::Option(Some(value)) => write!(f, "some({value})"),
            Self::Option(None) => write!(f, "none"),
            Self::Result(Ok(ok)) => write!(f, "ok({ok})"),
            Self::Result(Err(err)) => write!(f, "err({err})"),
            Self::Null => write!(f, "null"),
        }
    }
}

impl LanternValue {
    pub fn r#type(&self) -> LanternType {
        match self {
            Self::String(_) => LanternType::String,
            Self::Num(_) => LanternType::Num,
            Self::Bool(_) => LanternType::Bool,
            Self::Custom(LanternCustomRecord { r#type, .. }) => r#type.clone(),
            Self::Option(option) => LanternType::Option(option.as_ref().map(|value| Box::new(value.r#type()))),
            Self::Result(Ok(ok)) => LanternType::Result(Some(Box::new(ok.r#type())), None),
            Self::Result(Err(err)) => LanternType::Result(None, Some(Box::new(err.r#type()))),
            Self::Null => LanternType::Nil,
        }
    }

    pub fn field(&self, name: &str) -> Option<LanternValue> {
        match self {
            Self::String(str) => String::field(name).map(|field| (field.get)(str)),
            Self::Num(num) => f64::field(name).map(|field| (field.get)(num)),
            Self::Bool(bool) => bool::field(name).map(|field| (field.get)(bool)),
            // TODO: get rid of clone
            Self::Option(option) => Option::field(name).map(|field| (field.get)(&option.as_ref().map(|value| LanternAny(*value.clone())))),
            Self::Result(result) => Result::field(name).map(|field| (field.get)(&result.as_ref()
                    .map(|ok| LanternAny(*ok.clone()))
                    .map_err(|err| LanternAny(*err.clone())))),
            Self::Custom(custom) => custom.fields.get(name).cloned(),
            Self::Null => <()>::field(name).map(|field| (field.get)(&())),
        }
    }

    pub fn method(&self, name: &str) -> Option<LanternMethod> {
        match self {
            Self::String(_) => String::method(name),
            Self::Num(_) => f64::method(name),
            Self::Bool(_) => bool::method(name),
            Self::Option(_) => <Option<LanternAny>>::method(name),
            Self::Result(_) => <Result<LanternAny, LanternAny>>::method(name),
            Self::Custom(custom) => custom.methods.get(name).cloned(),
            Self::Null => <()>::method(name),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LanternVariable {
    pub name: String,
    pub r#type: LanternType,
    pub value: LanternValue,
}

impl LanternVariable {
    pub fn new(name: impl ToString, value: LanternValue) -> Self {
        Self { name: name.to_string(), r#type: value.r#type(), value }
    }
}

#[derive(Clone, PartialEq)]
pub struct LanternFunction {
    pub name: String,
    pub args: Vec<LanternFunctionArg>,
    pub ret_type: LanternType,
    pub body: LanternFunctionBody,
    pub scope: ScopeMut,
}

impl core::fmt::Debug for LanternFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LanternFunction")
            .field("name", &self.name)
            .field("args", &self.args)
            .field("ret_type", &self.ret_type)
            .field("body", &self.body)
            .finish()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LanternFunctionArg {
    pub name: String,
    pub r#type: LanternType,
}

#[derive(Clone, PartialEq)]
pub enum LanternFunctionBody {
    Native(fn(ScopeMut) -> Result<LanternValue, RuntimeError>),
    Custom(Block),
}

impl core::fmt::Debug for LanternFunctionBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Native(_) => f.debug_struct("Native").finish(),
            Self::Custom(_) => f.debug_struct("Custom").finish(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ReturnType<T> {
    Return(T),
    Break,
    Continue,
    None,
}

impl<T> ReturnType<T> {
    pub fn keyword(&self) -> Option<KeywordKind> {
        match self {
            Self::Return(_) => Some(KeywordKind::Ret),
            Self::Break => Some(KeywordKind::Break),
            Self::Continue => Some(KeywordKind::Cont),
            Self::None => None,
        }
    }
}

