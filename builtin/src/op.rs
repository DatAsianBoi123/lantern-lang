use std::fmt::{Display, Formatter};

use lantern_lang::{error::RuntimeError, LanternValue};
use lantern_parse::ast::{expr::{BinaryOperator, Operator, UnaryOperator}, LanternType};

pub fn perform_binary_op(left: LanternValue, op: BinaryOperator, right: LanternValue) -> Result<LanternValue, RuntimeError> {
    match op {
        BinaryOperator::Add => {
            match (&left, &right) {
                (LanternValue::Num(l), LanternValue::Num(r)) => Ok(LanternValue::Num(l + r)),
                (LanternValue::String(l), LanternValue::String(r)) => Ok(LanternValue::String(l.clone() + r)),
                _ => Err(RuntimeError::new(InvalidOpTypes::new_binary(op, left.r#type(), right.r#type()))),
            }
        },
        BinaryOperator::Sub => binary_num_op(left, right, op, |l, r| Ok(l - r)),
        BinaryOperator::Mult => binary_num_op(left, right, op, |l, r| Ok(l * r)),
        BinaryOperator::Div => binary_num_op(left, right, op, |l, r| Ok(l / r)),
        BinaryOperator::Mod => binary_num_op(left, right, op, |l, r| Ok(l % r)),

        BinaryOperator::And => binary_bool_op(left, right, op, |l, r| Ok(l && r)),
        BinaryOperator::Or => binary_bool_op(left, right, op, |l, r| Ok(l || r)),

        BinaryOperator::Eq | BinaryOperator::NotEq => {
            if left.r#type() != right.r#type() {
                return Err(RuntimeError::new(InvalidOpTypes::new_binary(op, left.r#type(), right.r#type())));
            };

            let bool = if let BinaryOperator::Eq = op { left == right }
            else { left != right };
            Ok(LanternValue::Bool(bool))
        },

        BinaryOperator::LessThan | BinaryOperator::GreaterThan | BinaryOperator::LessOrEq | BinaryOperator::GreaterOrEq => {
            match (&left, &right) {
                (LanternValue::Num(l), LanternValue::Num(r)) => {
                    let bool = match op {
                        BinaryOperator::LessThan => l < r,
                        BinaryOperator::GreaterThan => l > r,
                        BinaryOperator::LessOrEq => l <= r,
                        BinaryOperator::GreaterOrEq => l >= r,
                        _ => unreachable!(),
                    };
                    Ok(LanternValue::Bool(bool))
                },
                _ => Err(RuntimeError::new(InvalidOpTypes::new_binary(op, left.r#type(), right.r#type()))),
            }
        },

        BinaryOperator::Pipe => {
            let left_type = left.r#type();
            let right_type = right.r#type();
            match (left, right) {
                (LanternValue::Option(Some(_)), right) => Ok(LanternValue::Option(Some(Box::new(right)))),
                (LanternValue::Result(Ok(_)), right) => Ok(LanternValue::Result(Ok(Box::new(right)))),
                (ret @ LanternValue::Option(None) | ret @ LanternValue::Result(Err(_)), _) => Ok(ret),
                (_, _) => Err(RuntimeError::new(InvalidOpTypes::new_binary(op, left_type, right_type))),
            }
        },

        BinaryOperator::Coerce => {
            let left_type = left.r#type();
            let right_type = right.r#type();
            match (left, right) {
                (LanternValue::Option(Some(left)) | LanternValue::Result(Ok(left)), _) => Ok(*left),
                (LanternValue::Option(None) | LanternValue::Result(Err(_)), right) => Ok(right),
                (_, _) => Err(RuntimeError::new(InvalidOpTypes::new_binary(op, left_type, right_type))),
            }
        }
    }
}

pub fn perform_unary_op(op: UnaryOperator, value: LanternValue) -> Result<LanternValue, RuntimeError> {
    match op {
        UnaryOperator::Neg => {
            if let LanternValue::Num(num) = value {
                Ok(LanternValue::Num(-num))
            } else {
                Err(RuntimeError::new(InvalidOpTypes::new_unary(op, value.r#type())))
            }
        },
        UnaryOperator::Not => {
            if let LanternValue::Bool(bool) = value {
                Ok(LanternValue::Bool(!bool))
            } else {
                Err(RuntimeError::new(InvalidOpTypes::new_unary(op, value.r#type())))
            }
        },
    }
}

fn binary_num_op<F>(left: LanternValue, right: LanternValue, op: BinaryOperator, operate: F) -> Result<LanternValue, RuntimeError>
where F: FnOnce(f64, f64) -> Result<f64, RuntimeError>
{
    match (&left, &right) {
        (LanternValue::Num(left), LanternValue::Num(right)) => operate(*left, *right).map(LanternValue::Num),
        _ => Err(RuntimeError::new(InvalidOpTypes::new_binary(op, left.r#type(), right.r#type()))),
    }
}

fn binary_bool_op<F>(left: LanternValue, right: LanternValue, op: BinaryOperator, operate: F) -> Result<LanternValue, RuntimeError>
where F: FnOnce(bool, bool) -> Result<bool, RuntimeError>
{
    match (&left, &right) {
        (LanternValue::Bool(left), LanternValue::Bool(right)) => operate(*left, *right).map(LanternValue::Bool),
        _ => Err(RuntimeError::new(InvalidOpTypes::new_binary(op, left.r#type(), right.r#type()))),
    }
}

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[error("invalid operator {0}")]
pub struct InvalidOpError(pub Operator);

#[derive(thiserror::Error, Debug, Clone, PartialEq)]
#[non_exhaustive]
pub struct InvalidOpTypes {
    pub left: Option<LanternType>,
    pub op: Operator,
    pub right: LanternType,
}

impl InvalidOpTypes {
    pub fn new_binary(op: BinaryOperator, left: LanternType, right: LanternType) -> Self {
        Self { op: Operator::Binary(op), left: Some(left), right }
    }

    pub fn new_unary(op: UnaryOperator, r#type: LanternType) -> Self {
        Self { op: Operator::Unary(op), left: None, right: r#type }
    }
}

impl Display for InvalidOpTypes {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.left {
            Some(left) => write!(f, "operator {} cannot be applied to type {} and type {}", self.op, left, self.right),
            None => write!(f, "operator {} cannot be applied to type {}", self.op, self.right),
        }
    }
}

