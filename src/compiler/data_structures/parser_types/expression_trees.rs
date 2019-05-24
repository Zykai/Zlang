use super::super::{LiteralType, OperatorType};
use std::fmt::Debug;

pub trait Expression: Debug {}

#[derive(Debug)]
pub struct BinaryExpression {
    left: Box<Expression>,
    right: Box<Expression>,
    operator: OperatorType,
}

impl Expression for BinaryExpression {}

impl BinaryExpression {
    pub fn new(
        left: Box<Expression>,
        right: Box<Expression>,
        operator: OperatorType,
    ) -> BinaryExpression {
        BinaryExpression {
            left,
            right,
            operator,
        }
    }
}

#[derive(Debug)]
pub struct ValueExpression {
    pub value: LiteralType,
}

impl ValueExpression {
    pub fn new(value: LiteralType) -> ValueExpression {
        ValueExpression { value }
    }
}

impl Expression for ValueExpression {}
