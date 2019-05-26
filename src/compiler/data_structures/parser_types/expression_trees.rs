use super::super::{DataType, LiteralType, OperatorType};
use std::collections::HashMap;
use std::fmt::Debug;

pub trait Expression: Debug {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::Generic
    }
}

pub enum ExpressionType {
    Generic,
    Variable,
    Value,
    Assign,
}

#[derive(Debug)]
pub struct AssignExpression {
    left: Box<Expression>,
    right: Box<Expression>,
    operator: OperatorType,
}

impl AssignExpression {
    pub fn new(
        left: Box<Expression>,
        right: Box<Expression>,
        operator: OperatorType,
    ) -> AssignExpression {
        AssignExpression {
            left,
            right,
            operator,
        }
    }
}

#[derive(Debug)]
pub struct BinaryExpression {
    left: Box<Expression>,
    right: Box<Expression>,
    operator: OperatorType,
}

#[derive(Debug)]
pub struct ValueExpression {
    pub value: LiteralType,
}

#[derive(Debug)]
pub struct VariableExpr {
    pub name: String,
    pub dimensions: Option<Vec<Box<Expression>>>,
}

#[derive(Debug)]
pub struct NewExpr {
    pub type_name: String,
    pub parameters: HashMap<String, Box<Expression>>,
}

#[derive(Debug)]
pub struct CreateArrayExpr {
    pub array_type: DataType,
    pub dimensions: Vec<Box<Expression>>,
}

#[derive(Debug)]
pub struct FunctionExpr {
    pub function_name: String,
    pub parameters: Vec<Box<Expression>>,
}

impl Expression for BinaryExpression {}
impl Expression for NewExpr {}
impl Expression for CreateArrayExpr {}
impl Expression for FunctionExpr {}
impl Expression for AssignExpression {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::Assign
    }
}
impl Expression for VariableExpr {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::Variable
    }
}
impl Expression for ValueExpression {
    fn get_type(&self) -> ExpressionType {
        ExpressionType::Value
    }
}

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

impl VariableExpr {
    pub fn new(name: String, dimensions: Option<Vec<Box<Expression>>>) -> VariableExpr {
        VariableExpr { name, dimensions }
    }
}

impl ValueExpression {
    pub fn new(value: LiteralType) -> ValueExpression {
        ValueExpression { value }
    }
}

impl NewExpr {
    pub fn new(type_name: String, parameters: HashMap<String, Box<Expression>>) -> NewExpr {
        NewExpr {
            type_name,
            parameters,
        }
    }
}

impl CreateArrayExpr {
    pub fn new(array_type: DataType, dimensions: Vec<Box<Expression>>) -> CreateArrayExpr {
        CreateArrayExpr {
            array_type,
            dimensions,
        }
    }
}

impl FunctionExpr {
    pub fn new(function_name: String, parameters: Vec<Box<Expression>>) -> FunctionExpr {
        FunctionExpr {
            function_name,
            parameters,
        }
    }
}
