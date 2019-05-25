use std::collections::HashMap;
use super::CustomType;
use statement_tree::{Statement};
use super::DataType;

pub mod expression_trees;
pub mod statement_tree;

pub struct Program {
    pub functions: HashMap<String, Function>,
    pub types: HashMap<String, CustomType>
}

impl Program {
    pub fn empty() -> Program {
        Program{
            functions: HashMap::new(),
            types: HashMap::new()
        }
    }
}

pub struct Function {
    pub name: String,
    pub data_type: DataType,
    parameters: Vec<Variable>,
    stmts: Box<Statement>,
}

impl Function {
    pub fn new(name: String, data_type: DataType, parameters: Vec<Variable>, stmts: Box<Statement>) -> Function {
        Function{
            name, data_type, parameters, stmts
        }
    }
}

pub struct Variable {
    pub name: String,
    pub data_type: DataType,
}

impl Variable {
    pub fn new(name: String, data_type: DataType) -> Variable {
        Variable {name, data_type}
    }
}