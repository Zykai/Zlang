use std::collections::HashMap;
use super::CustomType;
use statement_tree::{GroupStmt};
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
    stmts: GroupStmt,
}

impl Function {
    pub fn new(name: String, data_type: DataType, stmts: GroupStmt) -> Function {
        Function{
            name, data_type, stmts
        }
    }
}