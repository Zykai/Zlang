use std::fmt::Debug;
use super::expression_trees::Expression;
use super::super::{DataType, Token};

pub trait Statement {}
pub trait DeclAssign {}

pub struct DeclStmt<'a>{
    data_type: DataType,
    name: &'a String,
}

pub struct ExprStmt{
    pub expr: Box<Expression>,
}

pub struct GroupStmt{
    pub stmts: Vec<Box<Statement>>,
}

pub struct ForStmt{
    pub init: Option<Box<DeclAssign>>,
    pub check:  Option<Box<Expression>>,
    pub update:  Option<Box<Expression>>,
    pub body: Box<Statement>,
}

pub struct WhileStmt {
    pub check: Box<Expression>,
    pub body: Box<Statement>,
}

pub struct JmpStatement<'a> {
    pub cmd: &'a Token,
    pub expr: Option<Box<Expression>>,
}

impl <'a> Statement for DeclStmt<'a> {}
impl Statement for ExprStmt {}
impl Statement for GroupStmt {}
impl Statement for ForStmt {}
impl Statement for WhileStmt {}
impl <'a> Statement for JmpStatement<'a> {}

impl <'a> DeclStmt <'a>{
    pub fn new(data_type: DataType, name: &'a String) -> DeclStmt{
        DeclStmt{
            data_type, name
        }
    }
}

impl ExprStmt{
    pub fn new(expr: Box<Expression>) -> ExprStmt{
        ExprStmt{
            expr
        }
    }
}

impl GroupStmt{
    pub fn new() -> GroupStmt {
        GroupStmt{
            stmts: Vec::new()
        }
    }
    pub fn addStmt(&mut self, s: Box<Statement>){
        self.stmts.push(s);
    }
}

impl ForStmt{
    pub fn new(init: Option<Box<DeclAssign>>, check: Option<Box<Expression>>, update: Option<Box<Expression>>, body: Box<ExprStmt>) -> ForStmt {
        ForStmt{
            init, check, update, body
        }
    }
}

impl WhileStmt{
    pub fn new(check: Box<Expression>, body: Box<Statement>) -> WhileStmt{
        WhileStmt{
            check, body
        }
    }
}

impl <'a> JmpStatement <'a>{
    pub fn new(cmd: &'a Token, expr: Option<Box<Expression>>) -> JmpStatement{
        JmpStatement{
            cmd, expr
        }
    }
}