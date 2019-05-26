use super::super::{DataType, Token, TokenType};
use super::expression_trees::{AssignExpression, Expression};
use std::fmt::Debug;

pub trait Statement {}

pub enum DeclAssign {
    Decl(Box<Statement>),
    Assign(AssignExpression),
}

pub struct DeclStmt {
    data_type: DataType,
    name: String,
    assign: Option<Box<Expression>>,
}

pub struct ExprStmt {
    pub expr: Box<Expression>,
}

pub struct GroupStmt {
    pub stmts: Vec<Box<Statement>>,
}

pub struct ForStmt {
    pub init: Option<DeclAssign>,
    pub check: Option<Box<Expression>>,
    pub update: Option<Box<Expression>>,
    pub body: Box<Statement>,
}

pub struct WhileStmt {
    pub check: Box<Expression>,
    pub body: Box<Statement>,
}

pub struct IfStmt {
    pub condition: Box<Expression>,
    pub body: Box<Statement>,
    pub else_body: Option<Box<Statement>>,
}

pub struct JmpStatement {
    pub cmd: TokenType,
    pub expr: Option<Box<Expression>>,
}

pub struct DeleteStmt {
    pub reference: String,
}

impl Statement for DeclStmt {}
impl Statement for ExprStmt {}
impl Statement for GroupStmt {}
impl Statement for ForStmt {}
impl Statement for WhileStmt {}
impl Statement for IfStmt {}
impl Statement for JmpStatement {}
impl Statement for DeleteStmt {}

impl DeclStmt {
    pub fn new(data_type: DataType, name: String, assign: Option<Box<Expression>>) -> DeclStmt {
        DeclStmt { data_type, name, assign }
    }
}

impl ExprStmt {
    pub fn new(expr: Box<Expression>) -> ExprStmt {
        ExprStmt { expr }
    }
}

impl GroupStmt {
    pub fn new() -> GroupStmt {
        GroupStmt { stmts: Vec::new() }
    }
    pub fn add_stmt(&mut self, s: Box<Statement>) {
        self.stmts.push(s);
    }
}

impl ForStmt {
    pub fn new(init: Option<DeclAssign>, check: Option<Box<Expression>>, update: Option<Box<Expression>>, body: Box<Statement>) -> ForStmt {
        ForStmt { init, check, update, body }
    }
}

impl WhileStmt {
    pub fn new(check: Box<Expression>, body: Box<Statement>) -> WhileStmt {
        WhileStmt { check, body }
    }
}

impl IfStmt {
    pub fn new(condition: Box<Expression>, body: Box<Statement>, else_body: Option<Box<Statement>>) -> IfStmt {
        IfStmt { condition, body, else_body }
    }
}

impl JmpStatement {
    pub fn new(cmd: TokenType, expr: Option<Box<Expression>>) -> JmpStatement {
        JmpStatement { cmd, expr }
    }
}

impl DeleteStmt {
    pub fn new(reference: String) -> DeleteStmt {
        DeleteStmt { reference }
    }
}
