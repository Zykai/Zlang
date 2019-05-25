use super::data_structures::
{
    CustomType, LiteralType, OperatorType, Token, TokenType, DataType,
    parser_types::{
        *,
        statement_tree::*,
    }
};
use super::data_structures::parser_types::expression_trees::*;
use crate::compiler::data_structures::parser_types::*;
use std::sync::Mutex;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

type ParserResult<'a, T> = Result<T, ParserError<'a>>;
const EOF: &str = "Error in parser stage: unexpected end of file";

pub struct Parser<'a> {
    tokens: &'a[Token],
    size: usize,
    index: Mutex<usize>,
}

pub enum ParserError<'a> {
    ExpcectedError(TokenType, &'a Token),
    UnexpectedEOF,
    ExpectCustomType(&'a Token),
}

impl <'a> Display for ParserError <'a> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            ParserError::ExpcectedError(e, r) => {
                write!(f, "Error in ln. {}, cl. {} during parser stage: expected token of type {:?}, received {:?}", r.line, r.column, e, r.token_type)
            },
            ParserError::UnexpectedEOF => {
                write!(f, "Parser Error: unexpected end of file")
            },
            ParserError::ExpectCustomType(t) => {
                write!(f, "Parser Error in ln. {}, cl. {}: expected custom type name after 'new'", t.line, t.column)
            }
        }
    }
}

impl <'a> Parser<'a> {

pub fn new(token_vec: &'a Vec<Token>) -> Parser<'a> {
    Parser{
        tokens: &token_vec[..],
        size: token_vec.len(),
        index: Mutex::new(0),
    }
}

fn peek_current(&self) -> ParserResult<&Token> {
    let index = *self.index.lock().unwrap();
    if index >= self.size {
        Err(ParserError::UnexpectedEOF)
    } else {
        Ok(&self.tokens[index])
    }
}

fn peek_next(&self) -> ParserResult<&Token> {
    let index = *self.index.lock().unwrap();
    if index+1 >= self.size {
        Err(ParserError::UnexpectedEOF)
    } else {
        Ok(&self.tokens[index+1])
    }
}

fn current_token(&self) -> ParserResult<&Token> {
    let mut index = self.index.lock().unwrap();
    *index = *index + 1;
    if *index-1 >= self.size {
        Err(ParserError::UnexpectedEOF)
    } else {
        Ok(&self.tokens[*index-1])
    }
}

#[inline(always)]
fn inc(&self) {
    let mut index = self.index.lock().unwrap();
    *index = *index + 1;
}

pub fn parse_program(&self) -> ParserResult<Program> {
    let mut program = Program::empty();
    loop {
        let peek = self.peek_current();
        if let Err(_) = peek {
            break;
        }
        let token_type = &peek.ok().unwrap().token_type;
        match token_type {
            TokenType::Struct => {
                self.inc();
                program.types.insert("test".to_string(), self.parse_struct().ok().unwrap());
            },
            TokenType::TypeName(d) => {
                let function = self.parse_function(&d)?;
                program.functions.insert(function.name.clone(), function);
            },
            TokenType::Identifier(s) => {
                let function = self.parse_function(&DataType::Reference(s.clone()))?;
                program.functions.insert(function.name.clone(), function);
            }
            _ => {
                self.inc();
            }
                    
        }
    }
    Ok(program)
}

// Parses a function
fn parse_function(&self, data_type: & DataType) -> ParserResult<Function> {
    let head = self.parse_data_type(false)?;
    let mut parameters: Vec<Variable> = Vec::new();
    self.parser_expect(TokenType::ParentheseOpen)?;
    if self.peek_current()?.token_type != TokenType::ParentheseClose {
        loop{
            let parameter = self.parse_data_type(false)?;
            parameters.push(parameter);
            if self.peek_current()?.token_type != TokenType::Comma {
                break;
            } else {
                self.inc();
            }
        }
    }
    self.parser_expect(TokenType::ParentheseClose)?;
    let stmts = self.parse_group_stmt()?;
    Ok(Function::new(head.name, head.data_type, parameters, stmts))
}

// Parses a struct (without 'struct'-token), i.e. a user-defined data-type
fn parse_struct(&self) -> ParserResult<CustomType> {
    let name = self.parser_get(TokenType::Identifier("".to_string()))?;
    let name = {
        match name {
            TokenType::Identifier(s) => {
                s
            },
            _ => {
                panic!("Verified that token_type is identifier, is not identifier anymore, should never happen");
            }
        }
    };
    self.parser_expect(TokenType::CurlyOpen)?;
    let mut member_vars: HashMap<String, DataType> = HashMap::new();

    loop {
        let member_type = self.parser_get(TokenType::TypeName(DataType::Void))?;
        let member_name = self.parser_get(TokenType::Identifier("".to_string()))?;
        let member_type = {
            match member_type {
                TokenType::TypeName(t) => {t},
                _ => {panic!("Verified that token_type is identifier, is not identifier anymore, should never happen");}
            }
        };
        let member_name = {
            match member_name {
                TokenType::Identifier(s) => {s},
                _ => {panic!("Verified that token_type is identifier, is not identifier anymore, should never happen");}
            }
        };
        member_vars.insert(member_name, member_type);
        let after = self.peek_current()?;
        self.inc();
        match after.token_type {
            TokenType::Comma => {
                continue;
            },
            TokenType::CurlyClose => {
                break;
            }
            _ => {
            
            }
        }
    }

    Ok(CustomType::new(name, member_vars))
}


// Is decl checks wether the last token should be consumed
fn parse_data_type(&self, is_decl: bool) -> ParserResult<Variable> {
    let data_type = self.current_token()?;
    let data_type = {
        let simple_type = match &data_type.token_type {
            TokenType::TypeName(t) => {
                t.clone()
            },
            TokenType::Identifier(i) => {
                DataType::Reference(i.clone())
            },
            _ => {
                // If the token_type is neither an identifier or typename, this method should never have been called
                unreachable!();
            }
        };
        let mut dimensions = 0;
        if self.peek_current()?.token_type == TokenType::SquareOpen {
            dimensions = dimensions + 1;
            self.inc();
            while self.peek_current()?.token_type == TokenType::Comma {
                self.inc();
                dimensions = dimensions + 1;
            }
            self.parser_expect(TokenType::SquareClose)?;
        }
        if dimensions == 0 {
            simple_type.clone()
        } else {
            DataType::Array(Box::new(simple_type.clone()), dimensions)
        }
    };
    let id = self.peek_current()?;
    if ! is_decl {
        self.inc();
    }
    if let TokenType::Identifier(s) = &id.token_type {
        Ok(Variable::new(s.clone(), data_type))
    } else {
        Err(ParserError::ExpcectedError(TokenType::Identifier("".to_string()), id))
    }
}

// Checks wether a token is of the required type and returns the type
fn parser_get(&self, expected: TokenType) -> Result<TokenType, ParserError> {
    let received = self.peek_current()?;
    self.inc();
    if received.token_type != expected {
        return Err(ParserError::ExpcectedError(expected, received));
    }
    Ok(received.token_type.clone())
}

// Checks wether a token is of the required type
fn parser_expect(&self, expected: TokenType) -> Result<(), ParserError> {
    let received = self.peek_current()?;
    self.inc();
    if received.token_type != expected {
        return Err(ParserError::ExpcectedError(expected, received));
    }
    Ok(())
}

fn parse_statement(&self) -> ParserResult<Box<Statement>> {
    match self.peek_current()?.token_type {
        TokenType::For => self.parse_for_stmt(),
        TokenType::While => self.parse_while_stmt(),
        TokenType::TypeName(_) => self.parse_decl_stmt(),
        TokenType::Return => self.parse_return_stmt(),
        TokenType::CurlyOpen =>  self.parse_group_stmt(),
        TokenType::If => self.parse_if_stmt(),
        TokenType::Delete => self.parse_delete_smt(),
        _ => self.parse_expr_stmt(),
    }
}

fn parse_for_stmt(&self) -> ParserResult<Box<Statement>> {
    self.inc();
    let declare;
    let check;
    let update;
    let body;
    if let TokenType::Semicolon = self.peek_current()?.token_type {
        declare = None;
        self.inc();
    } else {
        declare = Some(DeclAssign::Decl(self.parse_decl_stmt()?));
    }
    if let TokenType::Semicolon = self.peek_current()?.token_type {
        check = None;
    } else {
        check = Some(self.log_expression()?);
    }
    self.parse_semicolon()?;
    if let TokenType::CurlyOpen = self.peek_current()?.token_type {
        update = None;
    } else {
        update = Some(self.parse_expression()?);
    }
    body = self.parse_group_stmt()?;
    Ok(Box::new(ForStmt::new(declare, check, update, body)))
}

fn parse_while_stmt(&self) -> ParserResult<Box<Statement>> {
    self.inc();
    let expr = self.log_expression()?;
    let body = self.parse_group_stmt()?;
    Ok(Box::new(WhileStmt::new(expr, body)))
}

fn parse_if_stmt(&self) -> ParserResult<Box<Statement>> {
    self.inc();
    let expr = self.log_expression()?;
    let body = self.parse_group_stmt()?;
    let else_body;
    if let TokenType::Else = &self.peek_current()?.token_type {
        self.inc();
        else_body = Some(self.parse_group_stmt()?);
    } else {
        else_body = None;
    }
    Ok(Box::new(IfStmt::new(expr, body, else_body)))
}

// Parses a declaration, needs to look ahead to prevent consuming part of an assign-expression
fn parse_decl_stmt(&self) -> ParserResult<Box<Statement>> {
    let var = self.parse_data_type(true)?;
    let assign;
    if let TokenType::Semicolon = self.peek_next()?.token_type {
        assign = None;
        self.inc();
        self.inc();
    } else {
        assign = Some(self.parse_expression()?);
        self.parse_semicolon()?;
    }
    Ok(Box::new(DeclStmt::new(var.data_type, var.name, assign)))
}

fn parse_return_stmt(&self) -> ParserResult<Box<Statement>> {
    let token_type = self.current_token()?.token_type.clone();
    if let TokenType::Semicolon = self.peek_current()?.token_type {
        self.inc();
        Ok(Box::new(JmpStatement::new(token_type, None)))
    } else {
        let expr = self.parse_expression()?;
        if let TokenType::Semicolon = self.peek_current()?.token_type{
            self.inc();
            Ok(Box::new(JmpStatement::new(token_type, Some(expr))))
        } else {
            Err(ParserError::ExpcectedError(TokenType::Semicolon, self.current_token()?))
        }
    }
}

fn parse_expr_stmt(&self) -> ParserResult<Box<Statement>> {
    let expr = self.parse_expression()?;
    self.parse_semicolon()?;
    Ok(Box::new(ExprStmt::new(expr)))
}

fn parse_group_stmt(&self) -> ParserResult<Box<Statement>> {
    let mut group_stmt = GroupStmt::new();
    self.parser_expect(TokenType::CurlyOpen)?;
    loop {
        if self.peek_current()?.token_type == TokenType::CurlyClose{
            break;
        }
        group_stmt.addStmt(self.parse_statement()?);
    }
    Ok(Box::new(group_stmt))
}

fn parse_delete_smt(&self) -> ParserResult<Box<Statement>> {
    self.inc();
    let id = self.current_token()?;
    if let TokenType::Identifier(s) = &id.token_type {
        let delete_stmt = Box::new(DeleteStmt::new(s.clone()));
        self.parse_semicolon()?;
        Ok(delete_stmt)
    } else {
        Err(ParserError::ExpcectedError(TokenType::Identifier("".to_string()), id))
    }
}

pub fn parse_expression(&self) -> ParserResult<Box<dyn Expression>> {
    self.assign_expression()
}

// Parses an assign expression, cannot be stacked
fn assign_expression(&self) -> ParserResult<Box<Expression>> {
    let left = self.log_expression()?;
    if let TokenType::Operator(o) = &self.peek_current()?.token_type {
        self.inc();
        let right = self.log_expression()?;
        Ok(Box::new(AssignExpression::new(left, right, o.clone())))
    } else {
        Ok(left)
    }
}

fn log_expression(&self) -> ParserResult<Box<Expression>> {
    let mut current = self.bitwise_expression()?;
    loop {
        let operator = self.peek_current()?;
        let op_type;
        match operator.token_type {
            TokenType::Operator(OperatorType::LogAnd) => op_type = OperatorType::LogAnd,
            TokenType::Operator(OperatorType::LogOr) => op_type = OperatorType::LogOr,
            TokenType::Operator(OperatorType::LogXor) => op_type = OperatorType::LogXor,
            _ => break,
        }
        self.inc();
        let next_expression = self.bitwise_expression()?;
        current = Box::new(BinaryExpression::new(current, next_expression, op_type));
    }
    Ok(current)
}

fn bitwise_expression(&self) -> ParserResult<Box<Expression>> {
    let mut current = self.equal_expression()?;
    loop {
        let operator = self.peek_current()?;
        let op_type;
        match operator.token_type {
            TokenType::Operator(OperatorType::BitAnd) => op_type = OperatorType::BitAnd,
            TokenType::Operator(OperatorType::BitOr) => op_type = OperatorType::BitOr,
            TokenType::Operator(OperatorType::BitXor) => op_type = OperatorType::BitXor,
            _ => break,
        }
        self.inc();
        let next_expression = self.equal_expression()?;
        current = Box::new(BinaryExpression::new(current, next_expression, op_type));
    }
    Ok(current)
}

fn equal_expression(&self) -> ParserResult<Box<Expression>> {
    let mut current = self.compare_expression()?;
    loop {
        let operator = self.peek_current()?;
        let op_type;
        match operator.token_type {
            TokenType::Operator(OperatorType::CmpEqual) => op_type = OperatorType::CmpEqual,
            TokenType::Operator(OperatorType::CmpNotEqual) => op_type = OperatorType::CmpNotEqual,
            _ => break,
        }
        self.inc();
        let next_expression = self.compare_expression()?;
        current = Box::new(BinaryExpression::new(current, next_expression, op_type));
    }
    Ok(current)
}

fn compare_expression(&self) -> ParserResult<Box<Expression>> {
    let mut current = self.addition_expression()?;
    loop {
        let operator = self.peek_current()?;
        let op_type;
        match operator.token_type {
            TokenType::Operator(OperatorType::CmpLess) => op_type = OperatorType::CmpLess,
            TokenType::Operator(OperatorType::CmpLessEqual) => op_type = OperatorType::CmpLessEqual,
            TokenType::Operator(OperatorType::CmpGreater) => op_type = OperatorType::CmpGreater,
            TokenType::Operator(OperatorType::CmpGreaterEqual) => {
                op_type = OperatorType::CmpGreaterEqual
            }
            _ => break,
        }
        self.inc();
        let next_expression = self.addition_expression()?;
        current = Box::new(BinaryExpression::new(current, next_expression, op_type));
    }
    Ok(current)
}

fn addition_expression(&self) -> ParserResult<Box<Expression>> {
    let mut current = self.multiplication_expression()?;
    loop {
        let operator = self.peek_current()?;
        let op_type;
        match operator.token_type {
            TokenType::Operator(OperatorType::Plus) => op_type = OperatorType::Plus,
            TokenType::Operator(OperatorType::Minus) => op_type = OperatorType::Minus,
            _ => break,
        }
        self.inc();
        let next_expression = self.multiplication_expression()?;
        current = Box::new(BinaryExpression::new(current, next_expression, op_type));
    }
    Ok(current)
}

fn multiplication_expression(&self) -> ParserResult<Box<Expression>> {
    let mut current = self.value_expression()?;
    loop {
        let operator = self.peek_current()?;
        let op_type;
        match operator.token_type {
            TokenType::Operator(OperatorType::Multiply) => op_type = OperatorType::Multiply,
            TokenType::Operator(OperatorType::Divide) => op_type = OperatorType::Divide,
            _ => break,
        }
        self.inc();
        let next_expression = self.value_expression()?;
        current = Box::new(BinaryExpression::new(current, next_expression, op_type));
    }
    Ok(current)
}

fn value_expression(&self) -> ParserResult<Box<Expression>> {
    let t = self.current_token()?;
    match &t.token_type {
        TokenType::Identifier(s) => {
            self.identifier_expression(s)
        },
        TokenType::ValueLiteral(v) => {
            Ok(Box::new(ValueExpression::new(v.clone())))
        },
        TokenType::ParentheseOpen => {
            let res = self.log_expression()?;
            self.parser_expect(TokenType::ParentheseClose)?;
            Ok(res)
        }
        TokenType::New => {
            self.parse_new()
        }
        _  => {
            Err(ParserError::ExpcectedError(TokenType::Identifier("".to_string()), t))
        }
    }
}

// Parses VariableExpr and FunctionExpr
fn identifier_expression(&self, id: &String) -> ParserResult<Box<Expression>> {
    match self.peek_current()?.token_type {
        TokenType::SquareOpen => {
            self.inc();
            let mut dimensions = Vec::new();
            dimensions.push(self.log_expression()?);
            while let TokenType::Comma = self.peek_current()?.token_type {
                self.inc();
                dimensions.push(self.log_expression()?);
            }
            if let TokenType::SquareClose = self.peek_current()?.token_type {
                self.inc();
            } else {
                return Err(ParserError::ExpcectedError(TokenType::SquareClose, self.current_token()?));
            }
            let dimensions = Some(dimensions);
            Ok(Box::new(VariableExpr::new(id.clone(), dimensions)))
        },
        TokenType::ParentheseOpen => {
            self.inc();
            let mut parameters = Vec::new();
            if TokenType::ParentheseClose != self.peek_current()?.token_type {
                parameters.push(self.log_expression()?);
                while let TokenType::Comma = self.peek_current()?.token_type {
                    self.inc();
                    parameters.push(self.log_expression()?);
                }
                self.parser_expect(TokenType::ParentheseClose)?;
            } else {
                self.inc();
            }
            Ok(Box::new(FunctionExpr::new(id.clone(), parameters)))
        },
        _ => {
            Ok(Box::new(VariableExpr::new(id.clone(), None)))
        }
    }
}

fn parse_new(&self) -> ParserResult<Box<Expression>> {
    let type_name = self.current_token()?;
    if let TokenType::Identifier(s) = &type_name.token_type {
        let type_name = s.clone();
        let mut parameters: HashMap<String, Box<Expression>> = HashMap::new();
        self.parser_expect(TokenType::CurlyOpen)?;
        loop {
            if let TokenType::Identifier(s) = &self.peek_current()?.token_type {
                self.inc();
                self.parser_expect(TokenType::Colon)?;
                let value = self.log_expression()?;
                parameters.insert(s.clone(), value);
                if self.peek_current()?.token_type == TokenType::Comma {
                    self.inc();
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        self.parser_expect(TokenType::CurlyClose)?;
        Ok(Box::new(NewExpr::new(type_name, parameters)))
    } else {
        Err(ParserError::ExpectCustomType(type_name))
    }

}

fn parse_semicolon(&self) -> ParserResult<()> {
    let current = self.current_token()?;
    if let TokenType::Semicolon = current.token_type {
        Ok(())
    } else {
        Err(ParserError::ExpcectedError(TokenType::Semicolon, current))
    }
}

}