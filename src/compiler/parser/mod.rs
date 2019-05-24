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
use std::iter::Peekable;
use std::slice::Iter;
use std::collections::HashMap;

type ParserResult<'a, T> = Result<T, ParserError<'a>>;
const EOF: &str = "Error in parser stage: unexpected end of file";
const ID_Place_HOLDER: TokenType = TokenType::Identifier(String::from(""));
pub struct Parser<'a> {
    tokens: &'a[Token],
    size: usize,
    current_index: usize,
}

enum ParserError<'a> {
    ExpcectedError(&'a TokenType, &'a Token),
    UnexpectedEOF,
}

impl <'a> Parser<'a> {

// pub fn new(token_vec: & Vec<Token>) -> Parser<'a> {
//     Parser{
//         tokens: &token_vec[..],
//         size: token_vec.len(),
//         current_index: 0,
//     }
// }

fn peek_current(&self) -> ParserResult<&Token> {
    if self.current_index >= self.size {
        Err(ParserError::UnexpectedEOF)
    } else {
        Ok(&self.tokens[self.current_index])
    }
}

fn current_token(&mut self) -> ParserResult<&Token> {
    self.current_index = self.current_index + 1;
    if self.current_index-1 >= self.size {
        Err(ParserError::UnexpectedEOF)
    } else {
        Ok(&self.tokens[self.current_index-1])
    }
}

// fn peek_next(&mut self) -> ParserResult<&Token> {
//     if self.current_index +1 >= self.size {
//         Err(ParserError::UnexpectedEOF)
//     } else {
//         Ok(&self.tokens[self.current_index+1])
//     }
// }

//#[inline(always)]
fn inc(&mut self) {
    self.current_index = self.current_index + 1;
}

pub fn parse_program(&mut self) -> ParserResult<Program> {
    let mut program = Program::empty();
    loop {
        let current = self.peek_current();
        match current {
            Ok(t) => {
                match &t.token_type {
                    TokenType::Struct => {
                        self.inc();
                        program.types.insert("test".to_string(), self.parse_struct().ok().unwrap());
                    },
                    TokenType::TypeName(d) => {
                        let function = self.parse_function(&d)?;
                        program.functions.insert(function.name.clone(), function);
                    }
                    _ => {
                        self.inc();
                    }
                }
            }
            Err(e) => {
                break;
            }
        }
    }
    Ok(program)
}

// Parses a function
fn parse_function(&mut self, data_type: & DataType) -> ParserResult<Function> {
    let (name, data_type) = self.parse_data_type()?;
    let group_stmt = GroupStmt::new();
    println!("FOUND a function: {}, {:?}", name, data_type);
    Ok(Function::new(name.to_string(), data_type, group_stmt))
}

// Parses a struct (without 'struct'-token), i.e. a user-defined data-type; TODO: better error handling
fn parse_struct(&mut self) -> ParserResult<CustomType> {
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

fn parse_data_type(&mut self) -> ParserResult<(String, DataType)> {
    let data_type = self.peek_current()?;
    self.inc();
    let data_type = {
        let simple_type = match &data_type.token_type {
            TokenType::TypeName(t) => {
                t.clone()
            },
            TokenType::Identifier(i) => {
                DataType::Reference(i.clone())
            },
            _ => {
                panic!("TODO");
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
        if dimensions > 0 {
            simple_type.clone()
        } else {
            DataType::Array(Box::new(simple_type.clone()), dimensions)
        }
    };
    let id = self.peek_current()?;
    self.inc();
    if let TokenType::Identifier(s) = &id.token_type {
        Ok((s.clone(), data_type))
    } else {
        Err(ParserError::ExpcectedError(&ID_Place_HOLDER, id))
    }
}

// Checks wether a token is of the required type and returns the type
fn parser_get(&mut self, expected: TokenType) -> Result<TokenType, ParserError> {
    let received = self.peek_current()?;
    self.inc();
    if received.token_type != expected {
        println!(
            "Error in ln. {}, cl. {} during parser stage: expected token of type {:?}, received {:?}",
            received.line, received.column, expected, received.token_type
        );
        return Err(ParserError::ExpcectedError(&expected, received));
    }
    Ok(received.token_type.clone())
}

// Checks wether a token is of the required type
fn parser_expect(&mut self, expected: TokenType) -> Result<(), ParserError> {
    let received = self.peek_current()?;
    self.inc();
    if received.token_type != expected {
        println!(
            "Error in ln. {}, cl. {} during parser stage: expected token of type {:?}, received {:?}",
            received.line, received.column, expected, received.token_type
        );
        return Err(ParserError::ExpcectedError(&expected, received));
    }
    Ok(())
}

pub fn parse_expression(&mut self) -> ParserResult<Box<dyn Expression>> {
    self.log_expression()
}

fn log_expression(&mut self) -> ParserResult<Box<dyn Expression>> {
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

fn bitwise_expression(&mut self) -> ParserResult<Box<dyn Expression>> {
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

fn equal_expression(&mut self) -> ParserResult<Box<dyn Expression>> {
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

fn compare_expression(&mut self) -> ParserResult<Box<dyn Expression>> {
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

fn addition_expression(&mut self) -> ParserResult<Box<dyn Expression>> {
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

fn multiplication_expression(&mut self) -> ParserResult<Box<dyn Expression>> {
    let mut current = self.value_expression()?;
    loop {
        let operator = self.get_mul_operator();
        if operator.is_err(){
            break;
        }
        let operator = operator.ok().unwrap();
        let next_expression = self.value_expression()?;
        current = Box::new(BinaryExpression::new(current, next_expression, operator));
    }
    Ok(current)
}

fn get_mul_operator(&mut self) -> Result<OperatorType, ()>{
    let operator = self.current_token();
    if operator.is_err() {
        return Err(());
    }
    let operator = operator.ok().unwrap();
    self.inc();
    match operator.token_type {
        TokenType::Operator(OperatorType::Multiply) => Ok(OperatorType::Multiply),
        TokenType::Operator(OperatorType::Divide) => Ok(OperatorType::Divide),
        _ => return Err(()),
    }
}

fn value_expression(&mut self) -> ParserResult<Box<dyn Expression>> {
    let value = self.peek_current()?;
    if let TokenType::ValueLiteral(v) = &value.token_type {
        self.inc();
        Ok(Box::new(ValueExpression::new(v.clone())))
    } else {
        // Return a placeholder literal
        Err(ParserError::ExpcectedError(&TokenType::ValueLiteral(LiteralType::Bool(false)), value))
    }
}

}