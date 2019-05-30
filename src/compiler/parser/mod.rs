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
use std::collections::{HashMap, LinkedList};
use std::fmt::{Display, Formatter};

type ParserResult<T> = Result<T, ParserError>;

pub struct Parser {
    tokens: LinkedList<Token>,
}

pub enum ParserError {
    ExpcectedError(TokenType, Token),
    ExpectMultiple(Vec<TokenType>, Token),
    UnexpectedEOF,
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            ParserError::ExpcectedError(e, r) => {
                write!(f, "Error in ln. {}, cl. {} during parser stage: expected token of type {:?}, received {:?}", r.line, r.column, e, r.token_type)
            },
            ParserError::ExpectMultiple(v, t) => {
                write!(f, "Error in ln. {}, cl. {} during parser stage: expected one of ", t.line, t.column)?;
                let n = v.len();
                let mut current = 0;
                for i in v {
                    write!(f, "{:?}", i)?;
                    if current == n - 2 {
                        write!(f, " or ")?;
                    } else if current < n-1 {
                        write!(f, ", ")?;
                    }
                    current = current + 1;
                }
                write!(f, "; received {:?}", t.token_type)
            },
            ParserError::UnexpectedEOF => {
                write!(f, "Parser Error: unexpected end of file")
            }
        }
    }
}

impl Parser {

pub fn new(token_vec: LinkedList<Token>) -> Parser {
    Parser{
        tokens: token_vec,
    }
}

fn peek_current(&mut self) -> ParserResult<&Token> {
    let first = self.tokens.iter().nth(0);
    if let Some(t) = first {
        Ok(t)
    } else {
        Err(ParserError::UnexpectedEOF)
    }
}

fn peek_next(&self) -> ParserResult<&Token> {
    let first = self.tokens.iter().nth(1);
    if let Some(t) = first {
        Ok(t)
    } else {
        Err(ParserError::UnexpectedEOF)
    }
}

fn current_token(&mut self) -> ParserResult<Token> {
    let first = self.tokens.pop_front();
    if let Some(t) = first {
        Ok(t)
    } else {
        Err(ParserError::UnexpectedEOF)
    }
}

#[inline]
fn push_front(&mut self, token: Token) {
    self.tokens.push_front(token);
}

pub fn parse_program(&mut self) -> ParserResult<Program> {
    let mut program = Program::empty();
    loop {
        let current = self.peek_current();
        // End of file
        if let Err(_) = current {
            break;
        }
        let current = current.ok().unwrap();
        match &current.token_type {
            TokenType::Struct => {
                self.current_token()?;
                let new_struct = self.parse_struct()?;
                program.types.insert(new_struct.name.clone(), new_struct);
            },
            TokenType::Identifier(_) | TokenType::TypeName(_) => {
                let function = self.parse_function()?;
                program.functions.insert(function.name.clone(), function);
            },
            _ => {
                self.current_token()?;
            }
                    
        }
    }
    Ok(program)
}

// Parses a function
fn parse_function(&mut self) -> ParserResult<Function> {
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
                self.current_token()?;
            }
        }
    }
    self.parser_expect(TokenType::ParentheseClose)?;
    let stmts = self.parse_group_stmt()?;
    Ok(Function::new(head.name, head.data_type, parameters, stmts))
}

// Parses a struct (without 'struct'-token), i.e. a user-defined data-type
fn parse_struct(&mut self) -> ParserResult<CustomType> {
    let name = {
        let name_token = self.current_token()?;
        if let TokenType::Identifier(s) = name_token.token_type {
            s
        } else {
            return Err(ParserError::ExpcectedError(TokenType::Identifier("".to_string()), name_token));
        }
    };
    self.parser_expect(TokenType::CurlyOpen)?;
    let mut member_vars: HashMap<String, DataType> = HashMap::new();
    loop {
        let member_type = self.current_token()?;
        let member_type = match member_type.token_type {
            TokenType::TypeName(d) => {
                d
            },
            TokenType::Identifier(s) => {
                DataType::Reference(s)
            },
            _ => {
                return Err(ParserError::ExpectMultiple(vec!(TokenType::TypeName(DataType::Void), TokenType::Identifier("".to_string())), member_type))
            }
        };
        let member_name  = {
            let name_token = self.current_token()?;
            if let TokenType::Identifier(s) = name_token.token_type {
                s
            } else {
                return Err(ParserError::ExpcectedError(TokenType::Identifier("".to_string()), name_token));
            }
        };
        member_vars.insert(member_name, member_type);
        let after = self.current_token()?;
        match &after.token_type {
            TokenType::Comma => {
                let next = self.peek_current()?;
                if let TokenType::CurlyClose = next.token_type {
                    break;
                } else {
                    continue;
                }
            },
            TokenType::CurlyClose => {
                break;
            }
            _ => {
                return Err(ParserError::ExpectMultiple(vec!(TokenType::CurlyClose, TokenType::Comma), after));
            }
        }
    }
    Ok(CustomType::new(name, member_vars))
}


// Is decl checks wether the last token should be consumed
fn parse_data_type(&mut self, is_decl: bool) -> ParserResult<Variable> {
    let data_type = self.current_token()?;
    let data_type = {
        let simple_type = match data_type.token_type {
            TokenType::TypeName(t) => t,
            TokenType::Identifier(i) => DataType::Reference(i),
            _ => unreachable!() // If the token_type is neither an identifier or typename, this method should never have been called            }
        };
        let mut dimensions = 0;
        if self.peek_current()?.token_type == TokenType::SquareOpen {
            dimensions = dimensions + 1;
            self.current_token()?;
            while self.peek_current()?.token_type == TokenType::Comma {
                self.current_token()?;
                dimensions = dimensions + 1;
            }
            self.parser_expect(TokenType::SquareClose)?;
        }
        if dimensions == 0 {
            simple_type
        } else {
            DataType::Array(Box::new(simple_type), dimensions)
        }
    };
    let id = self.current_token()?;
    // Dubplicate the token if it's part of a declaration e.g., int x = 5 -> int x; || x = 5;
    if is_decl {
        self.push_front(id.clone());
    }
    if let TokenType::Identifier(s) = id.token_type {
        Ok(Variable::new(s, data_type))
    } else {
        Err(ParserError::ExpcectedError(TokenType::Identifier("".to_string()), id))
    }
}

// Checks wether a token is of the required type
fn parser_expect(&mut self, expected: TokenType) -> Result<(), ParserError> {
    let received = self.current_token()?;
    if received.token_type != expected {
        return Err(ParserError::ExpcectedError(expected, received));
    }
    Ok(())
}

fn parse_statement(&mut self) -> ParserResult<Box<Statement>> {
    match self.peek_current()?.token_type {
        TokenType::For => self.parse_for_stmt(),
        TokenType::While => self.parse_while_stmt(),
        TokenType::Identifier(_) => self.parse_decl_assign(),
        TokenType::TypeName(_) => self.parse_decl_stmt(),
        TokenType::Return => self.parse_return_stmt(),
        TokenType::CurlyOpen =>  self.parse_group_stmt(),
        TokenType::If => self.parse_if_stmt(),
        TokenType::Delete => self.parse_delete_smt(),
        _ => self.parse_expr_stmt(),
    }
}

fn parse_for_stmt(&mut self) -> ParserResult<Box<Statement>> {
    self.current_token()?;
    let declare;
    let check;
    let update;
    let body;
    if let TokenType::Semicolon = self.peek_current()?.token_type {
        declare = None;
        self.current_token()?;
    } else {
        declare = Some(self.parse_decl_assign()?);
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
    Ok(Box::new(Statement::For(ForStmt::new(declare, check, update, body))))
}

fn parse_while_stmt(&mut self) -> ParserResult<Box<Statement>> {
    self.current_token()?;
    let expr = self.log_expression()?;
    let body = self.parse_group_stmt()?;
    Ok(Box::new(Statement::While(WhileStmt::new(expr, body))))
}

fn parse_if_stmt(&mut self) -> ParserResult<Box<Statement>> {
    self.current_token()?;
    let expr = self.log_expression()?;
    let body = self.parse_group_stmt()?;
    let else_body;
    if let TokenType::Else = &self.peek_current()?.token_type {
        self.current_token()?;
        else_body = Some(self.parse_group_stmt()?);
    } else {
        else_body = None;
    }
    Ok(Box::new(Statement::If(IfStmt::new(expr, body, else_body))))
}

// Parses a declaration, needs to look ahead to prevent consuming part of an assign-expression
fn parse_decl_stmt(&mut self) -> ParserResult<Box<Statement>> {
    let var = self.parse_data_type(true)?;
    let name = self.current_token()?;
    let assign;
    if let TokenType::Semicolon = self.peek_next()?.token_type {
        assign = None;
        self.current_token()?;
    } else {
        self.push_front(name);
        assign = Some(self.parse_expression()?);
        self.parse_semicolon()?;
    }
    Ok(Box::new(Statement::Declare(DeclStmt::new(var.data_type, var.name, assign))))
}

fn parse_return_stmt(&mut self) -> ParserResult<Box<Statement>> {
    let token_type = self.current_token()?.token_type;
    if let TokenType::Semicolon = self.peek_current()?.token_type {
        self.current_token()?;
        Ok(Box::new(Statement::Jmp(JmpStatement::new(token_type, None))))
    } else {
        let expr = self.parse_expression()?;
        if let TokenType::Semicolon = self.peek_current()?.token_type{
            self.current_token()?;
            Ok(Box::new(Statement::Jmp(JmpStatement::new(token_type, Some(expr)))))
        } else {
            Err(ParserError::ExpcectedError(TokenType::Semicolon, self.current_token()?))
        }
    }
}

fn parse_expr_stmt(&mut self) -> ParserResult<Box<Statement>> {
    let expr = self.parse_expression()?;
    self.parse_semicolon()?;
    Ok(Box::new(Statement::Expr(ExprStmt::new(expr))))
}

fn parse_group_stmt(&mut self) -> ParserResult<Box<Statement>> {
    let mut group_stmt = GroupStmt::new();
    self.parser_expect(TokenType::CurlyOpen)?;
    loop {
        if self.peek_current()?.token_type == TokenType::CurlyClose{
            break;
        }
        group_stmt.add_stmt(self.parse_statement()?);
    }
    self.parser_expect(TokenType::CurlyClose)?;
    Ok(Box::new(Statement::Group(group_stmt)))
}

fn parse_delete_smt(&mut self) -> ParserResult<Box<Statement>> {
    self.current_token()?;
    let id = self.current_token()?;
    if let TokenType::Identifier(s) = id.token_type {
        let delete_stmt = DeleteStmt::new(s);
        self.parse_semicolon()?;
        Ok(Box::new(Statement::Delete(delete_stmt)))
    } else {
        Err(ParserError::ExpcectedError(TokenType::Identifier("".to_string()), id))
    }
}

pub fn parse_expression(&mut self) -> ParserResult<Box<Expression>> {
    self.assign_expression()
}

// Parses an assign expression, cannot be stacked
fn assign_expression(&mut self) -> ParserResult<Box<Expression>> {
    let left = self.log_expression()?;
    let first = self.current_token()?;
    if let TokenType::Operator(o) = first.token_type {
        let right = self.log_expression()?;
        Ok(Box::new(Expression::Assign(AssignExpression::new(left, right, o))))
    } else {
        // Push token back, as it hat not been used
        self.push_front(first);
        Ok(left)
    }
}

fn log_expression(&mut self) -> ParserResult<Box<Expression>> {
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
        self.current_token()?;
        let next_expression = self.bitwise_expression()?;
        current = Box::new(Expression::Binary(BinaryExpression::new(current, next_expression, op_type)));
    }
    Ok(current)
}

fn bitwise_expression(&mut self) -> ParserResult<Box<Expression>> {
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
        self.current_token()?;
        let next_expression = self.equal_expression()?;
        current = Box::new(Expression::Binary(BinaryExpression::new(current, next_expression, op_type)));
    }
    Ok(current)
}

fn equal_expression(&mut self) -> ParserResult<Box<Expression>> {
    let mut current = self.compare_expression()?;
    loop {
        let operator = self.peek_current()?;
        let op_type;
        match operator.token_type {
            TokenType::Operator(OperatorType::CmpEqual) => op_type = OperatorType::CmpEqual,
            TokenType::Operator(OperatorType::CmpNotEqual) => op_type = OperatorType::CmpNotEqual,
            _ => break,
        }
        self.current_token()?;
        let next_expression = self.compare_expression()?;
        current = Box::new(Expression::Binary(BinaryExpression::new(current, next_expression, op_type)));
    }
    Ok(current)
}

fn compare_expression(&mut self) -> ParserResult<Box<Expression>> {
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
        self.current_token()?;
        let next_expression = self.addition_expression()?;
        current = Box::new(Expression::Binary(BinaryExpression::new(current, next_expression, op_type)));
    }
    Ok(current)
}

fn addition_expression(&mut self) -> ParserResult<Box<Expression>> {
    let mut current = self.multiplication_expression()?;
    loop {
        let operator = self.peek_current()?;
        let op_type;
        match operator.token_type {
            TokenType::Operator(OperatorType::Plus) => op_type = OperatorType::Plus,
            TokenType::Operator(OperatorType::Minus) => op_type = OperatorType::Minus,
            _ => break,
        }
        self.current_token()?;
        let next_expression = self.multiplication_expression()?;
        current = Box::new(Expression::Binary(BinaryExpression::new(current, next_expression, op_type)));
    }
    Ok(current)
}

fn multiplication_expression(&mut self) -> ParserResult<Box<Expression>> {
    let mut current = self.value_expression()?;
    loop {
        let operator = self.peek_current()?;
        let op_type;
        match operator.token_type {
            TokenType::Operator(OperatorType::Multiply) => op_type = OperatorType::Multiply,
            TokenType::Operator(OperatorType::Divide) => op_type = OperatorType::Divide,
            _ => break,
        }
        self.current_token()?;
        let next_expression = self.value_expression()?;
        current = Box::new(Expression::Binary(BinaryExpression::new(current, next_expression, op_type)));
    }
    Ok(current)
}

fn value_expression(&mut self) -> ParserResult<Box<Expression>> {
    let t = self.current_token()?;
    match t.token_type {
        TokenType::Identifier(s) => {
            self.identifier_expression(s)
        },
        TokenType::ValueLiteral(v) => {
            Ok(Box::new(Expression::Value(ValueExpression::new(v))))
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
            // TODO 
            Err(ParserError::ExpcectedError(TokenType::Identifier("".to_string()), t))
        }
    }
}

// Parses VariableExpr and FunctionExpr
fn identifier_expression(&mut self, id: String) -> ParserResult<Box<Expression>> {
    let current = self.current_token()?;
    match &current.token_type {
        TokenType::SquareOpen => {
            let mut dimensions = Vec::new();
            dimensions.push(self.log_expression()?);
            while let TokenType::Comma = self.peek_current()?.token_type {
                self.current_token()?;
                dimensions.push(self.log_expression()?);
            }
            if let TokenType::SquareClose = self.peek_current()?.token_type {
                self.current_token()?;
            } else {
                return Err(ParserError::ExpcectedError(TokenType::SquareClose, self.current_token()?));
            }
            let dimensions = Some(dimensions);
            Ok(Box::new(Expression::Variable(VariableExpr::new(id, dimensions))))
        },
        TokenType::ParentheseOpen => {
            let mut parameters = Vec::new();
            if TokenType::ParentheseClose != self.peek_current()?.token_type {
                parameters.push(self.log_expression()?);
                while let TokenType::Comma = self.peek_current()?.token_type {
                    self.current_token()?;
                    parameters.push(self.log_expression()?);
                }
                self.parser_expect(TokenType::ParentheseClose)?;
            } else {
                self.current_token()?;
            }
            Ok(Box::new(Expression::Function(FunctionExpr::new(id, parameters))))
        },
        _ => {
            self.push_front(current);
            Ok(Box::new(Expression::Variable(VariableExpr::new(id, None))))
        }
    }
}

fn parse_new(&mut self) -> ParserResult<Box<Expression>> {
    let type_name = self.current_token()?;
    match type_name.token_type {
        TokenType::Identifier(s) => {
            self.parse_custom_type(s)
        },
        TokenType::TypeName(d) => {
            self.parser_expect(TokenType::SquareOpen)?;
            self.parse_array(d)
        },
        _ => {
            Err(ParserError::ExpectMultiple(vec!(TokenType::Identifier("Custom_Type_Name".to_string()), TokenType::TypeName(DataType::Void)), type_name))
        }
    }

}

// Parses the creation of a custom type reference e.g: new Tree {left: null, right: null, value: 5}
fn parse_custom_type(&mut self, type_name: String) -> ParserResult<Box<Expression>> {
    let mut parameters: HashMap<String, Box<Expression>> = HashMap::new();
            let current = self.current_token()?;
            match current.token_type {
                TokenType::CurlyOpen => {
                    loop {
                        let name_token = self.current_token()?;
                        if let TokenType::Identifier(name) = name_token.token_type {
                            self.parser_expect(TokenType::Colon)?;
                            let value = self.log_expression()?;
                            parameters.insert(name, value);
                            if self.peek_current()?.token_type == TokenType::Comma {
                                self.current_token()?;
                            } else {
                                break;
                            }
                        } else {
                            self.push_front(name_token);
                            break;
                        }
                    }
                    self.parser_expect(TokenType::CurlyClose)?;
                    Ok(Box::new(Expression::New(NewExpr::new(type_name, parameters))))
                },
                TokenType::SquareOpen => {
                    let data_type = DataType::Reference(type_name);
                    self.parse_array(data_type)
                },
                _ => {
                    Err(ParserError::ExpectMultiple(vec!(TokenType::CurlyOpen, TokenType::SquareOpen), current))
                }
            }
}

// Parses an array value, expects '[' to have already been checked
fn parse_array(&mut self, data_type: DataType) -> ParserResult<Box<Expression>> {
    let mut dimension_count = 1;
    let mut dimensions = Vec::new();
    dimensions.push(self.log_expression()?);
    loop {
        let current = self.current_token()?;
        if current.token_type == TokenType::SquareClose {
            break;
        }
        dimensions.push(self.log_expression()?);
        dimension_count = dimension_count + 1;
    }
    Ok(Box::new(Expression::CreateArray(CreateArrayExpr::new(data_type, dimensions))))
}

// Decides wether to parse a declaration or an assignment, when only looking at the first token is not enough e.g: MyDataType name = ...
fn parse_decl_assign(&mut self) -> ParserResult<Box<Statement>> {
    let type_name = self.current_token()?;
    let var_name = self.current_token()?;
    // Is a declaration?
    if let TokenType::Identifier(_) = &var_name.token_type {
        self.push_front(var_name);
        self.push_front(type_name);
        self.parse_decl_stmt()
    } else {
        let third = self.current_token()?;
        if third.token_type == TokenType::Comma || third.token_type == TokenType::SquareClose {
            self.push_front(third);
            self.push_front(var_name);
            self.push_front(type_name);
            self.parse_decl_stmt()
        } else {
            self.push_front(third);
            self.push_front(var_name);
            self.push_front(type_name);
            self.parse_expr_stmt()
        }
    }
}

fn parse_semicolon(&mut self) -> ParserResult<()> {
    let current = self.current_token()?;
    if let TokenType::Semicolon = current.token_type {
        Ok(())
    } else {
        Err(ParserError::ExpcectedError(TokenType::Semicolon, current))
    }
}

}