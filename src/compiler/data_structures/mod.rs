use std::collections::HashMap;

pub mod parser_types;

// Enumerates all of zLangs datatypes
#[derive(Debug, Clone)]
pub enum DataType {
    Void,
    Reference(String),
    Array(Box<DataType>, u32),
    Byte,
    Short,
    Int,
    Long,
    Float,
    Double,
    Zstring,
}

// impl PartialEq for DataType {
//     fn eq(&self, other: &DataType) -> bool {
//         match (self, other) {
//             (DataType::Byte, DataType::Byte) => {
//                 true
//             },
//             _ => {
//                 false
//             }
//         }
//     }
// }

impl DataType{
    pub fn get_size(&self) -> u32 {
        match self {
            DataType::Void => 0,
            DataType::Reference(_) => 8,
            DataType::Array(_d, _s) => 8,
            DataType::Byte => 1,
            DataType::Short => 2,
            DataType::Int => 4,
            DataType::Long => 8,
            DataType::Float => 4,
            DataType::Double => 8,
            DataType::Zstring => 8,
        }
    }
}

// Enumerates all operator types
#[derive(Debug, Clone, PartialEq)]
pub enum OperatorType {
    Plus,
    Minus,
    Multiply,
    Divide,
    Assign,
    PlusAssign,
    MinusAssign,
    MultiplyAssign,
    DivideAssign,
    CmpEqual,
    CmpNotEqual,
    CmpLess,
    CmpLessEqual,
    CmpGreater,
    CmpGreaterEqual,
    Negate,
    BitAnd,
    BitOr,
    BitXor,
    LogAnd,
    LogOr,
    LogXor,
}

// Enumerates all types of a literal and inclues their value
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralType {
    Bool(bool),
    Byte(i8),
    Short(i16),
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    Zstring(String),
}

// Clone derived to enable copying values from the keyword hashmap
// Enumerates all possible tokens of zLang
#[derive(Debug, Clone)]
pub enum TokenType{
    Identifier(String),
    ValueLiteral(LiteralType),
    TypeName(DataType),
    Operator(OperatorType),
    Return,
    If,
    Else,
    While,
    For,
    Semicolon,
    Comma,
    MemberAccess,
    New,
    Delete,
    Struct,
    ParentheseOpen,
    ParentheseClose,
    CurlyOpen,
    CurlyClose,
    SquareOpen,
    SquareClose,
}

impl PartialEq for TokenType {
    fn eq(&self, other: &TokenType) -> bool {
        match (self, other) {
             (TokenType::Identifier(_), TokenType::Identifier(_)) => true,
             (TokenType::ValueLiteral(_), TokenType::ValueLiteral(_)) => true,
             (TokenType::TypeName(_), TokenType::TypeName(_)) => true,
             (TokenType::Operator(_), TokenType::Operator(_)) => true,
             (TokenType::Return, TokenType::Return) => true,
             (TokenType::If, TokenType::If) => true,
             (TokenType::Else, TokenType::Else) => true,
             (TokenType::While, TokenType::While) => true,
             (TokenType::For, TokenType::For) => true,
             (TokenType::Semicolon, TokenType::Semicolon) => true,
             (TokenType::Comma, TokenType::Comma) => true,
             (TokenType::MemberAccess, TokenType::MemberAccess) => true,
             (TokenType::New, TokenType::Comma) => true,
             (TokenType::Delete, TokenType::Delete) => true,
             (TokenType::Struct, TokenType::Struct) => true,
             (TokenType::ParentheseOpen, TokenType::ParentheseOpen) => true,
             (TokenType::ParentheseClose, TokenType::ParentheseClose) => true,
             (TokenType::CurlyOpen, TokenType::CurlyOpen) => true,
             (TokenType::CurlyClose, TokenType::CurlyClose) => true,
             (TokenType::SquareOpen, TokenType::SquareOpen) => true,
             (TokenType::SquareClose, TokenType::SquareClose) => true,
             (_a, _b) => false
         }
    }
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub line: u32,
    pub column: u32,
}

impl Token {
    pub fn new(token_type: TokenType, line: u32, column: u32) -> Token {
        Token {
            token_type,
            line,
            column,
        }
    }
}

#[derive(Debug)]
pub struct CustomType {
    name: String,
    member_vars: HashMap<String, DataType>,
    size: u32,
}

impl CustomType {
    pub fn new(name: String, member_vars: HashMap<String, DataType>) -> CustomType {
        let size = {
            let mut temp = 0;
            for i in &member_vars {
                temp = temp + i.1.get_size();
            }
            temp
        };
        CustomType {
            name,
            member_vars,
            size,
        }
    }
}
