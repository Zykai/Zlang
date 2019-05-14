// Enumerates all of zLangs datatypes
#[derive(Debug, Clone)]
pub enum DataType {
    Void,
    Reference,
    Byte,
    Short,
    Int,
    Long,
    Float,
    Double,
    Zstring,
}

// Enumerates all operator types
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
pub enum TokenType {
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

#[derive(Debug)]
pub struct Token {
    token_type: TokenType,
    line: u32,
    column: u32,
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
