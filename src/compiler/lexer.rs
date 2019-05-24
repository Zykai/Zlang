use super::data_structures::{DataType, LiteralType, OperatorType, Token, TokenType};
use std::collections::HashMap;
use std::error::Error;

#[derive(Debug)]
pub enum LexerErrorType {
    FileNotFound(String),
    IllegalChar(char),
    IllegalNumber(String),
}

#[derive(Debug)]
pub struct LexerError {
    error_type: LexerErrorType,
    line: u32,
    column: u32,
}

impl LexerError {
    fn new(error_type: LexerErrorType, line: u32, column: u32) -> LexerError {
        LexerError {
            error_type,
            line,
            column,
        }
    }
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.error_type {
            LexerErrorType::FileNotFound(s) => {
                write!(f, "Compiler error in lexer stage: file '{}' does not exist", s)
            },
            LexerErrorType::IllegalChar(c) => {
                write!(f, "Compiler error in lexer stage, ln. {} cl. {}, found the following illegal character: '{}' (numeric='{}')", self.line, self.column, c, *c as u32)
            },
            LexerErrorType::IllegalNumber(s) => {
                write!(f, "Malformed literal '{}' in ln. {} cl. {}", s, self.line, self.column)
            }
        }
    }
}

impl Error for LexerError {
    fn cause(&self) -> Option<&Error> {
        None
    }
}

// Transforms a source file into an vector of tokens
pub fn tokenize_file(filename: String) -> Result<Vec<Token>, Vec<LexerError>> {
    let name_copy = filename.clone();
    let source_code = std::fs::read_to_string(filename);
    if source_code.is_ok() {
        tokenize_string(source_code.unwrap())
    } else {
        Err(vec![LexerError::new(
            LexerErrorType::FileNotFound(name_copy),
            0,
            0,
        )])
    }
}

// Transforms a source string into an vector of tokens
pub fn tokenize_string(source_code: String) -> Result<Vec<Token>, Vec<LexerError>> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut current_line: u32 = 1;
    let mut current_column: u32 = 1;
    let mut current_char_iter = source_code.chars().peekable();
    current_char_iter.peek();
    let keywords = get_keyword_map();
    let mut count: u32 = 0;
    let mut error_list = Vec::new();
    loop {
        // Get the next char, set terminating symbol if None
        let current_char = current_char_iter.next().unwrap_or('\0');
        count = count + 1;
        match current_char {
            '+' => {
                if *current_char_iter
                    .peek()
                    .expect("* is followed by 'None', should never happen")
                    == '='
                {
                    tokens.push(Token::new(
                        TokenType::Operator(OperatorType::PlusAssign),
                        current_line,
                        current_column,
                    ));
                    current_char_iter.next();
                    current_column = current_column + 1;
                    count = count + 1;
                } else {
                    tokens.push(Token::new(
                        TokenType::Operator(OperatorType::Plus),
                        current_line,
                        current_column,
                    ));
                }
            }
            '-' => {
                match current_char_iter
                    .peek()
                    .expect("- is followed by 'None', should never happen")
                {
                    '>' => {
                        tokens.push(Token::new(
                            TokenType::MemberAccess,
                            current_line,
                            current_column,
                        ));
                        current_char_iter.next();
                        count = count + 1;
                        current_column = current_column + 1;
                    }
                    '=' => {
                        tokens.push(Token::new(
                            TokenType::Operator(OperatorType::MinusAssign),
                            current_line,
                            current_column,
                        ));
                        current_char_iter.next();
                        count = count + 1;
                        current_column = current_column + 1;
                    }
                    _ => {
                        tokens.push(Token::new(
                            TokenType::Operator(OperatorType::Minus),
                            current_line,
                            current_column,
                        ));
                    }
                }
            }
            '*' => {
                if *current_char_iter
                    .peek()
                    .expect("* is followed by 'None', should never happen")
                    == '='
                {
                    tokens.push(Token::new(
                        TokenType::Operator(OperatorType::MultiplyAssign),
                        current_line,
                        current_column,
                    ));
                    current_char_iter.next();
                    count = count + 1;
                    current_column = current_column + 1;
                } else {
                    tokens.push(Token::new(
                        TokenType::Operator(OperatorType::Multiply),
                        current_line,
                        current_column,
                    ));
                }
            }
            '/' => {
                match current_char_iter
                    .peek()
                    .expect("/ is followed by 'None', should never happen")
                {
                    '*' => {
                        current_char_iter.next();
                        count = count + 1;
                        current_column = current_column + 1;
                        parse_comment(
                            &mut current_line,
                            &mut current_column,
                            &mut count,
                            &mut current_char_iter,
                        );
                    }
                    '/' => {
                        current_char_iter.next();
                        count = count + 1;
                        current_column = current_column + 1;
                        parse_singleline_comment(
                            &mut current_line,
                            &mut current_column,
                            &mut count,
                            &mut current_char_iter,
                        );
                    }
                    '=' => {
                        tokens.push(Token::new(
                            TokenType::Operator(OperatorType::DivideAssign),
                            current_line,
                            current_column,
                        ));
                        current_char_iter.next();
                        count = count + 1;
                        current_column = current_column + 1;
                    }
                    _ => {
                        tokens.push(Token::new(
                            TokenType::Operator(OperatorType::Divide),
                            current_line,
                            current_column,
                        ));
                    }
                }
            }
            '=' => {
                if *current_char_iter
                    .peek()
                    .expect("= is followed by 'None', should never happen")
                    == '='
                {
                    tokens.push(Token::new(
                        TokenType::Operator(OperatorType::CmpEqual),
                        current_line,
                        current_column,
                    ));
                    current_char_iter.next();
                    count = count + 1;
                    current_column = current_column + 1;
                } else {
                    tokens.push(Token::new(
                        TokenType::Operator(OperatorType::Assign),
                        current_line,
                        current_column,
                    ));
                }
            }
            '<' => {
                if *current_char_iter
                    .peek()
                    .expect("> is followed by 'None', should never happen")
                    == '='
                {
                    tokens.push(Token::new(
                        TokenType::Operator(OperatorType::CmpLessEqual),
                        current_line,
                        current_column,
                    ));
                    current_char_iter.next();
                    count = count + 1;
                    current_column = current_column + 1;
                } else {
                    tokens.push(Token::new(
                        TokenType::Operator(OperatorType::CmpLess),
                        current_line,
                        current_column,
                    ));
                }
            }
            '>' => {
                if *current_char_iter
                    .peek()
                    .expect("> is followed by 'None', should never happen")
                    == '='
                {
                    tokens.push(Token::new(
                        TokenType::Operator(OperatorType::CmpGreaterEqual),
                        current_line,
                        current_column,
                    ));
                    current_char_iter.next();
                    count = count + 1;
                    current_column = current_column + 1;
                } else {
                    tokens.push(Token::new(
                        TokenType::Operator(OperatorType::CmpGreater),
                        current_line,
                        current_column,
                    ));
                }
            }
            '!' => {
                if *current_char_iter
                    .peek()
                    .expect("! is followed by 'None', should never happen")
                    == '='
                {
                    tokens.push(Token::new(
                        TokenType::Operator(OperatorType::CmpNotEqual),
                        current_line,
                        current_column,
                    ));
                    current_char_iter.next();
                    count = count + 1;
                    current_column = current_column + 1;
                } else {
                    tokens.push(Token::new(
                        TokenType::Operator(OperatorType::Negate),
                        current_line,
                        current_column,
                    ));
                }
            }
            '&' => {
                if *current_char_iter
                    .peek()
                    .expect("!&is followed by 'None', should never happen")
                    == '&'
                {
                    tokens.push(Token::new(
                        TokenType::Operator(OperatorType::LogAnd),
                        current_line,
                        current_column,
                    ));
                    current_char_iter.next();
                    count = count + 1;
                    current_column = current_column + 1;
                } else {
                    tokens.push(Token::new(
                        TokenType::Operator(OperatorType::BitAnd),
                        current_line,
                        current_column,
                    ));
                }
            }
            '|' => {
                if *current_char_iter
                    .peek()
                    .expect("| is followed by 'None', should never happen")
                    == '|'
                {
                    tokens.push(Token::new(
                        TokenType::Operator(OperatorType::LogOr),
                        current_line,
                        current_column,
                    ));
                    current_char_iter.next();
                    count = count + 1;
                    current_column = current_column + 1;
                } else {
                    tokens.push(Token::new(
                        TokenType::Operator(OperatorType::BitOr),
                        current_line,
                        current_column,
                    ));
                }
            }
            '^' => {
                if *current_char_iter
                    .peek()
                    .expect("^ is followed by 'None', should never happen")
                    == '^'
                {
                    tokens.push(Token::new(
                        TokenType::Operator(OperatorType::LogXor),
                        current_line,
                        current_column,
                    ));
                    current_char_iter.next();
                    count = count + 1;
                    current_column = current_column + 1;
                } else {
                    tokens.push(Token::new(
                        TokenType::Operator(OperatorType::BitXor),
                        current_line,
                        current_column,
                    ));
                }
            }
            ';' => tokens.push(Token::new(
                TokenType::Semicolon,
                current_line,
                current_column,
            )),
            ',' => tokens.push(Token::new(TokenType::Comma, current_line, current_column)),
            '\n' => {
                current_line += 1;
                current_column = 0;
            }
            '(' => tokens.push(Token::new(
                TokenType::ParentheseOpen,
                current_line,
                current_column,
            )),
            ')' => tokens.push(Token::new(
                TokenType::ParentheseClose,
                current_line,
                current_column,
            )),
            '{' => tokens.push(Token::new(
                TokenType::CurlyOpen,
                current_line,
                current_column,
            )),
            '}' => tokens.push(Token::new(
                TokenType::CurlyClose,
                current_line,
                current_column,
            )),
            '[' => tokens.push(Token::new(
                TokenType::SquareOpen,
                current_line,
                current_column,
            )),
            ']' => tokens.push(Token::new(
                TokenType::SquareClose,
                current_line,
                current_column,
            )),
            '.' => {
                let start_column = current_column;
                let number = parse_number(
                    &source_code,
                    &mut current_line,
                    &mut current_column,
                    &mut count,
                    &mut current_char_iter,
                );
                match number {
                    Ok(t) => {
                        tokens.push(Token::new(
                            TokenType::ValueLiteral(t),
                            current_line,
                            start_column,
                        ));
                    }
                    Err(e) => {
                        error_list.push(e);
                    }
                }
            }
            i => {
                // Parsing an identifer
                if i.is_ascii_alphabetic() {
                    let start_column = current_column;
                    let token = parse_identifier(
                        &source_code,
                        &mut current_column,
                        &mut count,
                        &mut current_char_iter,
                    );
                    // Check if the identifier is a reserved keyword
                    let token_type = keywords.get(&token);
                    match token_type {
                        Some(i) => {
                            tokens.push(Token::new(i.clone(), current_line, start_column));
                        }
                        None => {
                            tokens.push(Token::new(
                                TokenType::Identifier(token),
                                current_line,
                                start_column,
                            ));
                        }
                    }
                }
                // Parsing a number
                else if i.is_numeric() {
                    let start_column = current_column;
                    let number = parse_number(
                        &source_code,
                        &mut current_line,
                        &mut current_column,
                        &mut count,
                        &mut current_char_iter,
                    );
                    match number {
                        Ok(t) => {
                            tokens.push(Token::new(
                                TokenType::ValueLiteral(t),
                                current_line,
                                start_column,
                            ));
                        }
                        Err(e) => {
                            error_list.push(e);
                        }
                    }
                // Check for illegal chars (like special ascii or unicode characters)
                } else if !i.is_whitespace() {
                    // File end
                    if i == '\0' {
                        break;
                    }
                    error_list.push(LexerError::new(
                        LexerErrorType::IllegalChar(current_char),
                        current_line,
                        current_column,
                    ));
                }
            }
        }
        current_column += 1;
    }
    if error_list.len() != 0 {
        Err(error_list)
    } else {
        Ok(tokens)
    }
}

// Creates a map with each keyword and their corresponding TokenTypes
fn get_keyword_map() -> HashMap<String, TokenType> {
    let mut keywords: HashMap<String, TokenType> = HashMap::new();
    keywords.insert("void".to_string(), TokenType::TypeName(DataType::Void));
    keywords.insert("byte".to_string(), TokenType::TypeName(DataType::Byte));
    keywords.insert("short".to_string(), TokenType::TypeName(DataType::Short));
    keywords.insert("int".to_string(), TokenType::TypeName(DataType::Int));
    keywords.insert("long".to_string(), TokenType::TypeName(DataType::Long));
    keywords.insert("float".to_string(), TokenType::TypeName(DataType::Float));
    keywords.insert("double".to_string(), TokenType::TypeName(DataType::Double));
    keywords.insert("String".to_string(), TokenType::TypeName(DataType::Zstring));
    keywords.insert("return".to_string(), TokenType::Return);
    keywords.insert("if".to_string(), TokenType::If);
    keywords.insert("else".to_string(), TokenType::Else);
    keywords.insert("while".to_string(), TokenType::While);
    keywords.insert("for".to_string(), TokenType::For);
    keywords.insert("new".to_string(), TokenType::New);
    keywords.insert("delete".to_string(), TokenType::Delete);
    keywords.insert("struct".to_string(), TokenType::Struct);
    keywords.insert(
        "true".to_string(),
        TokenType::ValueLiteral(LiteralType::Bool(true)),
    );
    keywords.insert(
        "false".to_string(),
        TokenType::ValueLiteral(LiteralType::Bool(false)),
    );
    keywords
}

// Parses an identifer consisting of characters or numerics
fn parse_identifier(
    source_code: &String,
    column: &mut u32,
    count: &mut u32,
    iter: &mut std::iter::Peekable<std::str::Chars<'_>>,
) -> String {
    let start = *count;
    while let Some(c) = iter.peek() {
        if !(c.is_ascii_alphabetic() || c.is_numeric()) {
            break;
        }
        iter.next();
        *count = *count + 1;
        *column = *column + 1;
    }

    source_code
        .chars()
        .skip((start - 1) as usize)
        .take((*count - start + 1) as usize)
        .collect()
}

// Parses a number, includes type specifying (like 5.0d)
fn parse_number(
    source_code: &String,
    line: &mut u32,
    column: &mut u32,
    count: &mut u32,
    iter: &mut std::iter::Peekable<std::str::Chars<'_>>,
) -> Result<LiteralType, LexerError> {
    let start = *count;
    let start_column = *column;
    let mut std_type = LiteralType::Int(0);
    // Parse integer values
    while let Some(c) = iter.peek() {
        if !c.is_numeric() {
            break;
        }
        iter.next();
        *count = *count + 1;
        *column = *column + 1;
    }
    // Check for floating point
    if let Some('.') = iter.peek() {
        std_type = LiteralType::Float(1.0);
        iter.next();
        *count = *count + 1;
        *column = *column + 1;
        while let Some(c) = iter.peek() {
            if !c.is_numeric() {
                break;
            }
            iter.next();
            *count = *count + 1;
            *column = *column + 1;
        }
    }
    // Check for suffixes
    let string: String = source_code
        .chars()
        .skip(start as usize - 1)
        .take((*count - start + 1) as usize)
        .collect();
    let mut found_type = true;
    let mut res;
    // Assign the type + parse the string
    match iter.peek() {
        Some('b') => {
            let b = string.parse::<i8>();
            println!("{}", string);
            if b.is_ok() {
                res = Ok(LiteralType::Byte(b.unwrap()));
            } else {
                res = Err(LexerError::new(
                    LexerErrorType::IllegalNumber(string),
                    *line,
                    start_column,
                ));
            }
        }
        Some('s') => {
            let s = string.parse::<i16>();
            if s.is_ok() {
                res = Ok(LiteralType::Short(s.unwrap()));
            } else {
                res = Err(LexerError::new(
                    LexerErrorType::IllegalNumber(string),
                    *line,
                    start_column,
                ));
            }
        }
        Some('i') => {
            let i = string.parse::<i32>();
            if i.is_ok() {
                res = Ok(LiteralType::Int(i.unwrap()));
            } else {
                res = Err(LexerError::new(
                    LexerErrorType::IllegalNumber(string),
                    *line,
                    start_column,
                ));
            }
        }
        Some('l') => {
            let l = string.parse::<i64>();
            if l.is_ok() {
                res = Ok(LiteralType::Long(l.unwrap()));
            } else {
                res = Err(LexerError::new(
                    LexerErrorType::IllegalNumber(string),
                    *line,
                    start_column,
                ));
            }
        }
        Some('f') => {
            let f = string.parse::<f32>();
            if f.is_ok() {
                res = Ok(LiteralType::Float(f.unwrap()));
            } else {
                res = Err(LexerError::new(
                    LexerErrorType::IllegalNumber(string),
                    *line,
                    start_column,
                ));
            }
        }
        Some('d') => {
            let d = string.parse::<f64>();
            if d.is_ok() {
                res = Ok(LiteralType::Double(d.unwrap()));
            } else {
                res = Err(LexerError::new(
                    LexerErrorType::IllegalNumber(string),
                    *line,
                    start_column,
                ));
            }
        }
        _ => {
            match std_type {
                LiteralType::Int(_i) => {
                    let i = string.parse::<i32>();
                    if i.is_ok() {
                        res = Ok(LiteralType::Int(i.unwrap()));
                    } else {
                        res = Err(LexerError::new(
                            LexerErrorType::IllegalNumber(string),
                            *line,
                            start_column,
                        ));
                    }
                }
                LiteralType::Float(_f) => {
                    let f = string.parse::<f32>();
                    if f.is_ok() {
                        res = Ok(LiteralType::Float(f.unwrap()));
                    } else {
                        res = Err(LexerError::new(
                            LexerErrorType::IllegalNumber(string),
                            *line,
                            start_column,
                        ));
                    }
                }
                _ => {
                    res = Err(LexerError::new(
                        LexerErrorType::IllegalChar('?'),
                        *line,
                        *column,
                    ));
                }
            }
            found_type = false;
        }
    }
    if found_type {
        iter.next();
        *count = *count + 1;
        *column = *column + 1;
    }
    res
}

// Parses a multi-line comment
fn parse_comment(
    line: &mut u32,
    column: &mut u32,
    count: &mut u32,
    iter: &mut std::iter::Peekable<std::str::Chars<'_>>,
) {
    while let Some(c) = iter.next() {
        *count = *count + 1;
        *column = *column + 1;
        match c {
            '\n' => {
                *line = *line + 1;
                *column = 1;
            }
            '*' => {
                while let Some('*') = iter.peek() {
                    iter.next();
                    *count = *count + 1;
                    *column = *column + 1;
                }
                if let Some('/') = iter.peek() {
                    iter.next();
                    *count = *count + 1;
                    *column = *column + 1;
                    break;
                }
            }
            _ => {}
        }
    }
}

// Parses a single-line comment
fn parse_singleline_comment(
    line: &mut u32,
    column: &mut u32,
    count: &mut u32,
    iter: &mut std::iter::Peekable<std::str::Chars<'_>>,
) {
    while let Some(c) = iter.next() {
        *count = *count + 1;
        if c == '\n' {
            *line = *line + 1;
            *column = 1;
            break;
        }
    }
}
