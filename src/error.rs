pub enum ErrorType {
    // Lexer errors
    InvalidCharacter = 1,
    // Parser errors
    // Compiler errors
}

pub struct Error {
    pub error_type: ErrorType,
    pub message: String,
    pub line: usize,
    pub column: usize,
}

impl Error {
    pub fn new(error_type: ErrorType, message: String, line: usize, column: usize) -> Self {
        Self {
            error_type,
            message,
            line,
            column,
        }
    }
}
