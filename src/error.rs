#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ErrorType {
    // Lexer errors
    InvalidCharacter = 1,
    // Parser errors
    UnexpectedEOF = 2,
    UnexpectedToken = 3,
    InvalidStatement = 4,
    InvalidInstruction = 5,
    InvalidOperand = 6,
    InvalidRegister = 7,
    InvalidNumberOfOperands = 8,
    InvalidOperandType = 9,
    // Compiler errors
}

#[derive(Debug)]
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
