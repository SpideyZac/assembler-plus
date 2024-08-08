#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    EOF,
    NewLine,
    Int,
    Label,
    Symbol,
    MacroSymbol,
    MacroExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub value: Option<String>,
    pub line: usize,
    pub column: usize,
}

impl Token {
    pub fn new(token_type: TokenType, value: Option<String>, line: usize, column: usize) -> Self {
        Self {
            token_type,
            value,
            line,
            column,
        }
    }
}
