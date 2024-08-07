use crate::error::{Error, ErrorType};
use crate::tokens::{Token, TokenType};

pub struct Lexer {
    code: String,
    tokens: Vec<Token>,
    current_char: char,
    current_index: usize,
    current_line: usize,
    current_column: usize,
}

impl Lexer {
    pub fn new(code: String) -> Self {
        let mut lexer = Self {
            code,
            tokens: Vec::new(),
            current_char: ' ',
            current_index: 0,
            current_line: 1,
            current_column: 1,
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        if self.current_index >= self.code.len() {
            self.current_char = '\0';
        } else {
            self.current_char = self.code.chars().nth(self.current_index).unwrap();
        }
        self.current_index += 1;
        self.current_column += 1;
    }

    fn _peek_char(&self) -> char {
        if self.current_index >= self.code.len() {
            '\0'
        } else {
            self.code.chars().nth(self.current_index).unwrap()
        }
    }

    fn skip_whitespace(&mut self) {
        while self.current_char.is_whitespace() {
            if self.current_char == '\n' {
                self.current_line += 1;
                self.current_column = 1;
            }
            self.read_char();
        }
    }

    fn lex_int(&mut self) -> String {
        let mut int = String::new();
        while self.current_char.is_digit(10) {
            int.push(self.current_char);
            self.read_char();
        }
        int
    }

    fn lex_identifier(&mut self) -> String {
        let mut identifier = String::new();
        while self.current_char.is_alphanumeric() {
            identifier.push(self.current_char);
            self.read_char();
        }
        identifier
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, Error> {
        while self.current_char != '\0' {
            self.skip_whitespace();
            let token = match self.current_char {
                '\0' => Token::new(
                    TokenType::EOF,
                    None,
                    self.current_line,
                    self.current_column,
                ),
                '%' => {
                    self.read_char();
                    Token::new(
                        TokenType::Percent,
                        None,
                        self.current_line,
                        self.current_column,
                    )
                }
                '.' => {
                    self.read_char();
                    Token::new(TokenType::Dot, None, self.current_line, self.current_column)
                }
                _ if self.current_char.is_digit(10) => {
                    let int = self.lex_int();
                    Token::new(
                        TokenType::Int,
                        Some(int),
                        self.current_line,
                        self.current_column,
                    )
                }
                _ if self.current_char.is_alphabetic() => {
                    let identifier = self.lex_identifier();
                    Token::new(
                        TokenType::Identifier,
                        Some(identifier),
                        self.current_line,
                        self.current_column,
                    )
                }
                _ => {
                    return Err(Error::new(
                        ErrorType::InvalidCharacter,
                        format!("Invalid character: {}", self.current_char),
                        self.current_line,
                        self.current_column,
                    ));
                }
            };
            self.tokens.push(token);
        }
        Ok(self.tokens.clone())
    }

    pub fn get_tokens(&self) -> Vec<Token> {
        self.tokens.clone()
    }
}