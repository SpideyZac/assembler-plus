use crate::ast as ast;
use crate::error::{Error, ErrorType};
use crate::tokens::{Token, TokenType};

pub struct Parser {
    tokens: Vec<Token>,
    current_index: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current_index: 0,
        }
    }

    fn expect(&mut self, token_type: TokenType) -> Result<Token, Error> {
        if self.current_index >= self.tokens.len() {
            return Err(Error::new(
                ErrorType::UnexpectedEOF,
                "Unexpected end of file".to_string(),
                self.tokens[self.tokens.len() - 1].line,
                self.tokens[self.tokens.len() - 1].column,
            ));
        }
        if self.tokens[self.current_index].token_type == token_type {
            let token = self.tokens[self.current_index].clone();
            self.current_index += 1;
            Ok(token)
        } else {
            Err(Error::new(
                ErrorType::UnexpectedToken,
                format!(
                    "Expected token {:?}, found {:?}",
                    token_type, self.tokens[self.current_index].token_type
                ),
                self.tokens[self.current_index].line,
                self.tokens[self.current_index].column,
            ))
        }
    }

    fn option(&mut self, token_type: TokenType) -> Option<Token> {
        if self.current_index >= self.tokens.len() {
            return None;
        }
        if self.tokens[self.current_index].token_type == token_type {
            let token = self.tokens[self.current_index].clone();
            self.current_index += 1;
            Some(token)
        } else {
            None
        }
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.current_index >= self.tokens.len() {
            return false;
        }
        self.tokens[self.current_index].token_type == token_type
    }

    pub fn parse(&mut self) -> Result<ast::Program, Error> {
        let mut statements: Vec<ast::Statement> = Vec::new();
        while self.current_index < self.tokens.len() {
            if self.check(TokenType::NewLine) {
                self.current_index += 1;
                continue;
            } else if self.check(TokenType::EOF) {
                break;
            }
            let statement = self.parse_statement();
            match statement {
                Ok(statement) => statements.push(statement),
                Err(error) => return Err(error),
            }
        }
        Ok(ast::Program { statements })
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, Error> {
        let token = self.tokens[self.current_index].clone();
        match token.token_type {
            TokenType::Label => {
                let result = self.parse_label();
                let _ = self.option(TokenType::NewLine);

                match result {
                    Ok(label) => Ok(ast::Statement::Label(label)),
                    Err(error) => Err(error),
                }
            }
            TokenType::Symbol => {
                let result = self.parse_symbol();
                let res = self.expect(TokenType::NewLine);

                let toret = match result {
                    Ok(instruction) => Ok(ast::Statement::Instruction(instruction)),
                    Err(error) => Err(error),
                };
                match res {
                    Ok(_) => toret,
                    Err(error) => {
                        if self.check(TokenType::EOF) {
                            toret
                        } else {
                            if toret.is_err() {
                                return Err(toret.err().unwrap());
                            }
                            Err(error)
                        }
                    },
                }
            }
            _ => Err(Error::new(
                ErrorType::InvalidStatement,
                "Invalid statement".to_string(),
                token.line,
                token.column,
            )),
        }
    }

    fn parse_label(&mut self) -> Result<String, Error> {
        let token = self.expect(TokenType::Label);
        if token.is_err() {
            return Err(token.unwrap_err());
        }
        Ok(format!(".{}", token.unwrap().value.unwrap()))
    }

    fn parse_symbol(&mut self) -> Result<ast::Instruction, Error> {
        let token = self.expect(TokenType::Symbol);
        if token.is_err() {
            return Err(token.unwrap_err());
        }
        let token = token.unwrap();
        let instruction = token.value.clone().unwrap();
        let mut operands: Vec<ast::InstructionOperand> = Vec::new();
        let instr = match instruction.to_lowercase().as_str() {
            "nop" => ast::InstructionType::NOP,
            _ => {
                return Err(Error::new(
                    ErrorType::InvalidInstruction,
                    format!("Invalid instruction: {}", instruction),
                    token.line,
                    token.column,
                ))
            }
        };

        while !self.check(TokenType::NewLine) && !self.check(TokenType::EOF) {
            let operand = self.parse_stmt_operand();
            match operand {
                Ok(operand) => operands.push(operand),
                Err(error) => return Err(error),
            }
        }

        let check = instr.check_args(operands.clone(), &token);
        if check.is_err() {
            return Err(check.err().unwrap());
        }

        Ok(ast::Instruction {
            instruction_type: instr,
            operands,
        })
    }

    fn parse_stmt_operand(&mut self) -> Result<ast::InstructionOperand, Error> {
        let token = self.tokens[self.current_index].clone();
        match token.token_type {
            TokenType::Int => {
                let int = self.parse_int();
                if int.is_err() {
                    return Err(int.err().unwrap());
                }
                Ok(ast::InstructionOperand::Int(int.unwrap()))
            }
            TokenType::Symbol => {
                let register = self.parse_register();
                if register.is_err() {
                    return Err(register.err().unwrap());
                }
                Ok(ast::InstructionOperand::Register(register.unwrap()))
            }
            _ => Err(Error::new(
                ErrorType::InvalidOperand,
                "Invalid operand".to_string(),
                token.line,
                token.column,
            )),
        }
    }

    fn parse_int(&mut self) -> Result<i8, Error> {
        let token = self.expect(TokenType::Int);
        if token.is_err() {
            return Err(token.unwrap_err());
        }
        Ok(token.unwrap().value.unwrap().parse::<i8>().unwrap())
    }

    fn parse_register(&mut self) -> Result<String, Error> {
        let token = self.expect(TokenType::Symbol);
        if token.is_err() {
            return Err(token.unwrap_err());
        }
        let token = token.unwrap();
        let value = token.value.unwrap();
        if !value.starts_with("r") {
            return Err(Error::new(
                ErrorType::InvalidRegister,
                format!("Invalid register: {}", value),
                token.line,
                token.column,
            ));
        }
        if !value[1..].chars().all(char::is_numeric) {
            return Err(Error::new(
                ErrorType::InvalidRegister,
                format!("Invalid register: {}", value),
                token.line,
                token.column,
            ));
        }
        let num = value[1..].parse::<i8>().unwrap();
        if num < 0 || num > 15 {
            return Err(Error::new(
                ErrorType::InvalidRegister,
                format!("Invalid register: {}", value),
                token.line,
                token.column,
            ));
        }

        Ok(value)
    }
}
