use crate::{error::{Error, ErrorType}, tokens::Token};

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Label(String),
    Instruction(Instruction),
}

#[derive(Debug)]
pub enum InstructionType {
    NOP,
}

impl InstructionType {
    pub fn check_args(&self, operands: Vec<InstructionOperand>, token: &Token) -> Result<(), Error> {
        match self {
            InstructionType::NOP => {
                if operands.len() != 0 {
                    return Err(Error::new(
                        ErrorType::InvalidNumberOfOperands,
                        "Invalid number of operands, expected 0".to_string(),
                        token.line,
                        token.column,
                    ));
                }
                Ok(())
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum InstructionOperand {
    Register(String),
    Int(i8),
}

#[derive(Debug)]
pub struct Instruction {
    pub instruction_type: InstructionType,
    pub operands: Vec<InstructionOperand>,
}
