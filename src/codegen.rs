use crate::parser::{AstStmt, AstStmtOperand, Statement, __token_ast_Token};

use laps::log_error;
use laps::span::Error;

macro_rules! eval_err {
    ($span:expr, $($arg:tt)+) => {
        return Err(log_error!($span, $($arg)+))
    };
}

pub struct Codegen {
    statements: Vec<Statement>,
}

impl Codegen {
    pub fn new(statements: Vec<Statement>) -> Self {
        Self { statements }
    }

    pub fn generate(&mut self) -> Result<String, Error> {
        let mut output = String::new();
        for stmt in self.statements.clone() {
            let out = self.generate_stmt(stmt)?;
            output.push_str(&out);
        }
        Ok(output)
    }

    fn generate_stmt(&mut self, stmt: Statement) -> Result<String, Error> {
        match stmt {
            Statement::AstStmt(ast) => {
                let out = self.generate_ast_stmt(ast)?;
                Ok(out)
            }
            Statement::Eof(_) => Ok(String::new()),
            Statement::Label(label) => {
                let out = self.generate_label_decl(label)?;
                Ok(out)
            }
            Statement::Newline(_) => Ok(String::new()),
        }
    }

    fn generate_ast_stmt(&mut self, ast: AstStmt) -> Result<String, Error> {
        let mnemonic = ast.mnemonic.0;
        let operands = ast.operands;
        eval_err!(mnemonic.span.clone(), "mnemonic not supported")
    }

    fn generate_label_decl(&mut self, label: __token_ast_Token::Token3) -> Result<String, Error> {
        let label = label.0;
        eval_err!(label.span.clone(), "label declaration not supported")
    }
}
