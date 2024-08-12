use crate::lexer::{Mnemonic, TokenKind};
use crate::parser::{AstStmt, AstStmtOperand, Statement, __token_ast_Token};

use laps::log_error;
use laps::span::{Error, Span};

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

    fn _check_stmt_operand(&mut self, operand: AstStmtOperand, correct: Vec<&str>) -> bool {
        match operand {
            AstStmtOperand::Register(_) => {
                if !correct.contains(&"register") {
                    return false;
                }
            }
            AstStmtOperand::Label(_) => {
                if !correct.contains(&"label") {
                    return false;
                }
            }
            AstStmtOperand::Int(_) => {
                if !correct.contains(&"int") {
                    return false;
                }
            }
            AstStmtOperand::Identifier(_) => {
                if !correct.contains(&"identifier") {
                    return false;
                }
            }
            AstStmtOperand::Char(_) => {
                if !correct.contains(&"char") {
                    return false;
                }
            }
        }
        true
    }

    fn check_stmt_operands(
        &mut self,
        span: Span,
        operands: Vec<AstStmtOperand>,
        correct: Vec<Vec<&str>>,
    ) -> Result<(), Error> {
        if operands.len() != correct.len() {
            eval_err!(
                span,
                "expected {} operands, got {}",
                correct.len(),
                operands.len()
            )
        }

        for (i, operand) in operands.iter().enumerate() {
            let expected = correct[i].clone();
            match operand {
                AstStmtOperand::Register(r) => {
                    if !expected.contains(&"register") {
                        eval_err!(
                            r.0.span.clone(),
                            "expected {} operand, got register",
                            expected.join(" or ")
                        )
                    }
                }
                AstStmtOperand::Label(l) => {
                    if !expected.contains(&"label") {
                        eval_err!(
                            l.0.span.clone(),
                            "expected {} operand, got label",
                            expected.join(" or ")
                        )
                    }
                }
                AstStmtOperand::Int(i) => {
                    if !expected.contains(&"int") {
                        eval_err!(
                            i.0.span.clone(),
                            "expected {} operand, got int",
                            expected.join(" or ")
                        )
                    }
                }
                AstStmtOperand::Identifier(ident) => {
                    if !expected.contains(&"identifier") {
                        eval_err!(
                            ident.0.span.clone(),
                            "expected {} operand, got identifier",
                            expected.join(" or ")
                        )
                    }
                }
                AstStmtOperand::Char(c) => {
                    if !expected.contains(&"char") {
                        eval_err!(
                            c.0.span.clone(),
                            "expected {} operand, got char",
                            expected.join(" or ")
                        )
                    }
                }
            }
        }
        Ok(())
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

        if let TokenKind::Mnemonic(m) = mnemonic.kind {
            match m {
                Mnemonic::Define => {
                    self.check_stmt_operands(
                        mnemonic.span.clone(),
                        operands.clone(),
                        vec![vec!["identifier"], vec!["int", "char"]],
                    )?;
                    return Ok(format!("{:?} {:?}\n", operands[0], operands[1]));
                }
                _ => eval_err!(mnemonic.span.clone(), "mnemonic not supported"),
            }
        }
        panic!("unreachable")
    }

    fn generate_label_decl(&mut self, label: __token_ast_Token::Token3) -> Result<String, Error> {
        let label = label.0;
        eval_err!(label.span.clone(), "label declaration not supported")
    }
}
