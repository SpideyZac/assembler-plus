use std::collections::HashMap;

use crate::lexer::{Mnemonic, TokenKind};
use crate::parser::{AstStmt, AstStmtOperand, Statement, __token_ast_Token};

use laps::log_error;
use laps::span::{Error, Span};

macro_rules! eval_err {
    ($span:expr, $($arg:tt)+) => {
        return Err(log_error!($span, $($arg)+))
    };
}

enum StmtValue {
    Int(i64),
    Char(u8),
}

pub struct Codegen {
    statements: Vec<Statement>,
    symbol_table: HashMap<String, StmtValue>,
    labels_table: HashMap<String, u64>,
    instruction_pointer: u64,
}

impl Codegen {
    pub fn new(statements: Vec<Statement>) -> Self {
        let mut res = Self {
            statements,
            symbol_table: HashMap::new(),
            labels_table: HashMap::new(),
            instruction_pointer: 0,
        };

        res.symbol_table.insert("eq".to_string(), StmtValue::Int(0));
        res.symbol_table.insert("ne".to_string(), StmtValue::Int(1));
        res.symbol_table.insert("ge".to_string(), StmtValue::Int(2));
        res.symbol_table.insert("lt".to_string(), StmtValue::Int(3));
        res.symbol_table.insert("=".to_string(), StmtValue::Int(0));
        res.symbol_table.insert("!=".to_string(), StmtValue::Int(1));
        res.symbol_table.insert(">=".to_string(), StmtValue::Int(2));
        res.symbol_table.insert("<".to_string(), StmtValue::Int(3));
        res.symbol_table.insert("z".to_string(), StmtValue::Int(0));
        res.symbol_table.insert("nz".to_string(), StmtValue::Int(1));
        res.symbol_table.insert("c".to_string(), StmtValue::Int(2));
        res.symbol_table.insert("nc".to_string(), StmtValue::Int(3));
        res.symbol_table
            .insert("zero".to_string(), StmtValue::Int(0));
        res.symbol_table
            .insert("nonzero".to_string(), StmtValue::Int(1));
        res.symbol_table
            .insert("carry".to_string(), StmtValue::Int(2));
        res.symbol_table
            .insert("no_carry".to_string(), StmtValue::Int(3));

        let ports = [
            "pixel_x",
            "pixel_y",
            "draw_pixel",
            "clear_pixel",
            "load_pixel",
            "buffer_screen",
            "clear_screen_buffer",
            "write_char",
            "buffer_chars",
            "clear_chars_buffer",
            "show_number",
            "clear_number",
            "signed_mode",
            "unsigned_mode",
            "rng",
            "controller_input",
        ];
        for (i, port) in ports.iter().enumerate() {
            res.symbol_table
                .insert(port.to_string(), StmtValue::Int(i as i64));
        }

        res
    }

    pub fn generate(&mut self) -> Result<String, Error> {
        let mut output = String::new();
        for stmt in self.statements.clone() {
            self.generate_preprocess(stmt)?;
        }
        for stmt in self.statements.clone() {
            let out = self.generate_stmt(stmt)?;
            output.push_str(&out);
        }
        Ok(output)
    }

    fn generate_preprocess(&mut self, stmt: Statement) -> Result<(), Error> {
        match stmt {
            Statement::AstStmt(ast) => {
                self.generate_ast_preprocess(ast)?;
                Ok(())
            }
            Statement::Label(label) => {
                self.generate_label_preprocess(label)?;
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn generate_ast_preprocess(&mut self, ast: AstStmt) -> Result<(), Error> {
        match ast.mnemonic.0.kind {
            TokenKind::Mnemonic(mnemonic) => match mnemonic {
                Mnemonic::Define => {
                    let ident = match ast.operands[0].clone() {
                        AstStmtOperand::Identifier(ident) => match ident.0.kind {
                            TokenKind::Identifier(i) => i,
                            _ => panic!("unreachable"),
                        },
                        _ => panic!("unreachable"),
                    };
                    let value = match ast.operands[1].clone() {
                        AstStmtOperand::Int(i) => match i.0.kind {
                            TokenKind::Int(int) => StmtValue::Int(int as i64),
                            _ => panic!("unreachable"),
                        },
                        AstStmtOperand::Char(c) => match c.0.kind {
                            TokenKind::Char(char) => StmtValue::Char(char.value),
                            _ => panic!("unreachable"),
                        },
                        _ => panic!("unreachable"),
                    };
                    if self.symbol_table.contains_key(&ident.to_string()) {
                        eval_err!(ast.mnemonic.0.span.clone(), "redefinition of '{}'", ident);
                    }
                    self.symbol_table.insert(ident, value);
                }
                _ => {
                    self.instruction_pointer += 1;
                }
            },
            _ => (),
        }

        Ok(())
    }

    fn generate_label_preprocess(&mut self, label: __token_ast_Token::Token3) -> Result<(), Error> {
        let name = match label.0.kind {
            TokenKind::Label(l) => l.name,
            _ => panic!("unreachable"),
        };
        if self.labels_table.contains_key(&name) {
            eval_err!(label.0.span.clone(), "redefinition of label '{}'", name);
        }
        self.labels_table.insert(name, self.instruction_pointer);
        Ok(())
    }

    fn check_stmt_operand(
        &self,
        span: Span,
        operand: &StmtValue,
        correct: Vec<&str>,
    ) -> Result<(), Error> {
        match operand {
            StmtValue::Int(_) => {
                if !correct.contains(&"int") {
                    eval_err!(
                        span,
                        "{}",
                        format!("expected {}, got integer literal", correct.join(" or "))
                    );
                }
            }
            StmtValue::Char(_) => {
                if !correct.contains(&"char") {
                    eval_err!(
                        span,
                        "{}",
                        format!("expected {}, got character literal", correct.join(" or "))
                    );
                }
            }
        }

        Ok(())
    }

    fn check_stmt_operands(
        &self,
        span: Span,
        operands: &Vec<AstStmtOperand>,
        correct: Vec<Vec<&str>>,
    ) -> Result<(), Error> {
        if operands.len() != correct.len() {
            eval_err!(
                span,
                "expected {} operands, got {}",
                correct.len(),
                operands.len()
            );
        }

        for (i, operand) in operands.iter().enumerate() {
            let expected = correct[i].clone();
            match operand {
                AstStmtOperand::Register(r) => {
                    if !expected.contains(&"register") {
                        eval_err!(
                            r.0.span.clone(),
                            "{}",
                            format!("expected {}, got register", expected.join(" or "))
                        );
                    }
                }
                AstStmtOperand::Label(l) => {
                    if !expected.contains(&"int") {
                        eval_err!(
                            l.0.span.clone(),
                            "{}",
                            format!("expected {}, got label", expected.join(" or "))
                        );
                    }
                }
                AstStmtOperand::Int(i) => {
                    if !expected.contains(&"int") {
                        eval_err!(
                            i.0.span.clone(),
                            "{}",
                            format!("expected {}, got integer literal", expected.join(" or "))
                        );
                    }
                }
                AstStmtOperand::Identifier(ident) => {
                    if !expected.contains(&"identifier") {
                        if let TokenKind::Identifier(i) = &ident.0.kind {
                            let value = self.symbol_table.get(&i.to_string());
                            if value.is_none() {
                                eval_err!(
                                    ident.0.span.clone(),
                                    "{}",
                                    format!("expected {}, got identifier", expected.join(" or "))
                                );
                            }
                            self.check_stmt_operand(
                                ident.0.span.clone(),
                                value.unwrap(),
                                expected.clone(),
                            )?;
                            return Ok(());
                        }
                        panic!("unreachable");
                    }
                }
                AstStmtOperand::Char(c) => {
                    if !expected.contains(&"int") {
                        eval_err!(
                            c.0.span.clone(),
                            "{}",
                            format!("expected {}, got character literal", expected.join(" or "))
                        );
                    }
                }
            }
        }

        Ok(())
    }

    fn generate_stmt(&mut self, stmt: Statement) -> Result<String, Error> {
        match stmt {
            Statement::AstStmt(ast) => {
                match ast.mnemonic.0.kind {
                    TokenKind::Mnemonic(m) => {
                        match m {
                            Mnemonic::Define => Ok(String::new()), // handle defines before generating code
                            _ => {
                                let res = self.generate_ast_stmt(ast)?;
                                Ok(res)
                            }
                        }
                    }
                    _ => panic!("unreachable"),
                }
            }
            Statement::Label(_) => Ok(String::new()), // handle labels before generating code
            Statement::Eof(_) => Ok(String::new()),
            Statement::Newline(_) => Ok(String::new()),
        }
    }

    fn generate_ast_stmt(&mut self, ast: AstStmt) -> Result<String, Error> {
        let mnemonic = ast.mnemonic.0.kind;
        let operands = ast.operands;
        let span = ast.mnemonic.0.span.clone();
        let machine_code;

        match mnemonic {
            TokenKind::Mnemonic(mnemonic) => match mnemonic {
                Mnemonic::Define => panic!("unreachable"),
                Mnemonic::Nop => {
                    self.check_stmt_operands(span.clone(), &operands, vec![])?;
                    machine_code = 0;
                }
                Mnemonic::Hlt => {
                    self.check_stmt_operands(span.clone(), &operands, vec![])?;
                    machine_code = 1 << 12;
                }
                Mnemonic::Add => {
                    self.check_stmt_operands(
                        span.clone(),
                        &operands,
                        vec![vec!["register"], vec!["register"], vec!["register"]],
                    )?;
                    let rd = self.generate_stmt_operand(operands[0].clone())?;
                    let rs = self.generate_stmt_operand(operands[1].clone())?;
                    let rt = self.generate_stmt_operand(operands[2].clone())?;
                    machine_code = 2 << 12 | (rd << 8) | (rs << 4) | rt;
                }
                Mnemonic::Sub => {
                    self.check_stmt_operands(
                        span.clone(),
                        &operands,
                        vec![vec!["register"], vec!["register"], vec!["register"]],
                    )?;
                    let rd = self.generate_stmt_operand(operands[0].clone())?;
                    let rs = self.generate_stmt_operand(operands[1].clone())?;
                    let rt = self.generate_stmt_operand(operands[2].clone())?;
                    machine_code = 3 << 12 | (rd << 8) | (rs << 4) | rt;
                }
                Mnemonic::Nor => {
                    self.check_stmt_operands(
                        span.clone(),
                        &operands,
                        vec![vec!["register"], vec!["register"], vec!["register"]],
                    )?;
                    let rd = self.generate_stmt_operand(operands[0].clone())?;
                    let rs = self.generate_stmt_operand(operands[1].clone())?;
                    let rt = self.generate_stmt_operand(operands[2].clone())?;
                    machine_code = 4 << 12 | (rd << 8) | (rs << 4) | rt;
                }
                Mnemonic::And => {
                    self.check_stmt_operands(
                        span.clone(),
                        &operands,
                        vec![vec!["register"], vec!["register"], vec!["register"]],
                    )?;
                    let rd = self.generate_stmt_operand(operands[0].clone())?;
                    let rs = self.generate_stmt_operand(operands[1].clone())?;
                    let rt = self.generate_stmt_operand(operands[2].clone())?;
                    machine_code = 5 << 12 | (rd << 8) | (rs << 4) | rt;
                }
                Mnemonic::Xor => {
                    self.check_stmt_operands(
                        span.clone(),
                        &operands,
                        vec![vec!["register"], vec!["register"], vec!["register"]],
                    )?;
                    let rd = self.generate_stmt_operand(operands[0].clone())?;
                    let rs = self.generate_stmt_operand(operands[1].clone())?;
                    let rt = self.generate_stmt_operand(operands[2].clone())?;
                    machine_code = 6 << 12 | (rd << 8) | (rs << 4) | rt;
                }
                Mnemonic::Rsh => {
                    self.check_stmt_operands(
                        span.clone(),
                        &operands,
                        vec![vec!["register"], vec!["register"]],
                    )?;
                    let rd = self.generate_stmt_operand(operands[0].clone())?;
                    let rs = self.generate_stmt_operand(operands[1].clone())?;
                    machine_code = 7 << 12 | (rd << 8) | rs;
                }
                Mnemonic::Ldi => {
                    self.check_stmt_operands(
                        span.clone(),
                        &operands,
                        vec![vec!["register"], vec!["int"]],
                    )?;
                    let rd = self.generate_stmt_operand(operands[0].clone())?;
                    let imm = self.generate_stmt_operand(operands[1].clone())?;
                    if imm < -128 || imm > 255 {
                        eval_err!(
                            span.clone(),
                            "immediate value {} out of range [-128, 255]",
                            imm
                        );
                    }
                    machine_code = 8 << 12 | (rd << 8) | imm;
                }
                Mnemonic::Adi => {
                    self.check_stmt_operands(
                        span.clone(),
                        &operands,
                        vec![vec!["register"], vec!["int"]],
                    )?;
                    let rd = self.generate_stmt_operand(operands[0].clone())?;
                    let imm = self.generate_stmt_operand(operands[1].clone())?;
                    if imm < -128 || imm > 255 {
                        eval_err!(
                            span.clone(),
                            "immediate value {} out of range [-128, 255]",
                            imm
                        );
                    }
                    machine_code = 9 << 12 | (rd << 8) | imm;
                }
                Mnemonic::Jmp => {
                    self.check_stmt_operands(span.clone(), &operands, vec![vec!["int"]])?;
                    let addr = self.generate_stmt_operand(operands[0].clone())?;
                    if addr != (addr % 1024) {
                        eval_err!(span.clone(), "jump address {} out of range [0, 1023]", addr);
                    }
                    machine_code = 10 << 12 | addr;
                }
                Mnemonic::Brh => {
                    self.check_stmt_operands(
                        span.clone(),
                        &operands,
                        vec![vec!["int"], vec!["int"]],
                    )?;
                    let cond = self.generate_stmt_operand(operands[0].clone())?;
                    let addr = self.generate_stmt_operand(operands[1].clone())?;
                    if addr != (addr % 1024) {
                        eval_err!(span.clone(), "jump address {} out of range [0, 1023]", addr);
                    }
                    machine_code = 11 << 12 | (cond << 8) | addr;
                }
                Mnemonic::Cal => {
                    self.check_stmt_operands(span.clone(), &operands, vec![vec!["int"]])?;
                    let addr = self.generate_stmt_operand(operands[0].clone())?;
                    machine_code = 12 << 12 | addr;
                }
                Mnemonic::Ret => {
                    self.check_stmt_operands(span.clone(), &operands, vec![])?;
                    machine_code = 13 << 12;
                }
                Mnemonic::Lod => {
                    self.check_stmt_operands(
                        span.clone(),
                        &operands,
                        vec![vec!["register"], vec!["register"], vec!["int"]],
                    )?;
                    let rd = self.generate_stmt_operand(operands[0].clone())?;
                    let rs = self.generate_stmt_operand(operands[1].clone())?;
                    let offset = self.generate_stmt_operand(operands[2].clone())?;
                    if offset < -8 || offset > 7 {
                        eval_err!(span.clone(), "offset value {} out of range [-8, 7]", offset);
                    }
                    machine_code = 14 << 12 | (rd << 8) | (rs << 4) | offset;
                }
                Mnemonic::Str => {
                    self.check_stmt_operands(
                        span.clone(),
                        &operands,
                        vec![vec!["register"], vec!["register"], vec!["int"]],
                    )?;
                    let rd = self.generate_stmt_operand(operands[0].clone())?;
                    let rs = self.generate_stmt_operand(operands[1].clone())?;
                    let offset = self.generate_stmt_operand(operands[2].clone())?;
                    if offset < -8 || offset > 7 {
                        eval_err!(span.clone(), "offset value {} out of range [-8, 7]", offset);
                    }
                    machine_code = 15 << 12 | (rd << 8) | (rs << 4) | offset;
                }
                Mnemonic::Cmp => {
                    self.check_stmt_operands(
                        span.clone(),
                        &operands,
                        vec![vec!["register"], vec!["register"]],
                    )?;
                    let rs = self.generate_stmt_operand(operands[0].clone())?;
                    let rt = self.generate_stmt_operand(operands[1].clone())?;
                    machine_code = 3 << 12 | (rs << 8) | (rt << 4);
                }
                Mnemonic::Mov => {
                    self.check_stmt_operands(
                        span.clone(),
                        &operands,
                        vec![vec!["register"], vec!["register"]],
                    )?;
                    let rd = self.generate_stmt_operand(operands[0].clone())?;
                    let rs = self.generate_stmt_operand(operands[1].clone())?;
                    machine_code = 2 << 12 | (rd << 8) | rs;
                }
                Mnemonic::Lsh => {
                    self.check_stmt_operands(
                        span.clone(),
                        &operands,
                        vec![vec!["register"], vec!["register"]],
                    )?;
                    let rd = self.generate_stmt_operand(operands[0].clone())?;
                    let rs = self.generate_stmt_operand(operands[1].clone())?;
                    machine_code = 2 << 12 | (rd << 8) | (rd << 4) | rs;
                }
                Mnemonic::Inc => {
                    self.check_stmt_operands(span.clone(), &operands, vec![vec!["register"]])?;
                    let rd = self.generate_stmt_operand(operands[0].clone())?;
                    machine_code = 9 << 12 | (rd << 8) | 1;
                }
                Mnemonic::Dec => {
                    self.check_stmt_operands(span.clone(), &operands, vec![vec!["register"]])?;
                    let rd = self.generate_stmt_operand(operands[0].clone())?;
                    machine_code = 9 << 12 | (rd << 8) | -1;
                }
                Mnemonic::Not => {
                    self.check_stmt_operands(
                        span.clone(),
                        &operands,
                        vec![vec!["register"], vec!["register"]],
                    )?;
                    let rd = self.generate_stmt_operand(operands[0].clone())?;
                    let rs = self.generate_stmt_operand(operands[1].clone())?;
                    machine_code = 4 << 12 | (rd << 8) | rs;
                }
            },
            _ => panic!("unreachable"),
        }

        Ok(format!("{:016b}\n", machine_code))
    }

    fn generate_stmt_operand(&mut self, operand: AstStmtOperand) -> Result<i64, Error> {
        match operand {
            AstStmtOperand::Register(r) => match r.0.kind {
                TokenKind::Register(reg) => Ok(reg.number as i64),
                _ => panic!("unreachable"),
            },
            AstStmtOperand::Label(l) => match l.0.kind {
                TokenKind::Label(label) => {
                    let value = self.labels_table.get(&label.name);
                    if value.is_none() {
                        eval_err!(l.0.span.clone(), "undefined label '{}'", label.name);
                    }
                    Ok(*value.unwrap() as i64)
                }
                _ => panic!("unreachable"),
            },
            AstStmtOperand::Int(i) => match i.0.kind {
                TokenKind::Int(int) => Ok(int as i64),
                _ => panic!("unreachable"),
            },
            AstStmtOperand::Identifier(ident) => match ident.0.kind {
                TokenKind::Identifier(i) => {
                    let value = self.symbol_table.get(&i.to_string());
                    if value.is_none() {
                        eval_err!(ident.0.span.clone(), "undefined identifier '{}'", i);
                    }
                    self.check_stmt_operand(
                        ident.0.span.clone(),
                        value.unwrap(),
                        vec!["int", "char"],
                    )?;
                    match value.unwrap() {
                        StmtValue::Int(int) => Ok(*int),
                        StmtValue::Char(c) => Ok(*c as i64),
                    }
                }
                _ => panic!("unreachable"),
            },
            AstStmtOperand::Char(c) => match c.0.kind {
                TokenKind::Char(char) => Ok(char.value as i64),
                _ => panic!("unreachable"),
            },
        }
    }
}
