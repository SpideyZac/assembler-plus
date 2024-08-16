use std::collections::HashMap;

use crate::lexer::{Mnemonic, TokenKind};
use crate::parser::{
    self, Comparison, Define, Equality, Expression, Factor, Instruction, LogicAnd, Primary,
    Statement, Term, Unary, __token_ast_Token,
};

use laps::span::{Error, Span, Spanned};
use laps::{log_error, log_warning};

macro_rules! eval_err {
    ($span:expr, $($arg:tt)+) => {
        return Err(log_error!($span, $($arg)+))
    };
}

struct Macro {
    pub args: Vec<String>,
    pub body: Vec<Statement>,
}

impl Macro {
    pub fn new(args: Vec<String>, body: Vec<Statement>) -> Self {
        Self { args, body }
    }
}

macro_rules! eval_binop {
    ($($fn_name: ident, $Ty: ty: $child_fn: ident, $($token_kind: ident, $op: tt),*;)*) => {
        $(
            fn $fn_name(&mut self, node: &$Ty) -> Result<i64, Error> {
                node.rest.iter().try_fold(
                    self.$child_fn(&node.first)?,
                    |acc, (op, operand)|  Ok(match op.0.kind {
                        $(
                            TokenKind::$token_kind => acc $op self.$child_fn(operand)?,
                        )*
                        _ => unreachable!(),
                    } as i64)
                )
            }
        )*
    };
}

pub struct Codegen {
    statements: Vec<Statement>,
    symbol_table: HashMap<String, i64>,
    labels_table: HashMap<String, u64>,
    instruction_pointer: u64,
    macros: HashMap<String, Macro>,
    current_macro_defs: Vec<HashMap<String, i64>>,
    next_macro_uuid: u64,
    uuid_stack: Vec<u64>,
}

impl Codegen {
    pub fn new(statements: Vec<Statement>) -> Self {
        let mut res = Self {
            statements,
            symbol_table: HashMap::new(),
            labels_table: HashMap::new(),
            instruction_pointer: 0,
            macros: HashMap::new(),
            current_macro_defs: Vec::new(),
            next_macro_uuid: 0,
            uuid_stack: Vec::new(),
        };

        res.symbol_table.insert("eq".to_string(), 0);
        res.symbol_table.insert("ne".to_string(), 1);
        res.symbol_table.insert("ge".to_string(), 2);
        res.symbol_table.insert("lt".to_string(), 3);
        res.symbol_table.insert("z".to_string(), 0);
        res.symbol_table.insert("nz".to_string(), 1);
        res.symbol_table.insert("c".to_string(), 2);
        res.symbol_table.insert("nc".to_string(), 3);
        res.symbol_table.insert("zero".to_string(), 0);
        res.symbol_table.insert("notzero".to_string(), 1);
        res.symbol_table.insert("carry".to_string(), 2);
        res.symbol_table.insert("notcarry".to_string(), 3);

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
            res.symbol_table.insert(port.to_string(), i as i64 + 240);
        }

        res
    }

    pub fn generate(&mut self) -> Result<String, Error> {
        let mut output = String::new();
        for stmt in self.statements.clone() {
            self.generate_preprocess(&stmt)?;
        }
        self.instruction_pointer = 0;
        self.next_macro_uuid = 0;
        self.uuid_stack.clear();
        for stmt in self.statements.clone() {
            let out = self.generate_stmt(&stmt)?;
            output.push_str(&out);
        }
        Ok(output)
    }

    fn generate_preprocess(&mut self, stmt: &Statement) -> Result<(), Error> {
        match stmt {
            Statement::Instruction(Instruction {
                mnemonic: parser::Mnemonic::MacroCall(name),
                ..
            }) => {
                let m = self.macros.get(name.get_name());
                let m = m.ok_or_else(|| {
                    log_error!(name.span(), "undefined macro '{}'", name.get_name())
                })?;
                let mut len = 0;
                for stmt in m.body.iter() {
                    if let Statement::Instruction(_) = stmt {
                        len += 1
                    }
                }

                self.instruction_pointer += len;
                Ok(())
            }
            Statement::Instruction(Instruction {
                mnemonic: parser::Mnemonic::Mnemonic(_),
                ..
            }) => {
                self.instruction_pointer += 1;
                Ok(())
            }
            Statement::Define(define) => self.generate_define_preprocess(define),
            Statement::Label(label) => self.generate_label_preprocess(label),
            Statement::MacroDefinition(def) => {
                if def.args.is_empty() {
                    eval_err!(
                        def.mac.0.span,
                        "macro must have at least one argument - the name of the macro"
                    );
                }
                let name = def.args[0].get_name();
                if self.macros.contains_key(name) {
                    eval_err!(def.mac.0.span, "redefinition of macro '{}'", name);
                }
                let args: Vec<String> = def.args[1..]
                    .iter()
                    .map(|arg| arg.get_name().to_owned())
                    .collect();
                self.macros
                    .insert(name.to_owned(), Macro::new(args, def.body.clone()));
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn generate_define_preprocess(&mut self, define: &Define) -> Result<(), Error> {
        let ident = define.name.get_name();
        let value = self.eval_expression(&define.value, 64)?;
        if self.symbol_table.contains_key(&ident.to_lowercase()) {
            eval_err!(define.name.span(), "redefinition of '{}'", ident);
        }
        self.symbol_table.insert(ident.to_lowercase(), value);
        Ok(())
    }

    fn eval_expression(&mut self, expression: &Expression, bits: u32) -> Result<i64, Error> {
        let value = Ok(expression.rest.iter().try_fold(
            self.eval_logic_and(&expression.first)?,
            |acc, (_op, operand)| Ok(acc | self.eval_logic_and(operand)?),
        )?)?;
        if (value.is_negative() && value.leading_ones() <= 64 - bits)
            || (value.is_positive() && value.leading_zeros() < 64 - bits)
        {
            log_warning!(expression.span(), "Value is truncated to {bits} bits");
        }
        Ok(value & (2i128.pow(bits) - 1) as i64)
    }

    eval_binop!(
        eval_logic_and, LogicAnd: eval_equality, And, &;
        eval_equality, Equality: eval_cmp, Eq, ==, Ne, !=;
        eval_cmp, Comparison: eval_term, Ge, >=, Gt, >, Le, <=, Lt, <;
        eval_term, Term: eval_factor, Plus, +, Minus, -;
        eval_factor, Factor: eval_unary, Mult, *, Div, /;
    );

    fn eval_unary(&mut self, unary: &Unary) -> Result<i64, Error> {
        unary
            .ops
            .iter()
            .rev()
            .try_fold(self.eval_primary(&unary.primary)?, |acc, op| {
                Ok(match op.0.kind {
                    TokenKind::Not => !acc,
                    TokenKind::Plus => acc,
                    TokenKind::Minus => -acc,
                    _ => unreachable!(),
                })
            })
    }

    fn eval_primary(&mut self, primary: &Primary) -> Result<i64, Error> {
        Ok(match primary {
            Primary::Parenthesized(_, expr, _) => self.eval_expression(expr, 64)?,
            Primary::Register(r) => match r.0.kind {
                TokenKind::Register(reg) => reg.number as i64,
                _ => unreachable!(),
            },
            Primary::Label(l) => match &l.0.kind {
                TokenKind::Label(label) => {
                    let mut name = label.name.clone();
                    if let Some(uuid) = self.uuid_stack.last() {
                        name.insert_str(0, &format!("_{}_", uuid));
                    } else {
                        name.insert_str(0, "__");
                    }
                    let value = self.labels_table.get(&name);
                    *value
                        .or_else(|| self.labels_table.get(&format!("__{}", &label.name)))
                        .ok_or_else(|| log_error!(l.0.span, "undefined label '{}'", label.name))?
                        as i64
                }
                _ => unreachable!(),
            },
            Primary::Int(i) => match i.0.kind {
                TokenKind::Int(int) => int as i64,
                _ => unreachable!(),
            },
            Primary::Identifier(ident) => {
                let value = self.symbol_table.get(&ident.get_name().to_lowercase());
                *value.ok_or_else(|| {
                    log_error!(ident.span(), "undefined identifier '{}'", ident.get_name())
                })?
            }
            Primary::Char(c) => match c.0.kind {
                TokenKind::Char(char) => char.value as i64,
                _ => unreachable!(),
            },
            Primary::MacroExpression(macroexpr) => {
                let name = match &macroexpr.0.kind {
                    TokenKind::MacroExpression(m) => &m.name,
                    _ => unreachable!(),
                };
                let mut value;
                for macro_def in self.current_macro_defs.iter().rev() {
                    value = macro_def.get(name);
                    if let Some(value) = value {
                        return Ok(*value);
                    }
                }
                eval_err!(macroexpr.0.span, "undefined macro expression '{}'", name);
            }
        })
    }

    fn generate_label_preprocess(
        &mut self,
        label: &__token_ast_Token::Token3,
    ) -> Result<(), Error> {
        let mut name = match &label.0.kind {
            TokenKind::Label(l) => l.name.clone(),
            _ => unreachable!(),
        };
        if let Some(uuid) = self.uuid_stack.last() {
            name.insert_str(0, &format!("_{}_", uuid));
        } else {
            name.insert_str(0, "__");
        }
        if self.labels_table.contains_key(&name) {
            eval_err!(label.0.span, "redefinition of label '{}'", name);
        }
        self.labels_table.insert(name, self.instruction_pointer);
        Ok(())
    }

    fn generate_macro_call(
        &mut self,
        name_span: Span,
        instruction_span: Span,
        name: &str,
        args: &[Expression],
    ) -> Result<String, Error> {
        let m = self.macros.get(name);
        let m = m.ok_or_else(|| log_error!(name_span, "undefined macro '{}'", name))?;
        if m.args.len() != args.len() {
            eval_err!(
                instruction_span,
                "macro '{}' expects {} arguments, got {}",
                name,
                m.args.len(),
                args.len()
            );
        }

        let macro_body = m.body.clone();

        let mut macro_def = HashMap::new();
        for (i, arg) in m.args.clone().iter().enumerate() {
            let value = self.eval_expression(&args[i], 64)?;
            if macro_def.contains_key(arg) {
                eval_err!(name_span, "redefinition of argument '{}'", arg);
            }
            macro_def.insert(arg.clone(), value);
        }
        self.uuid_stack.push(self.next_macro_uuid);
        self.next_macro_uuid += 1;
        self.current_macro_defs.push(macro_def);
        let mut output = String::new();
        for stmt in macro_body {
            let out = self.generate_stmt(&stmt)?;
            output.push_str(&out);
        }
        self.uuid_stack.pop();
        self.current_macro_defs.pop();
        Ok(output)
    }

    fn generate_stmt(&mut self, stmt: &Statement) -> Result<String, Error> {
        match stmt {
            Statement::Define(_) => Ok(String::new()),
            Statement::Instruction(ast) => self.generate_ast_stmt(ast),
            Statement::Label(lbl) => {
                if !self.uuid_stack.is_empty() {
                    self.generate_label_preprocess(lbl)?;
                }
                Ok(String::new())
            }
            Statement::IfDefMacro(ifdef) => {
                if self
                    .symbol_table
                    .contains_key(&ifdef.identifier.get_name().to_lowercase())
                {
                    let mut output = String::new();
                    for stmt in &ifdef.stmts {
                        let out = self.generate_stmt(stmt)?;
                        output += &out;
                    }
                    Ok(output)
                } else {
                    Ok(String::new())
                }
            }
            Statement::IfMacro(ifmac) => {
                let value = self.eval_expression(&ifmac.expression, 64)?;
                if value != 0 {
                    let mut output = String::new();
                    for stmt in ifmac.stmts.iter() {
                        let out = self.generate_stmt(stmt)?;
                        output += &out;
                    }
                    Ok(output)
                } else {
                    Ok(String::new())
                }
            }
            Statement::IncludeMacro(_) => Ok(String::new()), // handle include macros before generating code
            Statement::MacroDefinition(_) => Ok(String::new()), // handle macro definitions before generating code
            Statement::Newline(_) => Ok(String::new()),
        }
    }

    fn generate_ast_stmt(&mut self, ast: &Instruction) -> Result<String, Error> {
        let operands = &ast.operands;
        let span = &ast.span();
        match &ast.mnemonic {
            parser::Mnemonic::MacroCall(macro_call) => self.generate_macro_call(
                macro_call.span(),
                ast.span(),
                macro_call.get_name(),
                operands,
            ),
            parser::Mnemonic::Mnemonic(mnemonic) => {
                let mnemonic = match mnemonic.0.kind {
                    TokenKind::Mnemonic(mnemonic) => mnemonic,
                    _ => unreachable!(),
                };
                let machine_code = match mnemonic {
                    Mnemonic::Nop => {
                        if !operands.is_empty() {
                            eval_err!(span, "expected 0 operands, got {}", operands.len());
                        }
                        0
                    }
                    Mnemonic::Hlt => {
                        if !operands.is_empty() {
                            eval_err!(span, "expected 0 operands, got {}", operands.len());
                        }
                        1 << 12
                    }
                    Mnemonic::Add => {
                        if operands.len() != 3 {
                            eval_err!(span, "expected 3 operands, got {}", operands.len());
                        }
                        let rd = self.eval_expression(&operands[0], 4)?;
                        let rs = self.eval_expression(&operands[1], 4)?;
                        let rt = self.eval_expression(&operands[2], 4)?;
                        2 << 12 | (rd << 8) | (rs << 4) | rt
                    }
                    Mnemonic::Sub => {
                        if operands.len() != 3 {
                            eval_err!(span, "expected 3 operands, got {}", operands.len());
                        }
                        let rd = self.eval_expression(&operands[0], 4)?;
                        let rs = self.eval_expression(&operands[1], 4)?;
                        let rt = self.eval_expression(&operands[2], 4)?;
                        3 << 12 | (rd << 8) | (rs << 4) | rt
                    }
                    Mnemonic::Nor => {
                        if operands.len() != 3 {
                            eval_err!(span, "expected 3 operands, got {}", operands.len());
                        }
                        let rd = self.eval_expression(&operands[0], 4)?;
                        let rs = self.eval_expression(&operands[1], 4)?;
                        let rt = self.eval_expression(&operands[2], 4)?;
                        4 << 12 | (rd << 8) | (rs << 4) | rt
                    }
                    Mnemonic::And => {
                        if operands.len() != 3 {
                            eval_err!(span, "expected 3 operands, got {}", operands.len());
                        }
                        let rd = self.eval_expression(&operands[0], 4)?;
                        let rs = self.eval_expression(&operands[1], 4)?;
                        let rt = self.eval_expression(&operands[2], 4)?;
                        5 << 12 | (rd << 8) | (rs << 4) | rt
                    }
                    Mnemonic::Xor => {
                        if operands.len() != 3 {
                            eval_err!(span, "expected 3 operands, got {}", operands.len());
                        }
                        let rd = self.eval_expression(&operands[0], 4)?;
                        let rs = self.eval_expression(&operands[1], 4)?;
                        let rt = self.eval_expression(&operands[2], 4)?;
                        6 << 12 | (rd << 8) | (rs << 4) | rt
                    }
                    Mnemonic::Rsh => {
                        if operands.len() != 2 {
                            eval_err!(span, "expected 2 operands, got {}", operands.len());
                        }
                        let rd = self.eval_expression(&operands[0], 4)?;
                        let rs = self.eval_expression(&operands[1], 4)?;
                        7 << 12 | (rd << 8) | rs
                    }
                    Mnemonic::Ldi => {
                        if operands.len() != 2 {
                            eval_err!(span, "expected 2 operands, got {}", operands.len());
                        }
                        let rd = self.eval_expression(&operands[0], 4)?;
                        let imm = self.eval_expression(&operands[1], 8)?;
                        if !(-128..=255).contains(&imm) {
                            eval_err!(span, "immediate value {} out of range [-128, 255]", imm);
                        }
                        8 << 12 | (rd << 8) | imm
                    }
                    Mnemonic::Adi => {
                        if operands.len() != 2 {
                            eval_err!(span, "expected 2 operands, got {}", operands.len());
                        }
                        let rd = self.eval_expression(&operands[0], 4)?;
                        let imm = self.eval_expression(&operands[1], 8)?;
                        if !(-128..=255).contains(&imm) {
                            eval_err!(span, "immediate value {} out of range [-128, 255]", imm);
                        }
                        9 << 12 | (rd << 8) | imm
                    }
                    Mnemonic::Jmp => {
                        if operands.len() != 1 {
                            eval_err!(span, "expected 1 operands, got {}", operands.len());
                        }
                        let addr = self.eval_expression(&operands[0], 10)?;
                        if addr != (addr % 1024) {
                            eval_err!(span, "jump address {} out of range [0, 1023]", addr);
                        }
                        10 << 12 | addr
                    }
                    Mnemonic::Brh => {
                        if operands.len() != 2 {
                            eval_err!(span, "expected 2 operands, got {}", operands.len());
                        }
                        let cond = self.eval_expression(&operands[0], 2)?;
                        let addr = self.eval_expression(&operands[1], 10)?;
                        if addr != (addr % 1024) {
                            eval_err!(span, "jump address {} out of range [0, 1023]", addr);
                        }
                        11 << 12 | (cond << 10) | addr
                    }
                    Mnemonic::Cal => {
                        if operands.len() != 1 {
                            eval_err!(span, "expected 1 operands, got {}", operands.len());
                        }
                        let addr = self.eval_expression(&operands[0], 10)?;
                        12 << 12 | addr
                    }
                    Mnemonic::Ret => {
                        if !operands.is_empty() {
                            eval_err!(span, "expected 0 operands, got {}", operands.len());
                        }
                        13 << 12
                    }
                    Mnemonic::Lod => {
                        if !(2..=3).contains(&operands.len()) {
                            eval_err!(span, "expected 2 or 3 operands, got {}", operands.len());
                        }
                        let rd = self.eval_expression(&operands[0], 4)?;
                        let rs = self.eval_expression(&operands[1], 4)?;
                        let offset = if operands.len() == 3 {
                            self.eval_expression(&operands[2], 4)?
                        } else {
                            0
                        };
                        14 << 12 | (rd << 8) | (rs << 4) | offset
                    }
                    Mnemonic::Str => {
                        if !(2..=3).contains(&operands.len()) {
                            eval_err!(span, "expected 2 or 3 operands, got {}", operands.len());
                        }
                        let rd = self.eval_expression(&operands[0], 4)?;
                        let rs = self.eval_expression(&operands[1], 4)?;
                        let offset = if operands.len() == 3 {
                            self.eval_expression(&operands[2], 4)?
                        } else {
                            0
                        };
                        let mut masked = offset & 0b1111;
                        if masked & 0b1000 != 0 {
                            masked -= 16;
                        }
                        if !(-8..=7).contains(&masked) {
                            eval_err!(span, "offset value {} out of range [-8, 7]", masked);
                        }
                        15 << 12 | (rd << 8) | (rs << 4) | offset
                    }
                    Mnemonic::Cmp => {
                        if operands.len() != 2 {
                            eval_err!(span, "expected 2 operands, got {}", operands.len());
                        }
                        let rs = self.eval_expression(&operands[0], 4)?;
                        let rt = self.eval_expression(&operands[1], 4)?;
                        3 << 12 | (rs << 8) | (rt << 4)
                    }
                    Mnemonic::Mov => {
                        if operands.len() != 2 {
                            eval_err!(span, "expected 2 operands, got {}", operands.len());
                        }
                        let rd = self.eval_expression(&operands[0], 4)?;
                        let rs = self.eval_expression(&operands[1], 4)?;
                        2 << 12 | (rd << 8) | rs
                    }
                    Mnemonic::Lsh => {
                        if operands.len() != 2 {
                            eval_err!(span, "expected 2 operands, got {}", operands.len());
                        }
                        let rd = self.eval_expression(&operands[0], 4)?;
                        let rs = self.eval_expression(&operands[1], 4)?;
                        2 << 12 | (rd << 8) | (rd << 4) | rs
                    }
                    Mnemonic::Inc => {
                        if operands.len() != 1 {
                            eval_err!(span, "expected 1 operands, got {}", operands.len());
                        }
                        let rd = self.eval_expression(&operands[0], 4)?;
                        9 << 12 | (rd << 8) | 1
                    }
                    Mnemonic::Dec => {
                        if operands.len() != 1 {
                            eval_err!(span, "expected 1 operands, got {}", operands.len());
                        }
                        let rd = self.eval_expression(&operands[0], 4)?;
                        9 << 12 | (rd << 8) | 0xff
                    }
                    Mnemonic::Not => {
                        if operands.len() != 2 {
                            eval_err!(span, "expected 2 operands, got {}", operands.len());
                        }
                        let rd = self.eval_expression(&operands[0], 4)?;
                        let rs = self.eval_expression(&operands[1], 4)?;
                        4 << 12 | (rd << 8) | rs
                    }
                };
                Ok(format!("{:016b}\n", machine_code))
            }
        }
    }
}
