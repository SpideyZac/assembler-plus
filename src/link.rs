use std::collections::HashMap;

use crate::{
    common::eval_expression,
    lexer::{Mnemonic, TokenKind},
    parser::{self, Statement},
};

use laps::{
    log_error,
    span::{Error, Spanned},
};

macro_rules! eval_err {
    ($span:expr, $($arg:tt)+) => {
        return Err(log_error!($span, $($arg)+))
    };
}

macro_rules! instructions {
    (
        $self: ident, $mnemonic: expr, $operands: expr, $span: expr =>
        $($name: ident, $op: literal $(: $($bits: expr $(=>$padding: ident)? $(=>$default: literal)?),*)?;)*
    ) => {
        match $mnemonic {
            $(
                Mnemonic::$name => {
                    let args_min = instructions!(@args_min $(: $($bits $(=>$padding)? $(=>$default)?),*)?);
                    let args_max = instructions!(@args_max $(: $($bits $(=>$padding)? $(=>$default)?),*)?);
                    if !(args_min..=args_max).contains(&$operands.len()) {
                        if args_min == args_max {
                            eval_err!($span, "expected {args_min} operands, got {}", $operands.len());
                        } else {
                            eval_err!($span, "expected between {args_min} and {args_max} operands, got {}", $operands.len());
                        }
                    }
                    $(instructions!(
                        @eval_expression $self, $span, $operands, 0, 0 $(, $bits $(=>$padding)? $(=>$default)?)*) |
                    )? ($op << 12)
                }
            )*
        }
    };
    (@args_min $(: $($bits: expr $(=>$padding: ident)? $(=>$default: literal)?),*)?) => {
        0 $(+ instructions!(@args_min $($bits $(=>$padding)? $(=>$default)?),*))?
    };
    (@args_min $pad: expr =>$padding: ident $(, $bits: expr $(=>$rest: ident)? $(=>$default: literal)?),*) => {
        instructions!(@args_min $($bits $(=>$rest)? $(=>$default)?),*)
    };
    (@args_min $pad: expr =>$default: literal $(, $bits: expr $(=>$rest: ident)? $(=>$def: literal)?),*) => {
        instructions!(@args_min $($bits $(=>$rest)? $(=>$def)?),*)
    };
    (@args_min $bits: expr $(, $rest: expr $(=>$padding: ident)? $(=>$def: literal)?)*) => {
        1 + instructions!(@args_min $($rest $(=>$padding)? $(=>$def)?),*)
    };
    (@args_min) => {0};
    (@args_max $(: $($bits: expr $(=>$padding: ident)? $(=>$default: literal)?),*)?) => {
        0 $(+ instructions!(@args_max $($bits $(=>$padding)? $(=>$default)?),*))?
    };
    (@args_max $pad: expr =>$padding: ident $(, $bits: expr $(=>$rest: ident)? $(=>$default: literal)?),*) => {
        instructions!(@args_max $($bits $(=>$rest)? $(=>$default)?),*)
    };
    (@args_max $bits: expr $(=> $default: literal)? $(, $rest: expr $(=>$padding: ident)? $(=>$def: literal)?)*) => {
        1 + instructions!(@args_max $($rest $(=>$padding)? $(=>$def)?),*)
    };
    (@args_max) => {0};
    (
        @eval_expression
        $self: ident,
        $span: expr,
        $operands: expr,
        $acc: expr,
        $shift: expr,
        $bits: expr
        $(, $rest: expr $(=>$padding: ident)? $(=>$default: literal)?)*
    ) => {
        (eval_expression(&$operands[$acc], None, Some(&$self.labels_table), Some($bits))? << (12 - $shift - $bits)) |
        instructions!(@eval_expression $self, $span, $operands, $acc + 1, $shift + $bits $(, $rest $(=>$padding)? $(=>$default)?)*)
    };
    (
        @eval_expression
        $self: ident,
        $span: expr,
        $operands: expr,
        $acc: expr,
        $shift: expr,
        $bits: expr => $default: literal
        $(, $rest: expr $(=>$padding: ident)? $(=>$def: literal)?)*
    ) => {
        ($operands
            .get($acc)
            .map_or(Ok($default), |operand| eval_expression(&operand, None, Some(&$self.labels_table), Some($bits)))?
        << (12 - $shift - $bits)) |
        instructions!(@eval_expression $self, $span, $operands, $acc + 1, $shift + $bits $(, $rest $(=>$padding)? $(=>$def)?)*)
    };
    (
        @eval_expression
        $self: ident,
        $span: expr,
        $operands: expr,
        $acc: expr,
        $shift: expr,
        $bits: expr => $pad: ident
        $(, $rest: expr $(=>$padding: ident)? $(=>$default: literal)?)*
    ) => {
        instructions!(@eval_expression $self, $span, $operands, $acc, $shift + $bits $(, $rest $(=>$padding)? $(=>$default)?)*)
    };
    (@eval_expression $self: ident, $span: expr, $operands: expr, $acc: expr, $shift: expr) => {0}
}

pub struct Linker {
    statements: Vec<Statement>,
    labels_table: HashMap<String, u64>,
    instruction_pointer: u64,
}

impl Linker {
    pub fn new(statements: Vec<Statement>) -> Self {
        Self {
            statements,
            labels_table: HashMap::new(),
            instruction_pointer: 0,
        }
    }

    pub fn link(&mut self) -> Result<String, Error> {
        let mut output = String::new();
        for statement in &self.statements {
            match statement {
                Statement::Instruction(instr) => {
                    let operands = &instr.operands;
                    let span = instr.span();
                    match &instr.mnemonic {
                        parser::Mnemonic::Mnemonic(mnemonic) => {
                            const REG: u32 = 4;
                            const IMM: u32 = 8;
                            const ADDR: u32 = 10;
                            const COND: u32 = 2;
                            const OFFSET: u32 = 4;

                            let mnemonic = match mnemonic.0.kind {
                                TokenKind::Mnemonic(mnemonic) => mnemonic,
                                _ => unreachable!(),
                            };

                            let machine_code = instructions!(self, mnemonic, operands, span =>
                                Nop, 0;
                                Hlt, 1;
                                Add, 2: REG, REG, REG;
                                Sub, 3: REG, REG, REG;
                                Nor, 4: REG, REG, REG;
                                And, 5: REG, REG, REG;
                                Xor, 6: REG, REG, REG;
                                Rsh, 7: REG, 4=>pad, REG;
                                Ldi, 8: REG, IMM;
                                Adi, 9: REG, IMM;
                                Jmp,10: 2=>pad, ADDR;
                                Brh,11: COND, ADDR;
                                Cal,12: 2=>pad, ADDR;
                                Ret,13;
                                Lod,14: REG, REG, OFFSET=>0;
                                Str,15: REG, REG, OFFSET=>0;
                            );
                            output += &format!("{:016b}\n", machine_code);
                            self.instruction_pointer += 1;
                        }
                        _ => eval_err!(instr.span(), "Invalid instruction"),
                    }
                }
                Statement::Label(lbl) => {
                    self.labels_table.insert(
                        match &lbl.0.kind {
                            TokenKind::Label(label) => label.name.clone(),
                            _ => unreachable!(),
                        },
                        self.instruction_pointer,
                    );
                }
                _ => eval_err!(statement.span(), "Invalid statement"),
            }
        }
        Ok(output)
    }
}
