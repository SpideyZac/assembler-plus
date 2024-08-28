use std::collections::HashMap;

use crate::{
    common::eval_expression,
    lexer::{Label, Mnemonic, TokenKind},
    parser::{self, ExportMacro, Instruction, Statement, Symbol, __token_ast_Token},
};

use laps::{
    log_error,
    span::{Error, Spanned},
    token::Token,
};

macro_rules! eval_err {
    ($span:expr, $($arg:tt)+) => {
        return Err(log_error!($span, $($arg)+))
    };
}

macro_rules! instructions {
    (
        $globals: ident, $locals: ident, $mnemonic: expr, $operands: expr, $span: expr =>
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
                        @eval_expression $globals, $locals, $span, $operands, 0, 0 $(, $bits $(=>$padding)? $(=>$default)?)*) |
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
        $globals: ident, $locals: ident,
        $span: expr,
        $operands: expr,
        $acc: expr,
        $shift: expr,
        $bits: expr
        $(, $rest: expr $(=>$padding: ident)? $(=>$default: literal)?)*
    ) => {
        (eval_expression(&$operands[$acc], None, None, Some(&$globals), Some(&$locals), Some($bits))? << (12 - $shift - $bits)) |
        instructions!(@eval_expression $globals, $locals, $span, $operands, $acc + 1, $shift + $bits $(, $rest $(=>$padding)? $(=>$default)?)*)
    };
    (
        @eval_expression
        $globals: ident, $locals: ident,
        $span: expr,
        $operands: expr,
        $acc: expr,
        $shift: expr,
        $bits: expr => $default: literal
        $(, $rest: expr $(=>$padding: ident)? $(=>$def: literal)?)*
    ) => {
        ($operands
            .get($acc)
            .map_or(Ok($default), |operand| eval_expression(&operand, None, None, Some(&$globals), Some(&$locals), Some($bits)))?
        << (12 - $shift - $bits)) |
        instructions!(@eval_expression $globals, $locals, $span, $operands, $acc + 1, $shift + $bits $(, $rest $(=>$padding)? $(=>$def)?)*)
    };
    (
        @eval_expression
        $globals: ident, $locals: ident,
        $span: expr,
        $operands: expr,
        $acc: expr,
        $shift: expr,
        $bits: expr => $pad: ident
        $(, $rest: expr $(=>$padding: ident)? $(=>$default: literal)?)*
    ) => {
        instructions!(@eval_expression $globals, $locals, $span, $operands, $acc, $shift + $bits $(, $rest $(=>$padding)? $(=>$default)?)*)
    };
    (@eval_expression $globals: ident, $locals: ident, $span: expr, $operands: expr, $acc: expr, $shift: expr) => {0}
}

fn map_global_labels(files: &[&[Statement]]) -> Result<HashMap<String, u64>, Error> {
    let mut globals = vec![];
    for file in files {
        for stmt in file.iter() {
            match stmt {
                Statement::ExportMacro(ExportMacro {
                    symbol:
                        Symbol::Label(__token_ast_Token::Token3(Token {
                            kind: TokenKind::Label(Label { name }),
                            ..
                        })),
                    ..
                }) => globals.push(name.to_lowercase()),
                Statement::Label(_) => {}
                Statement::Instruction(Instruction {
                    mnemonic: parser::Mnemonic::Mnemonic(_),
                    ..
                }) => {}
                _ => unreachable!(),
            }
        }
    }
    let mut instruction_pointer = 0;
    let mut globals_map = HashMap::new();
    for file in files {
        for stmt in file.iter() {
            match stmt {
                Statement::Label(lbl) => {
                    let name = match &lbl.0.kind {
                        TokenKind::Label(lbl) => &lbl.name,
                        _ => unreachable!(),
                    };
                    if !globals.contains(&name.to_lowercase()) {
                        continue;
                    }
                    if globals_map.contains_key(&name.to_lowercase()) {
                        eval_err!(lbl.span(), "Dubplicate global label '{}'", name);
                    }
                    globals_map.insert(name.to_lowercase(), instruction_pointer);
                }
                Statement::Instruction(_) => instruction_pointer += 1,
                _ => {}
            }
        }
    }
    Ok(globals_map)
}

fn map_local_labels(
    stmts: &[Statement],
    mut instruction_pointer: u64,
) -> Result<HashMap<String, u64>, Error> {
    let mut locals_map = HashMap::new();
    for stmt in stmts {
        match stmt {
            Statement::Label(lbl) => {
                let name = match &lbl.0.kind {
                    TokenKind::Label(lbl) => &lbl.name,
                    _ => unreachable!(),
                };
                if locals_map.contains_key(name) {
                    eval_err!(lbl.span(), "Duplicate label '{}'", name);
                }
                locals_map.insert(name.to_lowercase(), instruction_pointer);
            }
            Statement::Instruction(_) => instruction_pointer += 1,
            _ => {}
        }
    }
    Ok(locals_map)
}

pub fn link(files: &[&[Statement]]) -> Result<String, Error> {
    let globals = map_global_labels(files)?;
    let mut output = String::new();
    let mut instruction_pointer = 0;
    for file in files {
        let locals = map_local_labels(file, instruction_pointer)?;
        for stmt in file.iter() {
            if let Statement::Instruction(instr) = stmt {
                let operands = &instr.operands;
                let span = instr.span();
                if let parser::Mnemonic::Mnemonic(mnemonic) = &instr.mnemonic {
                    const REG: u32 = 4;
                    const IMM: u32 = 8;
                    const ADDR: u32 = 10;
                    const COND: u32 = 2;
                    const OFFSET: u32 = 4;

                    let mnemonic = match mnemonic.0.kind {
                        TokenKind::Mnemonic(mnemonic) => mnemonic,
                        _ => unreachable!(),
                    };
                    let machine_code = instructions!(globals, locals, mnemonic, operands, span =>
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
                    instruction_pointer += 1;
                }
            }
        }
    }
    Ok(output)
}
