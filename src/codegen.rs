use std::collections::HashMap;
use std::str::FromStr;

use crate::lexer::{Char, Mnemonic, TokenKind};
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

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
enum Value {
    Int(i64),
    String(String),
}

macro_rules! try_value {
    ($($fn_name: ident, $msg: expr, $op: tt;)*) => {
        $(
            fn $fn_name(&self, other: &Self, span: Span) -> Result<Self, Error> {
                match (self, other) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a $op b)),
                    (Value::Int(_), Value::String(_)) => {
                        eval_err!(span, "Can't {} string with int", $msg)
                    }
                    (Value::String(_), Value::Int(_)) => {
                        eval_err!(span, "Can't {} int with string", $msg)
                    }
                    (Value::String(_), Value::String(_)) => eval_err!(span, "Can't {} string with string", $msg),
                }
            }
        )*
    };
}

impl Value {
    fn try_add(&self, other: &Self, span: Span) -> Result<Self, Error> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
            (Value::Int(_), Value::String(_)) => {
                eval_err!(span, "Can't add string to int")
            }
            (Value::String(_), Value::Int(_)) => {
                eval_err!(span, "Can't add int to string")
            }
            (Value::String(a), Value::String(b)) => Ok(Value::String(a.to_owned() + b)),
        }
    }

    try_value!(
        try_sub, "subtract", -;
        try_mul, "multiply", *;
        try_div, "divide", /;
        try_or, "or", |;
        try_and, "and", &;
    );
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Int(value as i64)
    }
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
    ($fn_name: ident, $Ty: ty: $child_fn: ident, $($token_kind: ident, try $op: ident),*; $($tt:tt)*) => {
        fn $fn_name(&self, node: &$Ty) -> Result<Value, Error> {
            node.rest.iter().try_fold(
                self.$child_fn(&node.first)?,
                |acc, (op, operand)|  Ok(match op.0.kind {
                    $(
                        TokenKind::$token_kind => acc.$op(&self.$child_fn(operand)?, node.span())?.into(),
                    )*
                    _ => unreachable!(),
                })
            )
        }
        eval_binop!($($tt)*);
    };
    ($fn_name: ident, $Ty: ty: $child_fn: ident, $($token_kind: ident, $op: ident),*; $($tt:tt)*) => {
        fn $fn_name(&self, node: &$Ty) -> Result<Value, Error> {
            node.rest.iter().try_fold(
                self.$child_fn(&node.first)?,
                |acc, (op, operand)|  Ok(match op.0.kind {
                    $(
                        TokenKind::$token_kind => acc.$op(&self.$child_fn(operand)?).into(),
                    )*
                    _ => unreachable!(),
                })
            )
        }
        eval_binop!($($tt)*);
    };
    () => {}
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
        (match $self.eval_expression(&$operands[$acc], $bits)? {
            Value::Int(int) => int,
            Value::String(_) => eval_err!($span, "expected an integer value, but got a string"),
        } << (12 - $shift - $bits)) |
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
        ($operands.get($acc).map_or(Ok($default), |operand| match $self.eval_expression(&operand, $bits)? {
            Value::Int(int) => Ok(int),
            Value::String(_) => eval_err!($span, "expected an integer value, but got a string")
        })? << (12 - $shift - $bits)) |
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

pub struct Codegen {
    statements: Vec<Statement>,
    symbol_table: HashMap<String, Value>,
    labels_table: HashMap<String, u64>,
    instruction_pointer: u64,
    macros: HashMap<String, Macro>,
    current_macro_defs: Vec<HashMap<String, Value>>,
    next_macro_uuid: u64,
    uuid_stack: Vec<u64>,
}

impl Codegen {
    pub fn new(statements: Vec<Statement>) -> Self {
        Self {
            statements,
            symbol_table: HashMap::new(),
            labels_table: HashMap::new(),
            instruction_pointer: 0,
            macros: HashMap::new(),
            current_macro_defs: Vec::new(),
            next_macro_uuid: 0,
            uuid_stack: Vec::new(),
        }
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
                let len = self.count_macro_call(name.span(), name.get_name())?;
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
                if self.macros.contains_key(&name.to_lowercase()) {
                    eval_err!(def.mac.0.span, "redefinition of macro '{}'", name);
                }
                let args: Vec<String> = def.args[1..]
                    .iter()
                    .map(|arg| arg.get_name().to_owned())
                    .collect();
                self.macros
                    .insert(name.to_lowercase(), Macro::new(args, def.body.clone()));
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

    fn eval_expression(&self, expression: &Expression, bits: u32) -> Result<Value, Error> {
        let value = expression.rest.iter().try_fold(
            self.eval_logic_and(&expression.first)?,
            |acc, (op, operand)| {
                Ok(match op.0.kind {
                    TokenKind::Or => {
                        acc.try_or(&self.eval_logic_and(operand)?, expression.span())?
                    }
                    _ => unreachable!(),
                })
            },
        )?;
        match value {
            Value::Int(value) => {
                if (value.is_negative() && value.leading_ones() <= 64 - bits)
                    || (value.is_positive() && value.leading_zeros() < 64 - bits)
                {
                    log_warning!(expression.span(), "Value is truncated to {bits} bits");
                }
                Ok(Value::Int(value & (2i128.pow(bits) - 1) as i64))
            }
            Value::String(string) => Ok(Value::String(string)),
        }
    }

    eval_binop!(
        eval_logic_and, LogicAnd: eval_equality, And, try try_and;
        eval_equality, Equality: eval_cmp, Eq, eq, Ne, ne;
        eval_cmp, Comparison: eval_term, Ge, ge, Gt, lt, Le, le, Lt, lt;
        eval_term, Term: eval_factor, Plus, try try_add, Minus, try try_sub;
        eval_factor, Factor: eval_unary, Mult, try try_mul, Div, try try_div;
    );

    fn eval_unary(&self, unary: &Unary) -> Result<Value, Error> {
        unary
            .ops
            .iter()
            .rev()
            .try_fold(self.eval_primary(&unary.primary)?, |acc, op| {
                let acc = match acc {
                    Value::Int(int) => int,
                    Value::String(_) => {
                        eval_err!(unary.span(), "Can't apply unary operator to string")
                    }
                };
                Ok(Value::Int(match op.0.kind {
                    TokenKind::Not => !acc,
                    TokenKind::Plus => acc,
                    TokenKind::Minus => -acc,
                    _ => unreachable!(),
                }))
            })
    }

    fn eval_primary(&self, primary: &Primary) -> Result<Value, Error> {
        Ok(match primary {
            Primary::String(string) => Value::String(match &string.0.kind {
                TokenKind::RawString(string) => string.value.clone(),
                _ => unreachable!(),
            }),
            Primary::Parenthesized(_, expr, _) => self.eval_expression(expr, 64)?,
            Primary::Register(r) => match r.0.kind {
                TokenKind::Register(reg) => Value::Int(reg.number as i64),
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
                    Value::Int(
                        *value
                            .or_else(|| self.labels_table.get(&format!("__{}", &label.name)))
                            .ok_or_else(|| {
                                log_error!(l.0.span, "undefined label '{}'", label.name)
                            })? as i64,
                    )
                }
                _ => unreachable!(),
            },
            Primary::Int(i) => match i.0.kind {
                TokenKind::Int(int) => Value::Int(int as i64),
                _ => unreachable!(),
            },
            Primary::Identifier(ident) => {
                let value = self.symbol_table.get(&ident.get_name().to_lowercase());
                value
                    .ok_or_else(|| {
                        log_error!(ident.span(), "undefined identifier '{}'", ident.get_name())
                    })?
                    .clone()
            }
            Primary::Char(c) => match c.0.kind {
                TokenKind::Char(char) => Value::Int(char.value as i64),
                _ => unreachable!(),
            },
            Primary::MacroExpression(macroexpr) => {
                let name = match &macroexpr.0.kind {
                    TokenKind::MacroExpression(m) => &m.name,
                    _ => unreachable!(),
                };
                for macro_def in self.current_macro_defs.iter().rev() {
                    let value = macro_def.get(name);
                    if let Some(value) = value {
                        return Ok(value.clone());
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

    fn statement_len(&self, stmt: &Statement) -> Result<u64, Error> {
        match stmt {
            Statement::Instruction(Instruction {
                mnemonic: parser::Mnemonic::MacroCall(name),
                ..
            }) => self.count_macro_call(name.span(), name.get_name()),
            Statement::Instruction(Instruction {
                mnemonic: parser::Mnemonic::Mnemonic(_),
                ..
            }) => Ok(1),
            Statement::Define(_) => Ok(0),
            Statement::Label(_) => Ok(0),
            Statement::MacroDefinition(_) => Ok(0),
            Statement::IncludeMacro(_) => Ok(0),
            Statement::IfDefMacro(ifdef) => {
                if self.symbol_table.contains_key(ifdef.identifier.get_name()) {
                    ifdef
                        .stmts
                        .iter()
                        .try_fold(0, |acc, stmt| Ok(acc + self.statement_len(stmt)?))
                } else {
                    Ok(0)
                }
            }
            Statement::IfMacro(if_macro) => {
                let value = self.eval_expression(&if_macro.expression, 64)?;
                if value != Value::Int(0) && value != Value::String(String::new()) {
                    if_macro
                        .stmts
                        .iter()
                        .try_fold(0, |acc, stmt| Ok(acc + self.statement_len(stmt)?))
                } else {
                    Ok(0)
                }
            }
            Statement::ForMacro(for_macro) => {
                Ok(match self.eval_expression(&for_macro.expr, 64)? {
                    Value::Int(_) => eval_err!(
                        for_macro.expr.span(),
                        "Invalid value for for loop, expected string, found int"
                    ),
                    Value::String(string) => string.len(),
                } as u64
                    * for_macro
                        .stmts
                        .iter()
                        .try_fold(0, |acc, stmt| Ok(acc + self.statement_len(stmt)?))?)
            }
            Statement::Newline(_) => Ok(0),
        }
    }

    fn count_macro_call(&self, name_span: Span, name: &str) -> Result<u64, Error> {
        let m = self.macros.get(&name.to_lowercase());
        let m = m.ok_or_else(|| log_error!(name_span, "undefined macro or mnemonic '{}'", name))?;

        let mut len = 0;
        for stmt in m.body.iter() {
            len += self.statement_len(stmt)?;
        }
        Ok(len)
    }

    fn generate_macro_call(
        &mut self,
        name_span: Span,
        instruction_span: Span,
        name: &str,
        args: &[Expression],
    ) -> Result<String, Error> {
        let m = self.macros.get(&name.to_lowercase());
        let m = m.ok_or_else(|| log_error!(name_span, "undefined macro or mnemonic '{}'", name))?;
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
                if value != Value::Int(0) && value != Value::String(String::new()) {
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
            Statement::ForMacro(for_macro) => {
                let value = match self.eval_expression(&for_macro.expr, 64)? {
                    Value::Int(_) => eval_err!(
                        for_macro.expr.span(),
                        "Invalid value for for loop, expected string, found int"
                    ),
                    Value::String(string) => string,
                };
                self.current_macro_defs.push(HashMap::new());
                if self.current_macro_defs[self.current_macro_defs.len() - 1]
                    .contains_key(for_macro.ident.get_name())
                {
                    eval_err!(
                        for_macro.ident.span(),
                        "redefinition of argument '{}'",
                        for_macro.ident.get_name()
                    );
                }
                let mut output = String::new();
                for char in value.chars() {
                    self.uuid_stack.push(self.next_macro_uuid);
                    self.next_macro_uuid += 1;
                    let len = self.current_macro_defs.len() - 1;
                    self.current_macro_defs[len].insert(
                        for_macro.ident.get_name().to_owned(),
                        Value::Int(
                            Char::from_str(&format!("'{char}'"))
                                .map_err(|_| {
                                    log_error!(
                                        for_macro.expr.span(),
                                        "Invalid character in string: '{char}'"
                                    )
                                })?
                                .value as i64,
                        ),
                    );
                    for stmt in &for_macro.stmts {
                        let out = self.generate_stmt(stmt)?;
                        output += &out;
                    }
                    self.uuid_stack.pop();
                }
                let len = self.current_macro_defs.len() - 1;
                self.current_macro_defs[len].remove(for_macro.ident.get_name());
                self.current_macro_defs.pop();
                Ok(output)
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
                Ok(format!("{:016b}\n", machine_code))
            }
        }
    }
}
