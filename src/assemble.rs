use std::collections::HashMap;
use std::str::FromStr;

use crate::common::{eval_expression, expression_map_primary};
use crate::lexer::{Char, Label, Token, TokenKind};
use crate::parser::{
    Comparison, Define, Equality, ExportMacro, Expression, Factor, If, Instruction, LogicAnd,
    Mnemonic, Primary, Sequence, Statement, Symbol, Term, Unary, Undefine, __token_ast_Token,
};

use laps::log_error;
use laps::span::{Error, Span, Spanned};

macro_rules! eval_err {
    ($span:expr, $($arg:tt)+) => {
        return Err(log_error!($span, $($arg)+))
    };
}

#[derive(Debug)]
struct Macro {
    pub args: Vec<String>,
    pub body: Vec<Statement>,
    pub quantifier: Quantifier,
}

#[derive(Debug)]
enum Quantifier {
    Plus,
    Star,
    None,
}

impl Macro {
    pub fn new(args: Vec<String>, body: Vec<Statement>, quantifier: Quantifier) -> Self {
        Self {
            args,
            body,
            quantifier,
        }
    }
}

pub struct Assembler {
    files: Vec<Vec<Statement>>,
    globals: Vec<String>,
    global_symbols: HashMap<String, i64>,
    local_symbols: HashMap<String, i64>,
    global_macros: HashMap<String, Macro>,
    local_macros: HashMap<String, Macro>,
    current_macro_defs: Vec<(
        HashMap<String, Expression>,
        Option<(String, Vec<Expression>)>,
    )>,
    next_macro_uuid: u64,
    uuid_stack: Vec<u64>,
}

impl Assembler {
    pub fn new(files: Vec<Vec<Statement>>) -> Self {
        Self {
            files,
            globals: vec![],
            global_symbols: HashMap::new(),
            local_symbols: HashMap::new(),
            global_macros: HashMap::new(),
            local_macros: HashMap::new(),
            current_macro_defs: Vec::new(),
            next_macro_uuid: 0,
            uuid_stack: Vec::new(),
        }
    }

    pub fn generate(mut self) -> Vec<Vec<Statement>> {
        let mut output = Vec::new();
        for file in self.files.clone() {
            let mut new_stmts = Vec::new();
            self.local_symbols = HashMap::new();
            self.local_macros = HashMap::new();
            for stmt in file {
                if let Ok(out) = self.generate_stmt(stmt) {
                    new_stmts.extend(out);
                }
            }
            output.push(new_stmts);
        }
        output
    }

    fn generate_define(&mut self, define: &Define) -> Result<Vec<Statement>, Error> {
        let ident = define.name.get_name().to_lowercase();
        if self.local_symbols.contains_key(&ident)
            || (self.globals.contains(&ident.to_string())
                && self.global_symbols.contains_key(&ident))
        {
            log_error!(define.name.span(), "redefinition of '{}'", ident);
        }
        let value = eval_expression(
            &define.value,
            Some(&self.global_symbols),
            Some(&self.local_symbols),
            None,
            None,
            None,
        )?;
        if self.globals.contains(&ident) {
            &mut self.global_symbols
        } else {
            &mut self.local_symbols
        }
        .insert(ident, value);
        Ok(vec![])
    }

    fn undefine(&mut self, undefine: &Undefine) -> Result<Vec<Statement>, Error> {
        let ident = undefine.name.get_name().to_lowercase();
        let table = if self.globals.contains(&ident) {
            &mut self.global_symbols
        } else {
            &mut self.local_symbols
        };
        if !table.contains_key(&ident) {
            eval_err!(undefine.name.span(), "undefined symbol '{ident}'");
        }
        table.remove(&ident.to_lowercase());
        Ok(vec![])
    }

    fn generate_label(&mut self, label: &__token_ast_Token::Token3) -> Vec<Statement> {
        let mut name = match &label.0.kind {
            TokenKind::Label(l) => l.name.clone(),
            _ => unreachable!(),
        };
        if let Some(uuid) = self.uuid_stack.last() {
            name.insert_str(0, &format!("_{}_", uuid));
        } else {
            name.insert_str(0, "__");
        }
        vec![Statement::Label(__token_ast_Token::Token3(Token::new(
            TokenKind::Label(Label { name }),
            label.span(),
        )))]
    }

    fn generate_macro_call(
        &mut self,
        name_span: Span,
        instruction_span: Span,
        name: &str,
        args: &mut [Expression],
    ) -> Result<Vec<Statement>, Error> {
        let name = name.to_lowercase();
        let m = self
            .local_macros
            .get(&name)
            .or(self.global_macros.get(&name));
        let m = m.ok_or_else(|| log_error!(name_span, "undefined macro or mnemonic '{}'", name))?;
        match m.quantifier {
            Quantifier::Plus => {
                if args.len() < m.args.len() {
                    eval_err!(
                        instruction_span,
                        "macro '{}' expects more than {} arguments, got {}",
                        name,
                        m.args.len() - 1,
                        args.len()
                    );
                }
            }
            Quantifier::Star => {
                if args.len() < m.args.len() - 1 {
                    eval_err!(
                        instruction_span,
                        "macro '{}' expects at least {} arguments, got {}",
                        name,
                        m.args.len() - 1,
                        args.len()
                    );
                }
            }
            Quantifier::None => {
                if args.len() != m.args.len() {
                    eval_err!(
                        instruction_span,
                        "macro '{}' expects {} arguments, got {}",
                        name,
                        m.args.len(),
                        args.len()
                    );
                }
            }
        };

        let macro_body = m.body.clone();

        let mut macro_def = HashMap::new();
        for (arg_name, arg) in m
            .args
            .clone()
            .iter()
            .take(m.args.len().saturating_sub(1))
            .zip(args.iter_mut())
        {
            if macro_def.contains_key(arg_name) {
                log_error!(name_span, "redefinition of argument '{}'", arg_name);
            }
            expression_map_primary(arg, &mut |primary| self.map_primary(primary))?;
            macro_def.insert(arg_name.clone(), arg.clone());
        }
        let rest = match m.quantifier {
            Quantifier::Plus | Quantifier::Star => {
                let values = args[m.args.len() - 1..].to_vec();
                let arg_name = m.args[m.args.len() - 1].clone();
                if macro_def.contains_key(&arg_name) {
                    log_error!(name_span, "redefinition of argument '{}'", arg_name);
                }
                Some((m.args[m.args.len() - 1].clone(), values))
            }
            Quantifier::None => {
                if !m.args.is_empty() {
                    let value = args[args.len() - 1].clone();
                    let arg_name = m.args[m.args.len() - 1].clone();
                    if macro_def.contains_key(&arg_name) {
                        log_error!(name_span, "redefinition of argument '{}'", arg_name);
                    }
                    macro_def.insert(arg_name, value);
                }
                None
            }
        };
        self.uuid_stack.push(self.next_macro_uuid);
        self.next_macro_uuid += 1;
        self.current_macro_defs.push((macro_def, rest));
        let mut output = Vec::new();
        for stmt in macro_body {
            output.extend(self.generate_stmt(stmt)?);
        }
        self.uuid_stack.pop();
        self.current_macro_defs.pop();
        Ok(output)
    }

    fn expand_if<I: Spanned, D: Spanned, U: Spanned>(
        &mut self,
        if_block: If<I, D, U>,
    ) -> Result<Option<Vec<Statement>>, Error> {
        match if_block {
            If::If(ifmac) => {
                let value = eval_expression(
                    &ifmac.expression,
                    Some(&self.global_symbols),
                    Some(&self.local_symbols),
                    None,
                    None,
                    None,
                )?;
                if value != 0 {
                    let mut output = Vec::new();
                    for stmt in ifmac.stmts {
                        if let Ok(out) = self.generate_stmt(stmt) {
                            output.extend(out);
                        }
                    }
                    return Ok(Some(output));
                }
            }
            If::Def(ifdef) => {
                if self
                    .local_symbols
                    .contains_key(&ifdef.identifier.get_name().to_lowercase())
                    || self
                        .global_symbols
                        .contains_key(&ifdef.identifier.get_name().to_lowercase())
                {
                    let mut output = Vec::new();
                    for stmt in ifdef.stmts {
                        if let Ok(out) = self.generate_stmt(stmt) {
                            output.extend(out);
                        }
                    }
                    return Ok(Some(output));
                }
            }
            If::Undef(undef) => {
                if !self
                    .local_symbols
                    .contains_key(&undef.identifier.get_name().to_lowercase())
                    && !self
                        .global_symbols
                        .contains_key(&undef.identifier.get_name().to_lowercase())
                {
                    let mut output = Vec::new();
                    for stmt in undef.stmts {
                        if let Ok(out) = self.generate_stmt(stmt) {
                            output.extend(out);
                        }
                    }
                    return Ok(Some(output));
                }
            }
        }
        Ok(None)
    }

    fn generate_export(&mut self, mut export: ExportMacro) -> Result<Vec<Statement>, Error> {
        if !self.uuid_stack.is_empty() {
            eval_err!(export.span(), "Can't use exports in macro's");
        }
        match export.symbol {
            Symbol::Ident(ident) => {
                self.globals.push(ident.get_name().to_lowercase());
                Ok(vec![])
            }
            Symbol::Label(ref mut lbl) => {
                match &mut lbl.0.kind {
                    TokenKind::Label(l) => l.name = format!("__{}", l.name),
                    _ => unreachable!(),
                }
                Ok(vec![Statement::ExportMacro(export)])
            }
        }
    }

    fn generate_stmt(&mut self, stmt: Statement) -> Result<Vec<Statement>, Error> {
        match stmt {
            Statement::ExportMacro(export) => self.generate_export(export),
            Statement::Define(define) => self.generate_define(&define),
            Statement::Undefine(undefine) => self.undefine(&undefine),
            Statement::Instruction(ast) => self.generate_instruction(ast),
            Statement::Label(lbl) => Ok(self.generate_label(&lbl)),
            Statement::If(if_chain) => {
                if let Some(output) = self.expand_if(if_chain.init_if)? {
                    return Ok(output);
                }
                for elif in if_chain.elifs {
                    if let Some(output) = self.expand_if(elif)? {
                        return Ok(output);
                    }
                }
                match if_chain.else_block {
                    Some(else_block) => {
                        let mut output = Vec::new();
                        for stmt in else_block.stmts {
                            if let Ok(out) = self.generate_stmt(stmt) {
                                output.extend(out);
                            }
                        }
                        Ok(output)
                    }
                    None => Ok(Vec::new()),
                }
            }
            Statement::ForMacro(for_macro) => {
                let value = match &for_macro.expr {
                    Sequence::Str(string) => match &string.0.kind {
                        TokenKind::RawString(string) => string,
                        _ => unreachable!(),
                    }
                        .value
                        .chars()
                        .map(|char| {
                            Ok(Expression {
                                first: LogicAnd {
                                    first: Equality {
                                        first: Comparison {
                                            first: Term {
                                                first: Factor {
                                                    first: Unary {
                                                        ops: vec![],
                                                        primary: Primary::Char(
                                                            __token_ast_Token::Token6(
                                                                Token::new(
                                                                    TokenKind::Char(
                                                                        Char::from_str(&format!("'{char}'"))
                                                                            .map_err(|_| {
                                                                                log_error!(
                                                                                    for_macro.expr.span(), // 21 levels of indentation
                                                                                    "Invalid character in string: '{char}'"
                                                                                )
                                                                            })?,
                                                                    ),
                                                                    for_macro.expr.span()
                                                                )
                                                            )
                                                        ),
                                                    },
                                                    rest: vec![],
                                                },
                                                rest: vec![],
                                            },
                                            rest: vec![],
                                        },
                                        rest: vec![],
                                    },
                                    rest: vec![],
                                },
                                rest: vec![],
                            })
                        })
                        .collect::<Result<Vec<_>, _>>(),
                    Sequence::MacroExpr(expr) => {
                        let expr = match &expr.0.kind {
                            TokenKind::MacroExpression(expr) => expr,
                            _ => unreachable!(),
                        };
                        let (name, values) = self.current_macro_defs
                            [self.current_macro_defs.len() - 1].clone()
                            .1
                            .ok_or_else(|| {
                                log_error!(
                                    for_macro.expr.span(),
                                    "No sequence macro expression found"
                                )
                            })?;
                        if name != expr.name {
                            log_error!(
                                for_macro.expr.span(),
                                "Could not find sequence macro '{}', this macro's sequence macro is called '{}'",
                                expr.name, name
                            );
                        }
                        Ok(values)
                    }
                    Sequence::Multiple(values) => Ok(values.clone()),
                }?;
                self.current_macro_defs.push((HashMap::new(), None));
                let mut output = Vec::new();
                for val in value {
                    self.uuid_stack.push(self.next_macro_uuid);
                    self.next_macro_uuid += 1;
                    let len = self.current_macro_defs.len() - 1;
                    self.current_macro_defs[len]
                        .0
                        .insert(for_macro.ident.get_name().to_owned(), val);
                    for stmt in for_macro.stmts.clone().into_iter() {
                        output.extend(self.generate_stmt(stmt)?);
                    }
                    self.uuid_stack.pop();
                }
                let len = self.current_macro_defs.len() - 1;
                self.current_macro_defs[len]
                    .0
                    .remove(for_macro.ident.get_name());
                self.current_macro_defs.pop();
                Ok(output)
            }
            Statement::IncludeMacro(_) => Ok(Vec::new()), // handle include macros before generating code
            Statement::MacroDefinition(def) => {
                if def.args.is_empty() {
                    eval_err!(
                        def.mac.0.span,
                        "macro must have at least one argument - the name of the macro"
                    );
                }
                let name = def.args[0].get_name().to_lowercase();
                if self.local_macros.contains_key(&name)
                    || (self.globals.contains(&name) && self.global_macros.contains_key(&name))
                {
                    log_error!(def.mac.0.span, "redefinition of macro '{}'", name);
                }
                let args: Vec<String> = def.args[1..]
                    .iter()
                    .map(|arg| arg.get_name().to_owned())
                    .collect();
                if self.globals.contains(&name) {
                    &mut self.global_macros
                } else {
                    &mut self.local_macros
                }
                .insert(
                    name,
                    Macro::new(
                        args,
                        def.body.clone(),
                        match &def.quantifier {
                            Some(quant) => match quant.0.kind {
                                TokenKind::Plus => Quantifier::Plus,
                                TokenKind::Mult => Quantifier::Star,
                                _ => unreachable!(),
                            },
                            None => Quantifier::None,
                        },
                    ),
                );
                Ok(Vec::new())
            }
            Statement::Newline(_) => Ok(Vec::new()),
        }
    }

    fn map_primary(&self, primary: &mut Primary) -> Result<(), Error> {
        match primary {
            Primary::Identifier(ref ident) => {
                *primary = Primary::Int(__token_ast_Token::Token4(Token::new(
                    TokenKind::Int(
                        *self
                            .local_symbols
                            .get(&ident.get_name().to_lowercase())
                            .or(self.global_symbols.get(&ident.get_name().to_lowercase()))
                            .ok_or_else(|| {
                                log_error!(
                                    primary.span(),
                                    "Undefined symbol '{}'",
                                    ident.get_name()
                                )
                            })? as i16,
                    ),
                    primary.span(),
                )));
            }
            Primary::MacroExpression(expr) => {
                let macro_def = &self.current_macro_defs[self.current_macro_defs.len() - 1];
                let name = match &expr.0.kind {
                    TokenKind::MacroExpression(expr) => &expr.name,
                    _ => unreachable!(),
                };
                let value = macro_def.0.get(name).ok_or_else(|| {
                    log_error!(expr.span(), "Undefined macro expression '{name}'")
                })?;
                let value = eval_expression(
                    value,
                    Some(&self.global_symbols),
                    Some(&self.local_symbols),
                    None,
                    None,
                    None,
                )?;
                *primary = Primary::Int(__token_ast_Token::Token4(Token::new(
                    TokenKind::Int(value as i16),
                    primary.span(),
                )));
            }
            Primary::Label(lbl) => {
                let mut name = match &lbl.0.kind {
                    TokenKind::Label(l) => l.name.clone(),
                    _ => unreachable!(),
                };
                if let Some(uuid) = self.uuid_stack.last() {
                    name.insert_str(0, &format!("_{}_", uuid));
                } else {
                    name.insert_str(0, "__");
                }
                match &mut lbl.0.kind {
                    TokenKind::Label(l) => l.name = name,
                    _ => unreachable!(),
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn generate_instruction(
        &mut self,
        mut instruction: Instruction,
    ) -> Result<Vec<Statement>, Error> {
        let span = instruction.span();
        match instruction.mnemonic {
            Mnemonic::Mnemonic(_) => {
                instruction.operands.iter_mut().try_for_each(|operand| {
                    expression_map_primary(operand, &mut |primary| self.map_primary(primary))
                })?;
                Ok(vec![Statement::Instruction(Instruction::new(
                    instruction.mnemonic,
                    instruction.operands,
                ))])
            }
            Mnemonic::MacroCall(name) => self.generate_macro_call(
                name.span(),
                span,
                name.get_name(),
                &mut instruction.operands,
            ),
        }
    }
}
