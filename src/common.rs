use std::{
    collections::HashMap,
    ops::{Add, BitAnd, BitOr, Div, Mul, Sub},
};

use laps::{
    log_error, log_warning,
    span::{Error, Spanned},
};

use crate::{
    lexer::TokenKind,
    parser::{Comparison, Equality, Expression, Factor, LogicAnd, Primary, Term, Unary},
};

macro_rules! eval_binop {
    ($($fn_name: ident, $Ty: ty: $child_fn: ident, $($token_kind: ident, $op: ident),*;)*) => {
        $(
            pub fn $fn_name(
                node: &$Ty,
                symbols: Option<&HashMap<String, i64>>,
                labels: Option<&HashMap<String, u64>>,
            ) -> Result<i64, Error> {
                node.rest.iter().try_fold(
                    $child_fn(&node.first, symbols, labels)?,
                    |acc, (op, operand)|  Ok(match op.0.kind {
                        $(
                            TokenKind::$token_kind => acc.$op(&$child_fn(operand, symbols, labels)?).into(),
                        )*
                        _ => unreachable!(),
                    })
                )
            }
        )*
    };
}

macro_rules! binop_map_primary {
    ($($fn_name: ident, $Ty: ty: $child_fn: ident;)*) => {
        $(
            pub fn $fn_name(node: &mut $Ty, f: &mut impl FnMut(&mut Primary) -> Result<(), Error>) -> Result<(), Error> {
                $child_fn(&mut node.first, f)?;
                for (_, child) in &mut node.rest {
                    $child_fn(child, f)?;
                }
                Ok(())
            }
        )*
    };
}

pub fn eval_expression(
    expression: &Expression,
    symbols: Option<&HashMap<String, i64>>,
    labels: Option<&HashMap<String, u64>>,
    bits: Option<u32>,
) -> Result<i64, Error> {
    let value = expression.rest.iter().try_fold(
        eval_logic_and(&expression.first, symbols, labels)?,
        |acc, (op, operand)| {
            Ok(match op.0.kind {
                TokenKind::Or => acc.bitor(&eval_logic_and(operand, symbols, labels)?),
                _ => unreachable!(),
            })
        },
    )?;
    match bits {
        Some(bits) => {
            if (value.is_negative() && value.leading_ones() <= 64 - bits)
                || (value.is_positive() && value.leading_zeros() < 64 - bits)
            {
                log_warning!(expression.span(), "Value is truncated to {bits} bits");
            }
            Ok(value & (2i128.pow(bits) - 1) as i64)
        }
        None => Ok(value),
    }
}

eval_binop!(
    eval_logic_and, LogicAnd: eval_equality, And, bitand;
    eval_equality, Equality: eval_cmp, Eq, eq, Ne, ne;
    eval_cmp, Comparison: eval_term, Ge, ge, Gt, lt, Le, le, Lt, lt;
    eval_term, Term: eval_factor, Plus, add, Minus, sub;
    eval_factor, Factor: eval_unary, Mult, mul, Div, div;
);

pub fn eval_unary(
    unary: &Unary,
    symbols: Option<&HashMap<String, i64>>,
    labels: Option<&HashMap<String, u64>>,
) -> Result<i64, Error> {
    unary
        .ops
        .iter()
        .rev()
        .try_fold(eval_primary(&unary.primary, symbols, labels)?, |acc, op| {
            Ok(match op.0.kind {
                TokenKind::Not => !acc,
                TokenKind::Plus => acc,
                TokenKind::Minus => -acc,
                _ => unreachable!(),
            })
        })
}

pub fn eval_primary(
    primary: &Primary,
    symbols: Option<&HashMap<String, i64>>,
    labels: Option<&HashMap<String, u64>>,
) -> Result<i64, Error> {
    Ok(match primary {
        Primary::Parenthesized(_, expr, _) => eval_expression(expr, symbols, labels, None)?,
        Primary::Label(l) => {
            let name = match &l.0.kind {
                TokenKind::Label(lbl) => &lbl.name,
                _ => unreachable!(),
            };
            *labels
                .ok_or_else(|| {
                    log_error!(
                        primary.span(),
                        "Expression that are used to expand code aren't allowed to have labels"
                    )
                })?
                .get(name)
                .ok_or_else(|| log_error!(primary.span(), "Undefined symbol '{name}'"))?
                as i64
        }
        Primary::Int(i) => match i.0.kind {
            TokenKind::Int(int) => int as i64,
            _ => unreachable!(),
        },
        Primary::Register(reg) => match reg.0.kind {
            TokenKind::Register(reg) => reg.number as i64,
            _ => unreachable!(),
        },
        Primary::Identifier(ident) => *symbols
            .ok_or_else(|| {
                log_error!(
                    primary.span(),
                    "Invalid object file format, no identifiers are allowed in object file"
                )
            })?
            .get(&ident.get_name().to_lowercase())
            .ok_or_else(|| log_error!(primary.span(), "Undefined symbol '{}'", ident.get_name()))?,
        Primary::Char(char) => match char.0.kind {
            TokenKind::Char(char) => char.value as i64,
            _ => unreachable!(),
        },
        Primary::MacroExpression(expr) => return Err(log_error!(expr.span(), "Invalid primary")),
    })
}

binop_map_primary!(
    expression_map_primary, Expression: logic_and_map_primary;
    logic_and_map_primary, LogicAnd: equality_map_primary;
    equality_map_primary, Equality: cmp_map_primary;
    cmp_map_primary, Comparison: term_map_primary;
    term_map_primary, Term: factor_map_primary;
    factor_map_primary, Factor: unary_map_primary;
);

fn unary_map_primary(
    unary: &mut Unary,
    f: &mut impl FnMut(&mut Primary) -> Result<(), Error>,
) -> Result<(), Error> {
    match &mut unary.primary {
        Primary::Parenthesized(_, expr, _) => expression_map_primary(expr, f),
        _ => f(&mut unary.primary),
    }
}
