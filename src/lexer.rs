use core::fmt;
use std::str::FromStr;

use laps::lexer::int_literal;
use laps::prelude::*;

#[token_kind]
#[derive(Debug, Tokenize)]
pub enum TokenKind {
    #[skip(r"[ \t\r]+|;.*")] // We will track newlines separately
    _Skip,
    #[regex(r"\n")]
    Newline,

    #[regex(r"define|nop|hlt|add|sub|nor|and|xor|rsh|ldi|adi|jmp|brh|cal|ret|lod|str")]
    Mnemonic(Mnemonic),
    #[regex(r"r\d+")]
    Register(Register),
    #[regex(r"\.[a-zA-Z_][a-zA-Z0-9_]*")]
    Label(Label),
    #[regex(r"[0-9]|[1-9][0-9]+|0x[0-9a-fA-F]+", int_literal)]
    Int(u8),
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier(String),

    #[eof]
    Eof,
}

pub type Token = laps::token::Token<TokenKind>;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Register {
    pub number: u8,
}

impl FromStr for Register {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let number = s[1..].parse().map_err(|_| ())?;
        if number > 15 {
            return Err(());
        }

        Ok(Register { number })
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "r{}", self.number)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Label {
    pub name: String,
}

impl FromStr for Label {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Label {
            name: s[1..].to_string(),
        })
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Mnemonic {
    Define,
    Nop,
    Hlt,
    Add,
    Sub,
    Nor,
    And,
    Xor,
    Rsh,
    Ldi,
    Adi,
    Jmp,
    Brh,
    Cal,
    Ret,
    Lod,
    Str,
}

impl FromStr for Mnemonic {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "define" => Ok(Mnemonic::Define),
            "nop" => Ok(Mnemonic::Nop),
            "hlt" => Ok(Mnemonic::Hlt),
            "add" => Ok(Mnemonic::Add),
            "sub" => Ok(Mnemonic::Sub),
            "nor" => Ok(Mnemonic::Nor),
            "and" => Ok(Mnemonic::And),
            "xor" => Ok(Mnemonic::Xor),
            "rsh" => Ok(Mnemonic::Rsh),
            "ldi" => Ok(Mnemonic::Ldi),
            "adi" => Ok(Mnemonic::Adi),
            "jmp" => Ok(Mnemonic::Jmp),
            "brh" => Ok(Mnemonic::Brh),
            "cal" => Ok(Mnemonic::Cal),
            "ret" => Ok(Mnemonic::Ret),
            "lod" => Ok(Mnemonic::Lod),
            "str" => Ok(Mnemonic::Str),
            _ => Err(()),
        }
    }
}

impl fmt::Display for Mnemonic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Mnemonic::Define => write!(f, "define"),
            Mnemonic::Nop => write!(f, "nop"),
            Mnemonic::Hlt => write!(f, "hlt"),
            Mnemonic::Add => write!(f, "add"),
            Mnemonic::Sub => write!(f, "sub"),
            Mnemonic::Nor => write!(f, "nor"),
            Mnemonic::And => write!(f, "and"),
            Mnemonic::Xor => write!(f, "xor"),
            Mnemonic::Rsh => write!(f, "rsh"),
            Mnemonic::Ldi => write!(f, "ldi"),
            Mnemonic::Adi => write!(f, "adi"),
            Mnemonic::Jmp => write!(f, "jmp"),
            Mnemonic::Brh => write!(f, "brh"),
            Mnemonic::Cal => write!(f, "cal"),
            Mnemonic::Ret => write!(f, "ret"),
            Mnemonic::Lod => write!(f, "lod"),
            Mnemonic::Str => write!(f, "str"),
        }
    }
}
