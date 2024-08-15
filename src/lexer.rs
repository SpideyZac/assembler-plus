use core::fmt;
use std::str::FromStr;

use laps::lexer::signed_int_literal;
use laps::prelude::*;

fn str_lit(s: &str) -> Option<RawString> {
    let mut buf = String::new();
    let mut escape = false;
    let mut hex_num = 0;
    let mut hex = 0;
    for c in s[1..s.len() - 1].chars() {
        if escape {
            if hex_num > 0 && c.is_ascii_digit() {
                hex = hex * 16 + c.to_digit(16)?;
                hex_num -= 1;
                if hex_num == 0 {
                    buf.push(char::from_u32(hex)?);
                    hex = 0;
                    escape = false;
                }
            } else if c == 'u' {
                hex_num = 4;
            } else {
                match c {
                    '"' => buf.push('"'),
                    '\\' => buf.push('\\'),
                    '/' => buf.push('/'),
                    'b' => buf.push('\x08'),
                    'f' => buf.push('\x0c'),
                    'n' => buf.push('\n'),
                    'r' => buf.push('\r'),
                    't' => buf.push('\t'),
                    _ => return None,
                }
                escape = false;
            }
        } else {
            match c {
                '\\' => escape = true,
                c => buf.push(c),
            }
        }
    }
    Some(RawString { value: buf })
}

#[token_kind]
#[derive(Debug, Tokenize)]
pub enum TokenKind {
    #[skip(r"[ \t\r]+|;.*|#.*|/.*")] // We will track newlines separately
    _Skip,
    #[regex(r"\n")]
    Newline,
    
    #[regex(r"\+")]
    Plus,
    #[regex(r"-")]
    Minus,
    #[regex(r"\*")]
    Mult,
    #[regex(r"/")]
    Div,
    #[regex(r"!")]
    Not,
    #[regex(r"&")]
    And,
    #[regex(r"\|")]
    Or,
    #[regex(r"==")]
    Eq,
    #[regex(r"!=")]
    Ne,
    #[regex(r">=")]
    Ge,
    #[regex(r">")]
    Gt,
    #[regex(r"<=")]
    Le,
    #[regex(r"<")]
    Lt,
    #[regex(r"\(")]
    LeftParen,
    #[regex(r"\)")]
    RightParen,

    #[regex(r"(?i)define")]
    Define,

    #[regex(r"(?i)nop|hlt|add|sub|nor|and|xor|rsh|ldi|adi|jmp|brh|cal|ret|lod|str|cmp|mov|lsh|inc|dec|not")]
    Mnemonic(Mnemonic),
    #[regex(r"(?i)r\d+")]
    Register(Register),
    #[regex(r"\.\S+")]
    Label(Label),
    #[regex(r"(?i)-?[0-9]|-?[1-9][0-9]+|0b[01]+|0x[0-9a-f]+|0o[0-7]+", signed_int_literal)]
    Int(i16),
    #[regex(r#"'.'|".""#)]
    Char(Char),
    #[regex(r#""([^\x00-\x1f"\\]|\\(["\\/bfnrt]|u[0-9a-fA-F]{4}))*""#, str_lit)]
    RawString(RawString),

    #[regex(r"%macro")]
    Macro,
    #[regex(r"%endmacro")]
    EndMacro,
    #[regex(r"%include")]
    IncludeMacro,
    #[regex(r"%ifdef")]
    IfDefMacro,
    #[regex(r"%if")]
    IfMacro,
    #[regex(r"%endif")]
    EndIfMacro,
    #[regex(r"%[a-zA-Z_~|]\S*")]
    MacroCall(MacroCall),
    #[regex(r"\$[a-zA-Z_~|]\S*")]
    MacroExpression(MacroExpression),

    #[regex(r"\S+")]
    Identifier(String),

    #[eof]
    Eof,
}

pub type Token = laps::token::Token<TokenKind>;

#[derive(Clone, Debug, PartialEq)]
pub struct RawString {
    pub value: String,
}

impl FromStr for RawString {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(RawString {
            value: s[1..s.len() - 1].to_string(),
        })
    }
}

impl fmt::Display for RawString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\"{}\"", self.value)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MacroCall {
    pub name: String,
}

impl FromStr for MacroCall {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(MacroCall {
            name: s[1..].to_string(),
        })
    }
}

impl fmt::Display for MacroCall {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MacroExpression {
    pub name: String,
}

impl FromStr for MacroExpression {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(MacroExpression {
            name: s[1..].to_string(),
        })
    }
}

impl fmt::Display for MacroExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

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
    Cmp,
    Mov,
    Lsh,
    Inc,
    Dec,
    Not,
}

impl FromStr for Mnemonic {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
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
            "cmp" => Ok(Mnemonic::Cmp),
            "mov" => Ok(Mnemonic::Mov),
            "lsh" => Ok(Mnemonic::Lsh),
            "inc" => Ok(Mnemonic::Inc),
            "dec" => Ok(Mnemonic::Dec),
            "not" => Ok(Mnemonic::Not),
            _ => Err(()),
        }
    }
}

impl fmt::Display for Mnemonic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
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
            Mnemonic::Cmp => write!(f, "cmp"),
            Mnemonic::Mov => write!(f, "mov"),
            Mnemonic::Lsh => write!(f, "lsh"),
            Mnemonic::Inc => write!(f, "inc"),
            Mnemonic::Dec => write!(f, "dec"),
            Mnemonic::Not => write!(f, "not"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Char {
    pub value: u8,
}

impl FromStr for Char {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let all_chars = [
            ' ', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p',
            'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '.', '!', '?',
        ];
        let value = s.to_lowercase().chars().nth(1).unwrap();
        let index = all_chars.iter().position(|&c| c == value).ok_or(())?;
        Ok(Char { value: index as u8 })
    }
}

impl fmt::Display for Char {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'{}'", self.value)
    }
}
