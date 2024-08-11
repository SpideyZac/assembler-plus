use laps::prelude::*;

#[token_kind]
#[derive(Debug, Tokenize)]
pub enum TokenKind {
    #[skip(r"[ \t\r]+")] // We will track newlines separately
    _Skip,

    #[regex(r"\n")]
    Newline,

    #[eof]
    Eof,
}

pub type Token = laps::token::Token<TokenKind>;
