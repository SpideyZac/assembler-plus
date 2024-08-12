use crate::lexer::{Token, TokenKind};

use laps::prelude::*;

token_ast! {
    #[derive(Debug, PartialEq, Clone)]
    pub macro Token<TokenKind> {
        [nl] => { kind: TokenKind::Newline, prompt: "new line" },
        [mnemonic] => { kind: TokenKind::Mnemonic(_), prompt: "mnemonic" },
        [register] => { kind: TokenKind::Register(_), prompt: "register" },
        [label] => { kind: TokenKind::Label(_), prompt: "label" },
        [int] => { kind: TokenKind::Int(_), prompt: "integer literal" },
        [ident] => { kind: TokenKind::Identifier(_), prompt: "identifier" },
        [chr] => { kind: TokenKind::Char(_), prompt: "character literal" },
        [eof] => { kind: TokenKind::Eof, prompt: "end of file" },
    }
}

#[derive(Parse, Debug, Clone)]
#[token(Token)]
pub enum Statement {
    AstStmt(AstStmt),
    Label(Token![label]),
    Eof(Token![eof]),
    Newline(Token![nl]),
}

#[derive(Parse, Debug, Clone)]
#[token(Token)]
pub struct AstStmt {
    pub mnemonic: Token![mnemonic],
    pub operands: Vec<AstStmtOperand>,
    _nl: Token![nl],
}

#[derive(Parse, Debug, Clone, PartialEq)]
#[token(Token)]
pub enum AstStmtOperand {
    Register(Token![register]),
    Label(Token![label]),
    Int(Token![int]),
    Identifier(Token![ident]),
    Char(Token![chr]),
}
