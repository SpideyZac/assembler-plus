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
        [rawstr] => { kind: TokenKind::RawString(_), prompt: "string literal" },

        [mac] => { kind: TokenKind::Macro, prompt: "macro" },
        [endmac] => { kind: TokenKind::EndMacro, prompt: "endmacro" },
        [includemac] => { kind: TokenKind::IncludeMacro, prompt: "include macro" },
        [maccall] => { kind: TokenKind::MacroCall(_), prompt: "macro call" },
        [macexpr] => { kind: TokenKind::MacroExpression(_), prompt: "macro expression" },

        [eof] => { kind: TokenKind::Eof, prompt: "end of file" },
    }
}

#[derive(Parse, Debug, Clone)]
#[token(Token)]
pub enum Statement {
    AstStmt(AstStmt),
    Label(Token![label]),
    MacroCall(MacroCall),
    MacroDefinition(MacroDefinition),
    IncludeMacro(IncludeMacro),
    #[allow(dead_code)]
    Newline(Token![nl]),
}

#[derive(Parse, Debug, Clone)]
#[token(Token)]
pub struct IncludeMacro {
    _includemac: Token![includemac],
    pub path: Token![rawstr],
    _nl: Token![nl],
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
    MacroExpression(Token![macexpr]),
}

#[derive(Parse, Debug, Clone)]
#[token(Token)]
pub struct MacroCall {
    pub name: Token![maccall],
    pub args: Vec<AstStmtOperand>,
    _nl: Token![nl],
}

#[derive(Parse, Debug, Clone)]
#[token(Token)]
pub struct MacroDefinition {
    pub mac: Token![mac],
    pub args: Vec<Token![ident]>,
    _nl2: Token![nl],
    pub body: Vec<Statement>,
    _end: Token![endmac],
    _nl: Token![nl],
}
