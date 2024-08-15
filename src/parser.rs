use crate::lexer::{Token, TokenKind};

use laps::{prelude::*, span::TrySpan};

token_ast! {
    #[derive(Debug, PartialEq, Clone)]
    pub macro Token<TokenKind> {
        [nl] => { kind: TokenKind::Newline | TokenKind::Eof, prompt: "new line" },
        [mnemonic] => { kind: TokenKind::Mnemonic(_), prompt: "mnemonic" },
        [register] => { kind: TokenKind::Register(_), prompt: "register" },
        [label] => { kind: TokenKind::Label(_), prompt: "label" },
        [int] => { kind: TokenKind::Int(_), prompt: "integer literal" },
        [ident] => { kind: TokenKind::Identifier(_), prompt: "identifier" },
        [chr] => { kind: TokenKind::Char(_), prompt: "character literal" },
        [rawstr] => { kind: TokenKind::RawString(_), prompt: "string literal" },

        [define] => { kind: TokenKind::Define, prompt: "define" },

        [+-] => { kind: TokenKind::Plus | TokenKind::Minus, prompt: "Plus" },
        [*/] => { kind: TokenKind::Mult | TokenKind::Div, prompt: "Minus" },
        [unary] => { kind: TokenKind::Not | TokenKind::Plus | TokenKind::Minus, prompt: "Not" },
        [&] => { kind: TokenKind::And, prompt: "And" },
        [|] => { kind: TokenKind::Or, prompt: "Or" },
        [equality] => { kind: TokenKind::Eq | TokenKind::Ne, prompt: "comparison" },
        [cmp] => { kind: TokenKind::Ge | TokenKind::Gt | TokenKind::Le | TokenKind::Lt, prompt: "Ge" },
        [lparen] => { kind: TokenKind::LeftParen, prompt: "left paren" },
        [rparen] => { kind: TokenKind::RightParen, prompt: "right paren" },

        [mac] => { kind: TokenKind::Macro, prompt: "macro" },
        [endmac] => { kind: TokenKind::EndMacro, prompt: "endmacro" },
        [includemac] => { kind: TokenKind::IncludeMacro, prompt: "include macro" },
        [ifdefmacro] => { kind: TokenKind::IfDefMacro, prompt: "ifdef macro" },
        [endif] => { kind: TokenKind::EndIfMacro, prompt: "endif macro" },
        [maccall] => { kind: TokenKind::MacroCall(_), prompt: "macro call" },
        [macexpr] => { kind: TokenKind::MacroExpression(_), prompt: "macro expression" },

        [eof] => { kind: TokenKind::Eof, prompt: "end of file" },
    }
}

#[derive(Parse, Debug, Clone)]
#[token(Token)]
pub enum Statement {
    AstStmt(AstStmt),
    Define(Define),
    Label(Token![label]),
    MacroCall(MacroCall),
    MacroDefinition(MacroDefinition),
    IncludeMacro(IncludeMacro),
    IfDefMacro(IfDefMacro),
    #[allow(dead_code)]
    Newline(Token![nl]),
}

#[derive(Parse, Debug, Clone, PartialEq)]
#[token(Token)]
pub struct Define {
    _define: Token![define],
    pub name: Token![ident],
    pub value: Expression,
}

#[derive(Parse, Debug, Clone)]
#[token(Token)]
pub struct IfDefMacro {
    _ifdef_macro: Token![ifdefmacro],
    pub identifier: Token![ident],
    _nl: Token![nl],
    pub stmts: Vec<Statement>,
    _end_if: Token![endif],
    _nl2: Token![nl],
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
    pub operands: Vec<Expression>,
    _nl: Token![nl],
}

#[derive(Parse, Debug, Clone, PartialEq)]
#[token(Token)]
pub struct Expression {
    pub first: LogicAnd,
    pub rest: Vec<(Token![|], LogicAnd)>,
}

impl Spanned for Expression {
    fn span(&self) -> laps::span::Span {
        let mut span = self.first.span();
        if let Some(rest_span) = self.rest.try_span() {
            span.update_end(rest_span);
        }
        span
    }
}

#[derive(Parse, Debug, Clone, PartialEq)]
#[token(Token)]
pub struct LogicAnd {
    pub first: Equality,
    pub rest: Vec<(Token![&], Equality)>,
}

impl Spanned for LogicAnd {
    fn span(&self) -> laps::span::Span {
        let mut span = self.first.span();
        if let Some(rest_span) = self.rest.try_span() {
            span.update_end(rest_span);
        }
        span
    }
}

#[derive(Parse, Debug, Clone, PartialEq)]
#[token(Token)]
pub struct Equality {
    pub first: Comparison,
    pub rest: Vec<(Token![equality], Comparison)>,
}

impl Spanned for Equality {
    fn span(&self) -> laps::span::Span {
        let mut span = self.first.span();
        if let Some(rest_span) = self.rest.try_span() {
            span.update_end(rest_span);
        }
        span
    }
}

#[derive(Parse, Debug, Clone, PartialEq)]
#[token(Token)]
pub struct Comparison {
    pub first: Term,
    pub rest: Vec<(Token![cmp], Term)>,
}

impl Spanned for Comparison {
    fn span(&self) -> laps::span::Span {
        let mut span = self.first.span();
        if let Some(rest_span) = self.rest.try_span() {
            span.update_end(rest_span);
        }
        span
    }
}

#[derive(Parse, Debug, Clone, PartialEq)]
#[token(Token)]
pub struct Term {
    pub first: Factor,
    pub rest: Vec<(Token![+-], Factor)>,
}

impl Spanned for Term {
    fn span(&self) -> laps::span::Span {
        let mut span = self.first.span();
        if let Some(rest_span) = self.rest.try_span() {
            span.update_end(rest_span);
        }
        span
    }
}

#[derive(Parse, Debug, Clone, PartialEq)]
#[token(Token)]
pub struct Factor {
    pub first: Unary,
    pub rest: Vec<(Token![*/], Unary)>,
}

impl Spanned for Factor {
    fn span(&self) -> laps::span::Span {
        let mut span = self.first.span();
        if let Some(rest_span) = self.rest.try_span() {
            span.update_end(rest_span);
        }
        span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub ops: Vec<Token![unary]>,
    pub primary: Primary,
}

impl<TS: TokenStream<Token = Token>> Parse<TS> for Unary {
    fn parse(tokens: &mut TS) -> laps::span::Result<Self> {
        Ok(Self {
            ops: tokens.parse()?,
            primary: tokens.parse()?,
        })
    }

    fn maybe(tokens: &mut TS) -> laps::span::Result<bool> {
        Ok(<Token![unary]>::maybe(tokens)? || Primary::maybe(tokens)?)
    }
}

impl Spanned for Unary {
    fn span(&self) -> laps::span::Span {
        self.ops.try_span().map_or_else(
            || self.primary.span(),
            |mut span| {
                span.update_end(self.primary.span());
                span
            },
        )
    }
}

#[derive(Parse, Debug, Clone, PartialEq)]
#[token(Token)]
pub enum Primary {
    Register(Token![register]),
    Label(Token![label]),
    Int(Token![int]),
    Identifier(Token![ident]),
    Char(Token![chr]),
    MacroExpression(Token![macexpr]),
    Parenthesized(Token![lparen], Box<Expression>, Token![rparen]),
}

impl Spanned for Primary {
    fn span(&self) -> laps::span::Span {
        match self {
            Primary::Register(reg) => reg.span(),
            Primary::Label(lbl) => lbl.span(),
            Primary::Int(int) => int.span(),
            Primary::Identifier(ident) => ident.span(),
            Primary::Char(char) => char.span(),
            Primary::MacroExpression(macro_expr) => macro_expr.span(),
            Primary::Parenthesized(left, _, right) => {
                let mut span = left.span();
                span.update_end(right.span());
                span
            }
        }
    }
}

#[derive(Parse, Debug, Clone)]
#[token(Token)]
pub struct MacroCall {
    pub name: Token![maccall],
    pub args: Vec<Expression>,
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
