use crate::lexer::{Token, TokenKind};

use derivative::Derivative;
use laps::{
    log_warning,
    prelude::*,
    span::{FileType, Span, TrySpan},
};

token_ast! {
    #[derive(Debug, PartialEq, Clone)]
    pub macro Token<TokenKind> {
        [nl] => { kind: TokenKind::Newline | TokenKind::Eof, prompt: "new line" },
        [mnemonic] => { kind: TokenKind::Mnemonic(_), prompt: "mnemonic" },
        [register] => { kind: TokenKind::Register(_), prompt: "register" },
        [label] => { kind: TokenKind::Label(_), prompt: "label" },
        [int] => { kind: TokenKind::Int(_), prompt: "integer literal" },
        [ident] => {
            kind:
                TokenKind::Identifier(_)
                | TokenKind::Plus
                | TokenKind::Mult
                | TokenKind::Div
                | TokenKind::Not
                | TokenKind::And
                | TokenKind::Or
                | TokenKind::Eq
                | TokenKind::Ne
                | TokenKind::Ge
                | TokenKind::Gt
                | TokenKind::Le
                | TokenKind::Lt
                | TokenKind::In,
            prompt: "identifier"
        },
        [chr] => { kind: TokenKind::Char(_), prompt: "character literal" },
        [rawstr] => { kind: TokenKind::RawString(_), prompt: "string literal" },

        [define] => { kind: TokenKind::Define, prompt: "define" },

        [+*] => { kind: TokenKind::Plus | TokenKind::Mult, prompt: "plus or star" },
        [sequence] => { kind: TokenKind::RawString(_) | TokenKind::MacroExpression(_), prompt: "sequence" },

        [+-] => { kind: TokenKind::Plus | TokenKind::Minus, prompt: "plus or minus" },
        [*/] => { kind: TokenKind::Mult | TokenKind::Div, prompt: "multiply or divide" },
        [unary] => { kind: TokenKind::Not | TokenKind::Plus | TokenKind::Minus, prompt: "unary" },
        [&] => { kind: TokenKind::And, prompt: "And" },
        [|] => { kind: TokenKind::Or, prompt: "Or" },
        [equality] => { kind: TokenKind::Eq | TokenKind::Ne, prompt: "equality" },
        [cmp] => { kind: TokenKind::Ge | TokenKind::Gt | TokenKind::Le | TokenKind::Lt, prompt: "comparison" },
        [lparen] => { kind: TokenKind::LeftParen, prompt: "left paren" },
        [rparen] => { kind: TokenKind::RightParen, prompt: "right paren" },

        [mac] => { kind: TokenKind::Macro, prompt: "macro" },
        [endmac] => { kind: TokenKind::EndMacro, prompt: "endmacro" },
        [includemac] => { kind: TokenKind::IncludeMacro, prompt: "include macro" },
        [ifmacro] => { kind: TokenKind::IfMacro, prompt: "if macro" },
        [ifdefmacro] => { kind: TokenKind::IfDefMacro, prompt: "ifdef macro" },
        [elifmacro] => { kind: TokenKind::ElifMacro, prompt: "elif macro" },
        [elifdefmacro] => { kind: TokenKind::ElifDefMacro, prompt: "elifdef macro" },
        [else] => { kind: TokenKind::ElseMacro, prompt: "else macro" },
        [endif] => { kind: TokenKind::EndIfMacro, prompt: "endif macro" },
        [formacro] => { kind: TokenKind::ForMacro, prompt: "for macro" },
        [in] => { kind: TokenKind::In, prompt: "in" },
        [endfor] => { kind: TokenKind::EndForMacro, prompt: "end for" },
        [macexpr] => { kind: TokenKind::MacroExpression(_), prompt: "macro expression" },

        [eof] => { kind: TokenKind::Eof, prompt: "end of file" },
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    identifier: Token![ident],
}

impl Ident {
    pub fn get_name(&self) -> &str {
        match &self.identifier.0.kind {
            TokenKind::Identifier(ident) => ident,
            TokenKind::Plus => "+",
            TokenKind::Mult => "*",
            TokenKind::Div => "/",
            TokenKind::Not => "~",
            TokenKind::And => "&",
            TokenKind::Or => "|",
            TokenKind::Eq => "==",
            TokenKind::Ne => "!=",
            TokenKind::Ge => ">=",
            TokenKind::Gt => ">",
            TokenKind::Le => "<=",
            TokenKind::Lt => "<",
            TokenKind::In => "in",
            _ => unreachable!(),
        }
    }
}

impl<TS: TokenStream<Token = Token>> Parse<TS> for Ident {
    fn parse(tokens: &mut TS) -> laps::span::Result<Self> {
        let res = Self {
            identifier: <Token![ident]>::parse(tokens)?,
        };
        if !matches!(res.identifier.0.kind, TokenKind::Identifier(_))
            && !["!=", ">=", "<"].contains(&res.get_name())
        // Allow symbols used in std library
        {
            log_warning!(
                res.span(),
                "Identifier uses weird symbols, weird symbols are not fully supported and only for backwards \
                compatibility, expect unexpected results"
            );
        }
        Ok(res)
    }

    fn maybe(tokens: &mut TS) -> laps::span::Result<bool> {
        <Token![ident]>::maybe(tokens)
    }
}

impl Spanned for Ident {
    fn span(&self) -> Span {
        self.identifier.span()
    }
}

#[derive(Parse, Debug, Clone)]
#[token(Token)]
pub enum Statement {
    Instruction(Instruction),
    Define(Define),
    Label(Token![label]),
    MacroDefinition(MacroDefinition),
    IncludeMacro(IncludeMacro),
    If(IfChain),
    ForMacro(ForMacro),
    #[allow(dead_code)]
    Newline(Token![nl]),
}

impl Spanned for Statement {
    fn span(&self) -> Span {
        match self {
            Statement::Instruction(instr) => instr.span(),
            Statement::Define(def) => def.span(),
            Statement::Label(lbl) => lbl.span(),
            Statement::MacroDefinition(macro_def) => macro_def.span(),
            Statement::IncludeMacro(include) => include.span(),
            Statement::If(if_macro) => if_macro.span(),
            Statement::ForMacro(for_macro) => for_macro.span(),
            Statement::Newline(nl) => nl.span(),
        }
    }
}

#[derive(Parse, Debug, Clone, PartialEq)]
#[token(Token)]
pub struct Define {
    _define: Token![define],
    pub name: Ident,
    pub value: Expression,
    _nl: Token![nl],
}

impl Spanned for Define {
    fn span(&self) -> Span {
        let mut span = self._define.span();
        span.update_end(self.value.span());
        span
    }
}

#[derive(Parse, Debug, Clone)]
#[token(Token)]
pub struct IfChain {
    pub init_if: If<Token![ifmacro], Token![ifdefmacro]>,
    pub elifs: Vec<If<Token![elifmacro], Token![elifdefmacro]>>,
    pub else_block: Option<Else>,
    _endif: Token![endif],
}

impl Spanned for IfChain {
    fn span(&self) -> Span {
        let mut span = self.init_if.span();
        span.update_end(self._endif.span());
        span
    }
}

#[derive(Parse, Debug, Clone)]
#[token(Token)]
pub struct Else {
    _else_macro: Token![else],
    _nl: Token![nl],
    pub stmts: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum If<I, D> {
    If(IfMacro<I>),
    Def(IfDefMacro<D>),
}

impl<TS: TokenStream<Token = Token>, I: Parse<TS>, D: Parse<TS>> Parse<TS> for If<I, D> {
    fn parse(tokens: &mut TS) -> laps::span::Result<Self> {
        if I::maybe(tokens)? {
            Ok(Self::If(tokens.parse()?))
        } else {
            Ok(Self::Def(tokens.parse()?))
        }
    }

    fn maybe(tokens: &mut TS) -> laps::span::Result<bool> {
        Ok(I::maybe(tokens)? || D::maybe(tokens)?)
    }
}

impl<I: Spanned, D: Spanned> Spanned for If<I, D> {
    fn span(&self) -> Span {
        match self {
            If::If(if_macro) => if_macro.span(),
            If::Def(def) => def.span(),
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug, Clone)]
pub struct IfMacro<T> {
    _ifdef_macro: T,
    pub expression: Expression,
    _nl: Token![nl],
    pub stmts: Vec<Statement>,
}

impl<TS: TokenStream<Token = Token>, T: Parse<TS>> Parse<TS> for IfMacro<T> {
    fn parse(tokens: &mut TS) -> laps::span::Result<Self> {
        Ok(Self {
            _ifdef_macro: tokens.parse()?,
            expression: tokens.parse()?,
            _nl: tokens.parse()?,
            stmts: tokens.parse()?,
        })
    }

    fn maybe(tokens: &mut TS) -> laps::span::Result<bool> {
        T::maybe(tokens)
    }
}

impl<T: Spanned> Spanned for IfMacro<T> {
    fn span(&self) -> Span {
        let mut span = self._ifdef_macro.span();
        span.update_end(self.stmts.try_span().unwrap_or(self._nl.span()));
        span
    }
}

#[derive(Derivative)]
#[derivative(Debug, Clone)]
pub struct IfDefMacro<T> {
    _ifdef_macro: T,
    pub identifier: Ident,
    _nl: Token![nl],
    pub stmts: Vec<Statement>,
}

impl<TS: TokenStream<Token = Token>, T: Parse<TS>> Parse<TS> for IfDefMacro<T> {
    fn parse(tokens: &mut TS) -> laps::span::Result<Self> {
        Ok(Self {
            _ifdef_macro: tokens.parse()?,
            identifier: tokens.parse()?,
            _nl: tokens.parse()?,
            stmts: tokens.parse()?,
        })
    }

    fn maybe(tokens: &mut TS) -> laps::span::Result<bool> {
        T::maybe(tokens)
    }
}

impl<T: Spanned> Spanned for IfDefMacro<T> {
    fn span(&self) -> Span {
        let mut span = self._ifdef_macro.span();
        span.update_end(self.stmts.try_span().unwrap_or(self._nl.span()));
        span
    }
}

#[derive(Parse, Debug, Clone)]
#[token(Token)]
pub struct ForMacro {
    _for_macro: Token![formacro],
    pub ident: Ident,
    _in: Token![in],
    pub expr: Token![sequence],
    _nl: Token![nl],
    pub stmts: Vec<Statement>,
    _end_for: Token![endfor],
    _nl2: Token![nl],
}

impl Spanned for ForMacro {
    fn span(&self) -> Span {
        let mut span = self._for_macro.span();
        span.update_end(self._nl2.span());
        span
    }
}

#[derive(Parse, Debug, Clone)]
#[token(Token)]
pub struct IncludeMacro {
    _includemac: Token![includemac],
    pub path: Token![rawstr],
    _nl: Token![nl],
}

impl Spanned for IncludeMacro {
    fn span(&self) -> Span {
        let mut span = self._includemac.span();
        span.update_end(self._nl.span());
        span
    }
}

#[derive(Parse, Debug, Clone)]
#[token(Token)]
pub struct Instruction {
    pub mnemonic: Mnemonic,
    pub operands: Vec<Expression>,
    _nl: Token![nl],
}

impl Instruction {
    pub fn new(mnemonic: Mnemonic, operands: Vec<Expression>) -> Self {
        Self {
            mnemonic,
            operands,
            _nl: __token_ast_Token::Token0(Token::new(
                TokenKind::Newline,
                Span::new(FileType::Buffer),
            )),
        }
    }
}

impl Spanned for Instruction {
    fn span(&self) -> Span {
        let mut span = self.mnemonic.span();
        if let Some(operands_span) = self.operands.try_span() {
            span.update_end(operands_span);
        }
        span
    }
}

#[derive(Parse, Debug, Clone, PartialEq)]
#[token(Token)]
pub enum Mnemonic {
    Mnemonic(Token![mnemonic]),
    MacroCall(Ident),
}

impl Spanned for Mnemonic {
    fn span(&self) -> Span {
        match self {
            Mnemonic::Mnemonic(mnemonic) => mnemonic.span(),
            Mnemonic::MacroCall(call) => call.span(),
        }
    }
}

macro_rules! binop {
    ($($name: ident: $child: ident, $op: ty),* $(,)?) => {
        $(
            #[derive(Parse, Debug, Clone, PartialEq)]
            #[token(Token)]
            pub struct $name {
                pub first: $child,
                pub rest: Vec<($op, $child)>,
            }

            impl Spanned for $name {
                fn span(&self) -> Span {
                    let mut span = self.first.span();
                    if let Some(rest_span) = self.rest.try_span() {
                        span.update_end(rest_span);
                    }
                    span
                }
            }
        )*
    };
}

binop!(
    Expression: LogicAnd, Token![|],
    LogicAnd: Equality, Token![&],
    Equality: Comparison, Token![equality],
    Comparison: Term, Token![cmp],
    Term: Factor, Token![+-],
    Factor: Unary, Token![*/],
);

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
    fn span(&self) -> Span {
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
    Identifier(Ident),
    Char(Token![chr]),
    MacroExpression(Token![macexpr]),
    Parenthesized(Token![lparen], Box<Expression>, Token![rparen]),
}

impl Spanned for Primary {
    fn span(&self) -> Span {
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

#[derive(Debug, Clone)]
pub struct MacroDefinition {
    pub mac: Token![mac],
    pub args: Vec<Ident>,
    pub quantifier: Option<Token![+*]>,
    _nl2: Token![nl],
    pub body: Vec<Statement>,
    _end: Token![endmac],
    _nl: Token![nl],
}

impl<TS: TokenStream<Token = Token>> Parse<TS> for MacroDefinition {
    fn parse(tokens: &mut TS) -> laps::span::Result<Self> {
        let mac = tokens.parse()?;
        let mut args = vec![];
        loop {
            if matches!(
                tokens.peek2()?,
                (
                    Token {
                        kind: TokenKind::Plus | TokenKind::Mult,
                        ..
                    },
                    Token {
                        kind: TokenKind::Newline | TokenKind::Eof,
                        ..
                    },
                )
            ) {
                break;
            }
            if !Ident::maybe(tokens)? {
                break;
            }
            args.push(tokens.parse()?);
        }
        let quantifier = if !args.is_empty() {
            tokens.parse()?
        } else {
            None
        };
        let _nl2 = tokens.parse()?;
        let body = tokens.parse()?;
        let _end = tokens.parse()?;
        let _nl = tokens.parse()?;
        Ok(Self {
            mac,
            args,
            quantifier,
            _nl2,
            body,
            _end,
            _nl,
        })
    }

    fn maybe(tokens: &mut TS) -> laps::span::Result<bool> {
        <Token![mac]>::maybe(tokens)
    }
}

impl Spanned for MacroDefinition {
    fn span(&self) -> Span {
        let mut span = self.mac.span();
        span.update_end(self._nl.span());
        span
    }
}
