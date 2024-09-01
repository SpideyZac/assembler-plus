use crate::lexer::{Token, TokenKind};

use derivative::Derivative;
use laps::{log_warning, prelude::*, span::TrySpan};

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
                | TokenKind::Minus
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
        [undefine] => { kind: TokenKind::Undefine, prompt: "undefine" },

        [+*] => { kind: TokenKind::Plus | TokenKind::Mult, prompt: "plus or star" },

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
        [exportmac] => { kind: TokenKind::ExportMacro, prompt: "export macro" },
        [includemac] => { kind: TokenKind::IncludeMacro, prompt: "include macro" },
        [ifmacro] => { kind: TokenKind::IfMacro, prompt: "if macro" },
        [ifdefmacro] => { kind: TokenKind::IfDefMacro, prompt: "ifdef macro" },
        [ifundefmacro] => { kind: TokenKind::IfUndefMacro, prompt: "ifundef macro" },
        [elifmacro] => { kind: TokenKind::ElifMacro, prompt: "elif macro" },
        [elifdefmacro] => { kind: TokenKind::ElifDefMacro, prompt: "elifdef macro" },
        [elifundefmacro] => { kind: TokenKind::ElifUndefMacro, prompt: "elifundef macro" },
        [else] => { kind: TokenKind::ElseMacro, prompt: "else macro" },
        [endif] => { kind: TokenKind::EndIfMacro, prompt: "endif macro" },
        [formacro] => { kind: TokenKind::ForMacro, prompt: "for macro" },
        [in] => { kind: TokenKind::In, prompt: "in" },
        [endfor] => { kind: TokenKind::EndForMacro, prompt: "end for" },
        [macexpr] => { kind: TokenKind::MacroExpression(_), prompt: "macro expression" },

        [eof] => { kind: TokenKind::Eof, prompt: "end of file" },
    }
}

#[derive(Spanned, Debug, Clone, PartialEq)]
pub struct Ident {
    identifier: Token![ident],
}

impl Ident {
    pub fn get_name(&self) -> &str {
        match &self.identifier.0.kind {
            TokenKind::Identifier(ident) => ident,
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
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

#[derive(Parse, Spanned, Debug, Clone)]
#[token(Token)]
pub enum Symbol {
    Ident(Ident),
    Label(Token![label]),
}

#[derive(Parse, Spanned, Debug, Clone)]
#[token(Token)]
pub enum Statement {
    Instruction(Instruction),
    Define(Define),
    Undefine(Undefine),
    Label(Token![label]),
    MacroDefinition(MacroDefinition),
    ExportMacro(ExportMacro),
    IncludeMacro(IncludeMacro),
    If(IfChain),
    ForMacro(ForMacro),
    #[allow(dead_code)]
    Newline(Token![nl]),
}

#[derive(Parse, Spanned, Debug, Clone, PartialEq)]
#[token(Token)]
pub struct Define {
    _define: Token![define],
    pub name: Ident,
    pub value: Expression,
    _nl: Token![nl],
}

#[derive(Parse, Spanned, Debug, Clone, PartialEq)]
#[token(Token)]
pub struct Undefine {
    _udefine: Token![undefine],
    pub name: Ident,
    _nl: Token![nl],
}

#[derive(Parse, Spanned, Debug, Clone)]
#[token(Token)]
pub struct IfChain {
    pub init_if: If<Token![ifmacro], Token![ifdefmacro], Token![ifundefmacro]>,
    pub elifs: Vec<If<Token![elifmacro], Token![elifdefmacro], Token![elifundefmacro]>>,
    pub else_block: Option<Else>,
    _endif: Token![endif],
}

#[derive(Parse, Debug, Clone)]
#[token(Token)]
pub struct Else {
    _else_macro: Token![else],
    _nl: Token![nl],
    pub stmts: Vec<Statement>,
}

#[derive(Spanned, Debug, Clone)]
pub enum If<I: Spanned, D: Spanned, U: Spanned> {
    If(IfMacro<I>),
    Def(IfDefMacro<D>),
    Undef(IfUndefMacro<U>),
}

impl<
        TS: TokenStream<Token = Token>,
        I: Parse<TS> + Spanned,
        D: Parse<TS> + Spanned,
        U: Parse<TS> + Spanned,
    > Parse<TS> for If<I, D, U>
{
    fn parse(tokens: &mut TS) -> laps::span::Result<Self> {
        if I::maybe(tokens)? {
            Ok(Self::If(tokens.parse()?))
        } else if D::maybe(tokens)? {
            Ok(Self::Def(tokens.parse()?))
        } else {
            Ok(Self::Undef(tokens.parse()?))
        }
    }

    fn maybe(tokens: &mut TS) -> laps::span::Result<bool> {
        Ok(I::maybe(tokens)? || D::maybe(tokens)? || U::maybe(tokens)?)
    }
}

#[derive(Spanned, Derivative)]
#[derivative(Debug, Clone)]
pub struct IfMacro<T: Spanned> {
    _if_macro: T,
    pub expression: Expression,
    _nl: Token![nl],
    #[try_span]
    pub stmts: Vec<Statement>,
}

impl<TS: TokenStream<Token = Token>, T: Parse<TS> + Spanned> Parse<TS> for IfMacro<T> {
    fn parse(tokens: &mut TS) -> laps::span::Result<Self> {
        Ok(Self {
            _if_macro: tokens.parse()?,
            expression: tokens.parse()?,
            _nl: tokens.parse()?,
            stmts: tokens.parse()?,
        })
    }

    fn maybe(tokens: &mut TS) -> laps::span::Result<bool> {
        T::maybe(tokens)
    }
}

#[derive(Spanned, Derivative)]
#[derivative(Debug, Clone)]
pub struct IfDefMacro<T: Spanned> {
    _ifdef_macro: T,
    pub identifier: Ident,
    _nl: Token![nl],
    #[try_span]
    pub stmts: Vec<Statement>,
}

impl<TS: TokenStream<Token = Token>, T: Parse<TS> + Spanned> Parse<TS> for IfDefMacro<T> {
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

#[derive(Spanned, Derivative)]
#[derivative(Debug, Clone)]
pub struct IfUndefMacro<T: Spanned> {
    _ifundef_macro: T,
    pub identifier: Ident,
    _nl: Token![nl],
    #[try_span]
    pub stmts: Vec<Statement>,
}

impl<TS: TokenStream<Token = Token>, T: Parse<TS> + Spanned> Parse<TS> for IfUndefMacro<T> {
    fn parse(tokens: &mut TS) -> laps::span::Result<Self> {
        Ok(Self {
            _ifundef_macro: tokens.parse()?,
            identifier: tokens.parse()?,
            _nl: tokens.parse()?,
            stmts: tokens.parse()?,
        })
    }

    fn maybe(tokens: &mut TS) -> laps::span::Result<bool> {
        T::maybe(tokens)
    }
}

#[derive(Debug, Clone)]
pub enum Sequence {
    Str(Token![rawstr]),
    MacroExpr(Token![macexpr]),
    Multiple(Vec<Expression>),
}

impl<TS: TokenStream<Token = Token>> Parse<TS> for Sequence {
    fn parse(tokens: &mut TS) -> laps::span::Result<Self> {
        if <Token![rawstr]>::maybe(tokens)? {
            Ok(Self::Str(tokens.parse()?))
        } else if <Token![macexpr]>::maybe(tokens)? {
            Ok(Self::MacroExpr(tokens.parse()?))
        } else {
            let first = tokens.parse()?;
            let rest: Vec<_> = tokens.parse()?;
            let mut all = vec![first];
            all.extend(rest);
            Ok(Self::Multiple(all))
        }
    }

    fn maybe(tokens: &mut TS) -> laps::span::Result<bool> {
        Ok(<Token![rawstr]>::maybe(tokens)?
            || <Token![macexpr]>::maybe(tokens)?
            || Expression::maybe(tokens)?)
    }
}

impl Spanned for Sequence {
    fn span(&self) -> laps::span::Span {
        match self {
            Sequence::Str(str) => str.span(),
            Sequence::MacroExpr(mac_expr) => mac_expr.span(),
            Sequence::Multiple(multiple) => multiple.try_span().unwrap(), // There will always be at least one element
        }
    }
}

#[derive(Parse, Spanned, Debug, Clone)]
#[token(Token)]
pub struct ForMacro {
    _for_macro: Token![formacro],
    pub ident: Ident,
    _in: Token![in],
    pub expr: Sequence,
    _nl: Token![nl],
    pub stmts: Vec<Statement>,
    _end_for: Token![endfor],
    _nl2: Token![nl],
}

#[derive(Parse, Spanned, Debug, Clone)]
#[token(Token)]
pub struct ExportMacro {
    _export_macro: Token![exportmac],
    pub symbol: Symbol,
    _nl: Token![nl],
}

#[derive(Parse, Spanned, Debug, Clone)]
#[token(Token)]
pub struct IncludeMacro {
    _includemac: Token![includemac],
    pub path: Token![rawstr],
    _nl: Token![nl],
}

#[derive(Parse, Spanned, Debug, Clone)]
#[token(Token)]
pub struct Instruction {
    pub mnemonic: Mnemonic,
    pub operands: Vec<Expression>,
    _nl: Token![nl],
}

impl Instruction {
    pub fn new(mnemonic: Mnemonic, operands: Vec<Expression>) -> Self {
        let span = operands.try_span().unwrap_or(mnemonic.span());
        Self {
            mnemonic,
            operands,
            _nl: __token_ast_Token::Token0(Token::new(TokenKind::Newline, span)),
        }
    }
}

#[derive(Parse, Spanned, Debug, Clone, PartialEq)]
#[token(Token)]
pub enum Mnemonic {
    Mnemonic(Token![mnemonic]),
    MacroCall(Ident),
}

macro_rules! binop {
    ($($name: ident: $child: ident, $op: ty),* $(,)?) => {
        $(
            #[derive(Spanned, Debug, Clone, PartialEq)]
            pub struct $name {
                pub first: $child,
                #[try_span]
                pub rest: Vec<($op, $child)>,
            }

            impl<TS: TokenStream<Token = Token>> Parse<TS> for $name {
                fn parse(tokens: &mut TS) -> laps::span::Result<Self> {
                    let first = tokens.parse()?;
                    let mut rest = Vec::new();
                    let mut look_ahead = tokens.lookahead();
                    while look_ahead
                        .maybe::<_, $op>(|_| unreachable!())?
                        .maybe::<_, $child>(|_| unreachable!())?
                        .result()?
                    {
                        rest.push((tokens.parse()?, tokens.parse()?));
                        look_ahead = tokens.lookahead();
                    }
                    Ok(Self { first, rest })
                }

                fn maybe(tokens: &mut TS) -> laps::span::Result<bool> {
                    $child::maybe(tokens)
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

#[derive(Spanned, Debug, Clone, PartialEq)]
pub struct Unary {
    #[try_span]
    pub ops: Vec<Token![unary]>,
    pub primary: Primary,
}

impl<TS: TokenStream<Token = Token>> Parse<TS> for Unary {
    fn parse(tokens: &mut TS) -> laps::span::Result<Self> {
        let mut ops: Vec<Token![unary]> = vec![];
        while <Token![unary]>::maybe(tokens)? {
            ops.push(tokens.parse()?);
        }
        while !Primary::maybe(tokens)? {
            if let Some(token) = ops.pop() {
                tokens.unread(token.0);
            } else {
                break;
            }
        }
        Ok(Self {
            ops,
            primary: tokens.parse()?,
        })
    }

    fn maybe(tokens: &mut TS) -> laps::span::Result<bool> {
        Ok(<Token![unary]>::maybe(tokens)? || Primary::maybe(tokens)?)
    }
}

#[derive(Parse, Spanned, Debug, Clone, PartialEq)]
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

#[derive(Spanned, Debug, Clone)]
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
