mod codegen;
mod lexer;
mod parser;

use std::env;
use std::process::exit;

use crate::lexer::{Token, TokenKind};
use crate::parser::Statement;

use laps::input::InputStream;
use laps::lexer::Tokenize;
use laps::reader::Reader;
use laps::span::Result;
use laps::token::{TokenBuffer, TokenStream};

fn main() -> Result<()> {
    let mut args = env::args();
    args.next(); // Skip the program name
    let fp = args.next().expect("expected file path");
    let op = args.next();
    let op = if op.is_some() {
        op.unwrap()
    } else {
        let mut path = fp.clone();
        path.push_str(".asp");
        path
    };
    let reader = Reader::from_path(fp).expect("failed to open file");
    let span = reader.span().clone();
    let lexer = TokenKind::lexer(reader);

    let mut tokens: TokenBuffer<laps::lexer::Lexer<Reader<std::fs::File, 1024>, TokenKind>, Token> =
        TokenBuffer::new(lexer);

    let mut statements = Vec::new();

    loop {
        if tokens.peek().unwrap().kind == TokenKind::Eof {
            break;
        }
        if let Ok(stmt) = tokens.parse::<Statement>() {
            statements.push(stmt);
        } else {
            span.log_summary();
            exit(span.error_num() as i32);
        }
    }

    let mut codegen = codegen::Codegen::new(statements);
    if let Ok(output) = codegen.generate() {
        std::fs::write(op, output).expect("failed to write output");
    } else {
        span.log_summary();
        exit(span.error_num() as i32);
    }

    Ok(())
}
