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
    let reader = Reader::from_path(fp).expect("failed to open file");
    let span = reader.span().clone();
    let lexer = TokenKind::lexer(reader);

    let mut tokens: TokenBuffer<laps::lexer::Lexer<Reader<std::fs::File, 1024>, TokenKind>, Token> =
        TokenBuffer::new(lexer);

    loop {
        if let Ok(stmt) = tokens.parse::<Statement>() {
            match stmt {
                Statement::Eof(_) => break,
                _ => println!("{:#?}", stmt),
            }
        } else {
            span.log_summary();
            exit(span.error_num() as i32);
        }
    }

    Ok(())
}
