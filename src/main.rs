mod assemble;
mod common;
mod lexer;
mod link;
mod parser;

use std::env;
use std::process::exit;

use crate::lexer::TokenKind;
use crate::parser::Statement;

use assemble::Assembler;
use laps::input::InputStream;
use laps::lexer::Tokenize;
use laps::reader::Reader;
use laps::span::{Result, Span};
use laps::token::{TokenBuffer, TokenStream};
use link::Linker;

fn generate_file(fp: &str) -> (Vec<Statement>, Span) {
    let reader = Reader::from_path(fp).expect("failed to open file");

    let span = reader.span().clone();
    let lexer = TokenKind::lexer(reader);

    let mut tokens = TokenBuffer::new(lexer);

    let mut statements = Vec::new();

    if !fp.ends_with("core.ap") {
        let (stmts, _) = generate_file(
            env::current_exe()
                .expect("Failed to get exe path")
                .parent()
                .expect("Failed to get exe dir")
                .join("resources/core.ap")
                .to_str()
                .expect("Failed to get core file"),
        );
        statements.extend(stmts);
    }

    loop {
        if tokens.peek().unwrap().kind == TokenKind::Eof {
            break;
        }
        if let Ok(stmt) = tokens.parse::<Statement>() {
            match stmt {
                Statement::IncludeMacro(incl) => match incl.path.0.kind {
                    TokenKind::RawString(path) => {
                        let (stmts, _) = generate_file(&path.value);
                        statements.extend(stmts);
                    }
                    _ => unreachable!(),
                },
                _ => statements.push(stmt),
            }
        } else {
            span.log_summary();
            exit(span.error_num() as i32);
        }
    }

    (statements, span)
}

fn main() -> Result<()> {
    let mut args = env::args();
    args.next(); // Skip the program name
    let fp = args.next().expect("expected file path");
    let op = args.next().expect("expected output path");
    let (statements, span) = generate_file(&fp);

    let assembler = Assembler::new(statements);
    let output = match assembler.generate() {
        Ok(output) => output,
        Err(_) => {
            span.log_summary();
            exit(span.error_num() as i32);
        }
    };
    let mut linker = Linker::new(output);
    match linker.link() {
        Ok(output) => {
            std::fs::write(op, output).expect("failed to write output");
        }
        Err(_) => {
            span.log_summary();
            exit(span.error_num() as i32);
        }
    }

    Ok(())
}
