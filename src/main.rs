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
use link::link;

fn generate_file(fp: &str, imported_files: &mut Vec<String>) -> (Vec<Vec<Statement>>, Span) {
    imported_files.push(fp.to_owned());

    let reader = Reader::from_path(fp).expect("failed to open file");

    let span = reader.span().clone();
    let lexer = TokenKind::lexer(reader);

    let mut tokens = TokenBuffer::new(lexer);

    let mut files = Vec::new();
    let mut statements = Vec::new();

    let core_path = env::current_exe()
        .expect("Failed to get exe path")
        .parent()
        .expect("Failed to get exe dir")
        .join("resources/core.ap")
        .to_str()
        .expect("Failed to get core file")
        .to_owned();

    if !imported_files.contains(&core_path) {
        let (stmts, _) = generate_file(&core_path, imported_files);
        files.extend(stmts);
    }

    loop {
        if tokens.peek().unwrap().kind == TokenKind::Eof {
            break;
        }
        if let Ok(stmt) = tokens.parse::<Statement>() {
            match stmt {
                Statement::IncludeMacro(incl) => match incl.path.0.kind {
                    TokenKind::RawString(path) => {
                        if !imported_files.contains(&path.value) {
                            let (stmts, _) = generate_file(&path.value, imported_files);
                            files.extend(stmts);
                        }
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

    files.push(statements);

    (files, span)
}

fn main() -> Result<()> {
    let mut args = env::args();
    args.next(); // Skip the program name
    let fp = args.next().expect("expected file path");
    let op = args.next().expect("expected output path");
    let (files, span) = generate_file(&fp, &mut vec![]);

    let output = Assembler::new(files).generate();
    if span.error_num() > 0 {
        span.log_summary();
        exit(span.error_num() as i32);
    }
    let output = link(&output.iter().map(|file| &file[..]).collect::<Vec<_>>());
    if span.error_num() > 0 {
        span.log_summary();
        exit(span.error_num() as i32);
    }
    std::fs::write(op, output).expect("failed to write output");
    Ok(())
}
