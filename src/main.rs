mod codegen;
mod lexer;
mod parser;

use std::env;
use std::process::exit;

use crate::lexer::TokenKind;
use crate::parser::Statement;

use laps::input::InputStream;
use laps::lexer::Tokenize;
use laps::reader::Reader;
use laps::span::{Result, Span};
use laps::token::{TokenBuffer, TokenStream};

fn generate_file(fp: &str) -> (Vec<Statement>, Span) {
    let mut content = std::fs::read_to_string(fp).expect("failed to read file");
    content = content.replace("brh =", "brh eq");
    content = content.replace("brh !=", "brh ne");
    content = content.replace("brh >=", "brh ge");
    content = content.replace("brh <", "brh lt");
    std::fs::write(fp, content).expect("failed to write file");
    let reader = Reader::from_path(fp).expect("failed to open file");

    let span = reader.span().clone();
    let lexer = TokenKind::lexer(reader);

    let mut tokens = TokenBuffer::new(lexer);

    let mut statements = Vec::new();

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

    let mut codegen = codegen::Codegen::new(statements);
    if let Ok(output) = codegen.generate() {
        std::fs::write(op, output).expect("failed to write output");
    } else {
        span.log_summary();
        exit(span.error_num() as i32);
    }

    Ok(())
}
