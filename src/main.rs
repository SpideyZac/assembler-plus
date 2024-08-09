mod ast;
mod emit;
mod error;
mod lexer;
mod parser;
mod tokens;

use crate::emit::Emitter;
use crate::lexer::Lexer;
use crate::parser::Parser;

fn main() {
    let code = ".main\nnop".to_string();
    let emitter = Emitter::new(code.clone(), "out.asm".to_string());
    let mut lexer = Lexer::new(code);
    let tokens = lexer.lex();
    if let Ok(token) = tokens {
        println!("{:?}", token);
        let mut parser = Parser::new(token);
        let ast = parser.parse();
        if let Ok(ast) = ast {
            println!("{:?}", ast);
        } else {
            let error = ast.unwrap_err();
            emitter.emit_error(error);
        }
    } else {
        let error = tokens.unwrap_err();
        emitter.emit_error(error);
    }
}
