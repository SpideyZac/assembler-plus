mod emit;
mod error;
mod lexer;
mod tokens;

use crate::emit::Emitter;
use crate::lexer::Lexer;

fn main() {
    let code = ";.main str r1 r2 0\nhlt".to_string();
    let emitter = Emitter::new(code.clone(), "out.asm".to_string());
    let mut lexer = Lexer::new(code);
    let tokens = lexer.lex();
    if let Ok(token) = tokens {
        println!("{:?}", token);
    } else {
        let error = tokens.unwrap_err();
        emitter.emit_error(error);
    }
}
