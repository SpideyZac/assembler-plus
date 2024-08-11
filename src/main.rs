mod lexer;

use crate::lexer::{Token, TokenKind};

use laps::lexer::Tokenize;
use laps::reader::Reader;
use laps::span::Result;
use laps::token::TokenBuffer;

fn main() -> Result<()> {
    let reader = Reader::from_stdin();
    let lexer = TokenKind::lexer(reader);

    let mut tokens: TokenBuffer<
        laps::lexer::Lexer<Reader<std::io::Stdin, 1024>, TokenKind>,
        Token,
    > = TokenBuffer::new(lexer);

    Ok(())
}
