use colored::Colorize;
use std::io::Write;

use crate::error::Error;

pub struct Emitter {
    code: String,
    file: std::fs::File,
}

impl Emitter {
    pub fn new(code: String, out_path: String) -> Self {
        let file = std::fs::OpenOptions::new()
            .write(true)
            .create(true)
            .open(&out_path)
            .unwrap();
        Self { code, file }
    }

    pub fn emit_clear_file(&self) {}

    pub fn emit_error(&self, error: Error) {
        let line = self.code.lines().nth(error.line - 1).unwrap();
        let location = format!("{}:{}", error.line, error.column);
        let line_pointer = format!("{: >1$}", "^", error.column - 1);
        let error_message = error.message;

        println!("{}", "Error:".red());
        println!("{} {}", "Location:".red(), location.red());
        println!("{}", line);
        println!("{}", line_pointer.red());
        println!("{}", error_message.red());
        std::process::exit(error.error_type as i32);
    }

    pub fn emits(&mut self, s: String) {
        writeln!(self.file, "{}", s).unwrap();
    }
}
