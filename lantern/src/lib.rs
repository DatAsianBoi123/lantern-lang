use std::{error::Error, fs::File, io::Read};

use lantern_parse::{ast::to_stmts, read::{FileStream, TokenStream}, tokenizer::tokenize};

#[cfg(feature = "runtime")]
pub extern crate lantern_runtime as runtime;

#[cfg(feature = "lang")]
pub extern crate lantern_lang as lang;

#[cfg(feature = "parse")]
pub extern crate lantern_parse as parse;

#[cfg(feature = "macros")]
pub extern crate lantern_macros as macros;

#[cfg(feature = "builtin")]
pub extern crate lantern_builtin as builtin;

pub fn run_file(file: &mut File) -> Result<(), Box<dyn Error>> {
    let mut code = String::new();
    file.read_to_string(&mut code)?;
    run(code)
}

pub fn run(code: String) -> Result<(), Box<dyn Error>> {
    let tokens = tokenize(FileStream::with_string(code))?;
    let stmts = to_stmts(TokenStream::new(tokens))?;
    lantern_runtime::execute(stmts, builtin::global_scope())?;
    Ok(())
}

