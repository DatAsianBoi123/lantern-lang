use std::{error::Error, fs::File, io::Read, path::Path};

use lantern_parse::{read::{FileStream, TokenStream}, tokenizer::tokenize};
use parse::ast::parse;

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

pub fn run(path: impl AsRef<Path>) -> Result<(), Box<dyn Error>> {
    let mut code = String::new();
    File::open(&path)?.read_to_string(&mut code)?;

    let tokens = tokenize(FileStream::with_string(code))?;
    let ast = parse(TokenStream::new(tokens))?;
    lantern_runtime::run(ast, path, builtin::global_context())?;
    Ok(())
}

