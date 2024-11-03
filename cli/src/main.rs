use std::{fs::File, process::ExitCode, time::Instant};

use clap::Parser;
use lantern::{builtin, parse::{ast::parse, read::{FileStream, TokenStream}, tokenizer::tokenize}, runtime};

#[derive(Parser, Debug, Clone)]
#[command(name = "LanternLang")]
#[command(version, about)]
struct Args {
    file: String,

    #[arg(long, short)]
    verbose: bool,

    #[arg(long)]
    very_verbose: bool,
}

fn main() -> ExitCode {
    let Args { file: file_name, verbose, very_verbose } = Args::parse();

    let file = File::open(file_name.clone()).unwrap();
    let file_stream = FileStream::new(file).unwrap();

    if verbose {
        println!("tokenizing... (run with --very-verbose to print result)");
    }
    let before = Instant::now();
    let tokens = match tokenize(file_stream) {
        Ok(tokens) => tokens,
        Err(err) => {
            eprintln!("{err}");
            return ExitCode::FAILURE;
        },
    };

    if verbose {
        if very_verbose { println!("{tokens:#?}"); };
        println!("tokenization took {:?}", Instant::now().duration_since(before));
        println!();
    }

    if verbose {
        println!("parsing into ast...")
    }
    let before = Instant::now();
    let ast = match parse(TokenStream::new(tokens)) {
        Ok(ast) => ast,
        Err(err) => {
            eprintln!("{err}");
            return ExitCode::FAILURE;
        },
    };

    if verbose {
        if very_verbose { println!("{ast:#?}"); };
        println!("parsing took {:?}", Instant::now().duration_since(before));
        println!();
    }

    println!("executing {file_name}...");

    let context = builtin::global_context();

    if let Err(err) = runtime::run(ast, file_name, context) {
        eprintln!("{err}");
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

