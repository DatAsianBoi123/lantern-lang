pub mod read;
pub mod tokenizer;
pub mod ast;
pub mod error;

#[macro_export]
macro_rules! try_ordered {
    ($($pat: pat = $ex: expr $( , if $( $cond: expr )? )? => $handler: expr),* $(,)?) => {{
        $(
            try_ordered!(@ $pat = $ex $( ,if $( $cond )? )? => $handler);
        )*
    }};

    (@ $pat: pat = $ex: expr => $handler: expr) => {{
        match $ex {
            Ok($pat) => return Ok($handler),
            Err(err) => return Err(err),
        }
    }};

    (@ $pat: pat = $ex: expr , if $cond: expr => $handler: expr) => {{
        if $cond {
            try_ordered!(@ $pat = $ex => $handler)
        }
    }};

    (@ $pat: pat = $ex: expr , if => $handler: expr) => {{
        match $ex {
            Ok($pat) => return Ok($handler),
            Err(Recoverable::Nonfatal(_)) => {},
            Err(Recoverable::Fatal(err)) => return Err(err),
        }
    }};
}

