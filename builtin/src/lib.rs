use std::{cell::RefCell, rc::Rc};

use lantern_lang::{error::RuntimeError, record::LanternAny, runtime_error, scope::{RuntimeContext, Scope}, LanternValue, LanternVariable};
use lantern_macros::lantern_fun;

pub mod op;

pub fn global_scope() -> Rc<RefCell<Scope>> {
    let mut context = RuntimeContext::new();
    context
        .add_variable(inf())
        .add_function(print())
        .add_function(stringify())
        .add_function(input())
        .add_function(assert())
        .add_function(assert_eq())
        .add_function(assert_neq());

    Rc::new(RefCell::new(Scope::new(context)))
}

pub fn inf() -> LanternVariable {
    LanternVariable::new("inf", LanternValue::Num(f64::INFINITY))
}

#[lantern_fun]
pub fn print(arg: LanternAny) {
    println!("{}", arg.0);
    Ok(())
}

#[lantern_fun]
pub fn stringify(arg: LanternAny) -> String {
    Ok(arg.0.to_string())
}

#[lantern_fun]
pub fn input() -> String {
    let mut buf = String::new();
    std::io::stdin()
        .read_line(&mut buf)
        .map_err(RuntimeError::new)?;

    let buf = buf.trim().to_string();

    Ok(buf)
}

#[lantern_fun]
pub fn assert(cond: bool) {
    if !cond {
        Err(runtime_error!("Assertion error: {cond}"))
    } else {
        Ok(())
    }
}

#[lantern_fun]
pub fn assert_eq(l: LanternAny, r: LanternAny) {
    if l != r {
        Err(runtime_error!("Assertion error: {} == {}", l.0, r.0))
    } else {
        Ok(())
    }
}

#[lantern_fun]
pub fn assert_neq(l: LanternAny, r: LanternAny) {
    if l == r {
        Err(runtime_error!("Assertion error: {} != {}", l.0, r.0))
    } else {
        Ok(())
    }
}

