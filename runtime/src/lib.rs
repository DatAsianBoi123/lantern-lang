use std::{cell::RefCell, ops::ControlFlow, rc::Rc};

use lantern_builtin::op::{perform_binary_op, perform_unary_op};
use lantern_lang::{error::{InvalidReturnType, MismatchedTypes, RuntimeError, UnknownItem}, record::{LanternMethod, LanternRecordFrame}, runtime_error, scope::{RuntimeContext, Scope}, LanternFunction, LanternFunctionArg, LanternFunctionBody, LanternValue, LanternVariable, ReturnType};
use lantern_parse::{ast::{expr::{AccessField, BinaryOperation, Branch, CallMethod, CoerceBlock, Expr, FunCall, NewRec, PipeBlock, UnaryOperation}, Block, FunArgs, FunDefinition, IfBranch, IfStatement, LanternType, LoopStatement, RecDefinition, Ret, Stmt, ValAssignment, ValBinding, WhileStatement}, error::ExpectedError, read::{ItemStream, TokenStream}, tokenizer::{Ident, Literal}};

pub type Result<T> = std::result::Result<T, RuntimeError>;
pub type ScopeMut = Rc<RefCell<Scope>>;

macro_rules! eval_or_ret {
    ($expr: expr, $scope: expr) => {{
        match eval_expr($expr, $scope)? {
            ControlFlow::Continue(val) => val,
            ControlFlow::Break(ret) => return Ok(ret),
        }
    }};
}

macro_rules! eval_or_break {
    ($expr: expr, $scope: expr) => {{
        match eval_expr($expr, $scope)? {
            ControlFlow::Continue(val) => val,
            br @ ControlFlow::Break(_) => return Ok(br),
        }
    }};
}

pub fn execute(stmts: Vec<Stmt>, scope: ScopeMut) -> Result<ReturnType> {
    for stmt in stmts {
        match stmt {
            Stmt::Expr(expr) => {
                eval_or_ret!(expr, scope.clone());
            },
            Stmt::ValBinding(ValBinding { ident: Ident { name, .. }, r#type, init }) => {
                let value = eval_or_ret!(init, scope.clone());

                if !value.r#type().applies_to(&r#type) { return Err(RuntimeError::new(MismatchedTypes(r#type, value.r#type()))) }

                scope.borrow_mut().add_variable(LanternVariable { name, r#type, value });
            },
            Stmt::If(IfStatement { condition, block, branch }) => {
                match eval_if_branch(IfBranch::Elif(condition, block, branch.map(Box::new)), scope.clone())? {
                    ReturnType::None => {},
                    ret_type => return Ok(ret_type),
                }
            },
            Stmt::FunDefinition(fun_definition) => scope.borrow_mut().add_function(gen_fun(fun_definition, scope.clone())?),
            Stmt::RecDefinition(RecDefinition { ident, fields, methods }) => {
                let methods = methods.into_iter()
                    .map(|fun_definition| gen_fun(fun_definition, scope.clone()).map(|function| LanternMethod { function }))
                    .collect::<Result<Vec<LanternMethod>>>()?;
                let rec_frame = LanternRecordFrame { ident, fields: gen_args(fields, scope.clone())?, methods };

                scope.borrow_mut().add_record(Rc::new(rec_frame));
            },
            Stmt::Loop(LoopStatement { block: Block { stmts } }) => {
                loop {
                    // TODO: dont clone on every loop
                    match execute(stmts.clone(), Rc::new(RefCell::new(Scope::nested(scope.clone(), RuntimeContext::new()))))? {
                        ReturnType::Return(ret) => return Ok(ReturnType::Return(ret)),
                        ReturnType::Break => break,
                        _ => {},
                    }
                }
            },
            Stmt::While(WhileStatement { condition, block: Block { stmts } }) => {
                loop {
                    // TODO: clones

                    let condition = match eval_or_ret!(condition.clone(), scope.clone()) {
                        LanternValue::Bool(condition) => condition,
                        value => return Err(RuntimeError::new(MismatchedTypes(LanternType::Bool, value.r#type()))),
                    };

                    if !condition { break; }

                    match execute(stmts.clone(), Rc::new(RefCell::new(Scope::nested(scope.clone(), RuntimeContext::new()))))? {
                        ReturnType::Return(ret) => return Ok(ReturnType::Return(ret)),
                        ReturnType::Break => break,
                        _ => {},
                    }
                }
            },
            Stmt::Break => return Ok(ReturnType::Break),
            Stmt::Cont => return Ok(ReturnType::Continue),
            Stmt::Ret(Ret { expr }) => return match expr {
                Some(expr) => Ok(ReturnType::Return(eval_or_ret!(expr, scope.clone()))),
                None => Ok(ReturnType::Return(LanternValue::Null)),
            },
            Stmt::ValAssignment(ValAssignment { ident, val }) => {
                let val = eval_or_ret!(val, scope.clone());
                scope.borrow_mut().reassign_variable(ident.name, val)?;
            },
        };
    }

    Ok(ReturnType::None)
}

pub fn eval_expr(expr: Expr, scope: ScopeMut) -> Result<ControlFlow<ReturnType, LanternValue>> {
    match expr {
        Expr::Group(expr) => eval_expr(*expr, scope),
        Expr::FunCall(FunCall { ident: Ident { name, .. }, args }) => {
            let function = scope.borrow().function(&name).ok_or_else(|| runtime_error!(UnknownItem::Function))?;
            eval_fun(scope.clone(), args, function)
        },
        Expr::NewRec(NewRec { ident: Ident { name, .. }, args }) => {
            let rec = scope.borrow().record(&name).ok_or_else(|| runtime_error!(UnknownItem::Record))?;

            let mut init_args = Vec::with_capacity(args.len());
            for arg in args {
                let value = match eval_expr(arg,scope.clone())? {
                    ControlFlow::Continue(cont) => cont,
                    br @ ControlFlow::Break(_) => return Ok(br),
                };
                init_args.push(value);
            }
            LanternRecordFrame::init(rec, init_args)
                .map(LanternValue::Custom)
                .map(ControlFlow::Continue)
        },
        Expr::Lit(Literal { kind, .. }) => {
            use lantern_parse::tokenizer::LiteralKind as L;

            let val = match kind {
                L::String(str) => LanternValue::String(str),
                L::Num(num) => LanternValue::Num(num),
                L::Bool(bool) => LanternValue::Bool(bool),
                L::Some(tokens) => {
                    let expr = TokenStream::new(tokens).read().map_err(RuntimeError::new)?;
                    LanternValue::Option(Some(Box::new(eval_or_break!(expr, scope.clone()))))
                },
                L::Ok(tokens) => {
                    let expr = TokenStream::new(tokens).read().map_err(RuntimeError::new)?;
                    LanternValue::Result(Ok(Box::new(eval_or_break!(expr, scope.clone()))))
                },
                L::Err(tokens) => {
                    let expr = TokenStream::new(tokens).read().map_err(RuntimeError::new)?;
                    LanternValue::Result(Err(Box::new(eval_or_break!(expr, scope.clone()))))
                },
                L::None => LanternValue::Option(None),
                L::Null => LanternValue::Null,
            };
            Ok(ControlFlow::Continue(val))
        },
        Expr::Val(Ident { name, .. }) => scope.borrow().variable(&name)
            .map(|var| ControlFlow::Continue(var.value))
            .ok_or_else(|| RuntimeError::new(UnknownItem::Variable)),
        Expr::Block(Block { stmts }) => {
            execute(stmts, Rc::new(RefCell::new(Scope::nested(scope.clone(), RuntimeContext::new())))).and_then(|ret_value| match ret_value {
                ReturnType::Return(ret) => Ok(ControlFlow::Continue(ret)),
                ReturnType::None => Ok(ControlFlow::Continue(LanternValue::Null)),
                ret_type => Err(RuntimeError::new(InvalidReturnType(ret_type))),
            })
        },
        Expr::AccessField(AccessField { base, field: Ident { name, .. } }) => {
            eval_or_break!(*base, scope.clone()).field(&name)
                .ok_or_else(|| RuntimeError::new(UnknownItem::Field))
                .map(ControlFlow::Continue)
        },
        Expr::CallMethod(CallMethod { base, fun_call: FunCall { ident: Ident { name, .. }, args } }) => {
            let callee = eval_or_break!(*base, scope.clone());
            let method = callee.method(&name).ok_or_else(|| RuntimeError::new(UnknownItem::Method))?;

            let mut context = RuntimeContext::new();
            context.add_variable(LanternVariable::new("self", callee));
            let scope = Rc::new(RefCell::new(Scope::nested(scope, context)));
            eval_fun(scope, args, method.function)
        },
        Expr::PipeBlock(PipeBlock { base, block: Block { stmts } }) => {
            let base = eval_or_break!(*base, scope.clone());
            let value = match base {
                LanternValue::Option(Some(val))=> LanternValue::Option(Some(Box::new(in_block(*val, stmts, scope.clone())?))),
                LanternValue::Result(Ok(val)) => LanternValue::Result(Ok(Box::new(in_block(*val, stmts, scope.clone())?))),
                LanternValue::Option(None) | LanternValue::Result(Err(_)) => base,
                // TODO: error
                _ => return Err(RuntimeError::new(MismatchedTypes(LanternType::Option(None), base.r#type()))),
            };

            Ok(ControlFlow::Continue(value))
        },
        Expr::CoerceBlock(CoerceBlock { base, block: Block { stmts } }) => {
            let base = eval_or_break!(*base, scope.clone());
            let value = match base {
                LanternValue::Option(None) => in_block(LanternValue::Null, stmts, scope.clone())?,
                LanternValue::Result(Err(err)) => in_block(*err, stmts, scope.clone())?,
                LanternValue::Option(Some(val)) | LanternValue::Result(Ok(val)) => *val,
                // TODO: error
                _ => return Err(RuntimeError::new(MismatchedTypes(LanternType::Option(None), base.r#type()))),
            };

            Ok(ControlFlow::Continue(value))
        },
        Expr::Branch(Branch { base, block }) => {
            let base = eval_or_break!(*base, scope.clone());
            match (base, block) {
                (LanternValue::Option(Some(val)) | LanternValue::Result(Ok(val)), _) => Ok(ControlFlow::Continue(*val)),
                (LanternValue::Option(None), Some(Block { stmts })) => in_block_ret(LanternValue::Null, stmts, scope.clone()).map(ControlFlow::Break),
                (LanternValue::Result(Err(err)), Some(Block { stmts })) => in_block_ret(*err, stmts, scope.clone()).map(ControlFlow::Break),
                (ret @ LanternValue::Option(None) | ret @ LanternValue::Result(Err(_)), None) => Ok(ControlFlow::Break(ReturnType::Return(ret))),
                // TODO: error
                (base, _) => Err(RuntimeError::new(MismatchedTypes(LanternType::Option(None), base.r#type()))),
            }
        },
        Expr::BinaryOperation(BinaryOperation { left, op, right }) => {
            let left = eval_or_break!(*left, scope.clone());
            let right = eval_or_break!(*right, scope.clone());

            perform_binary_op(left, op, right).map(ControlFlow::Continue)
        },
        Expr::UnaryOperation(UnaryOperation { op, expr }) => {
            let value = eval_or_break!(*expr, scope);

            perform_unary_op(op, value).map(ControlFlow::Continue)
        },
    }
}

fn eval_if_branch(branch: IfBranch, scope: ScopeMut) -> Result<ReturnType> {
    match branch {
        IfBranch::Elif(condition, Block { stmts }, branch) => {
            match (eval_or_ret!(condition, scope.clone()), branch) {
                (LanternValue::Bool(bool), _) if bool => execute(stmts, Rc::new(RefCell::new(Scope::nested(scope.clone(), RuntimeContext::new())))),
                (LanternValue::Bool(_), Some(branch)) => eval_if_branch(*branch, scope),
                (LanternValue::Bool(_), None) => Ok(ReturnType::None),
                (value, _) => Err(RuntimeError::new(MismatchedTypes(LanternType::Bool, value.r#type()))),
            }
        },
        IfBranch::Else(Block { stmts }) => execute(stmts, Rc::new(RefCell::new(Scope::nested(scope.clone(), RuntimeContext::new())))),
    }
}

// TODO: check to make sure custom type exists
fn gen_args(args: FunArgs, _scope: ScopeMut) -> Result<Vec<LanternFunctionArg>> {
    args.args.into_iter().map(|arg| {
        Ok(LanternFunctionArg { name: arg.name.name, r#type: arg.r#type })
    }).collect::<Result<Vec<LanternFunctionArg>>>()
}

fn gen_fun(FunDefinition { ident, args, ret, block }: FunDefinition, scope: ScopeMut) -> Result<LanternFunction> {
    let args = gen_args(args, scope.clone())?;

    // TODO: check to make sure custom type exists
    let ret_type = ret.map(Some)
        .unwrap_or(Some(LanternType::Nil))
        .ok_or_else(|| RuntimeError::new(UnknownItem::Type))?;

    Ok(LanternFunction {
        name: ident.name,
        args,
        ret_type,
        body: LanternFunctionBody::Custom(block.stmts),
    })
}

fn eval_fun(scope: ScopeMut, args: Vec<Expr>, function: LanternFunction) -> Result<ControlFlow<ReturnType, LanternValue>> {
    let fun_args = function.args;

    if args.len() != fun_args.len() {
        return Err(runtime_error!("expected {} arg(s), but got {} arg(s) instead", fun_args.len(), args.len()));
    };

    let mut context = RuntimeContext::new();
    for (expr, arg) in args.into_iter().zip(fun_args) {
        let value = eval_or_break!(expr, scope.clone());
        if !value.r#type().applies_to(&arg.r#type) {
            return Err(RuntimeError::new(MismatchedTypes(arg.r#type, value.r#type())));
        };

        context.add_variable(LanternVariable::new(arg.name.to_string(), value));
    }

    // TODO: global scope (functions are not in the scope where they are defined)
    let scope = Rc::new(RefCell::new(Scope::nested(scope.clone(), context)));
    let ret = match function.body {
        LanternFunctionBody::Native(fn_pointer) => fn_pointer(scope),
        LanternFunctionBody::Custom(stmts) => {
            match execute(stmts, scope)? {
                ReturnType::Return(ret) => Ok(ret),
                ReturnType::None => Ok(LanternValue::Null),
                ret_type => Err(runtime_error!("{} not allowed here", ret_type.keyword().expect("return type is not None"))),
            }
        },
    }?;
    if !ret.r#type().applies_to(&function.ret_type) { return Err(RuntimeError::new(MismatchedTypes(function.ret_type, ret.r#type()))); };

    Ok(ControlFlow::Continue(ret))
}

fn in_block(in_var: LanternValue, stmts: Vec<Stmt>, scope: ScopeMut) -> Result<LanternValue> {
    let mut context = RuntimeContext::new();
    context.add_variable(LanternVariable { name: "in".to_string(), r#type: in_var.r#type(), value: in_var });

    match execute(stmts, Rc::new(RefCell::new(Scope::nested(scope.clone(), context))))? {
        ReturnType::Return(ret) => Ok(ret),
        ReturnType::None => Err(RuntimeError::new(ExpectedError("return".to_string()))),
        ret_type => Err(RuntimeError::new(InvalidReturnType(ret_type))),
    }
}

fn in_block_ret(in_var: LanternValue, stmts: Vec<Stmt>, scope: ScopeMut) -> Result<ReturnType> {
    let mut context = RuntimeContext::new();
    context.add_variable(LanternVariable { name: "in".to_string(), r#type: in_var.r#type(), value: in_var });

    match execute(stmts, Rc::new(RefCell::new(Scope::nested(scope.clone(), context))))? {
        ReturnType::None => Err(RuntimeError::new(ExpectedError("return".to_string()))),
        ret_type => Ok(ret_type),
    }
}

