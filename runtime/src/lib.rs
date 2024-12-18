use std::{cell::RefCell, collections::HashMap, fs::File, ops::ControlFlow, path::Path, rc::Rc};

use lantern_builtin::op::{perform_binary_op, perform_unary_op};
use lantern_lang::{error::{InvalidReturnType, RuntimeError, UnknownItem}, module::LanternModule, record::{LanternMethod, LanternRecordFrame}, runtime_error, scope::{RuntimeContext, Scope}, LanternFunction, LanternFunctionArg, LanternFunctionBody, LanternValue, LanternVariable, ReturnType, ScopeMut};
use lantern_parse::{ast::{expr::{AccessField, BinaryOperation, Branch, CallMethod, CallModuleFunction, CoerceBlock, Expr, FunCall, NewRec, PipeBlock, UnaryOperation}, Block, FunArgs, FunDefinition, IfBranch, IfStatement, LanternType, LoopStatement, RecDefinition, Ret, Stmt, ValAssignment, ValBinding, WhileStatement, AST}, error::{ExpectedError, MismatchedTypesError}, read::{FileStream, ItemStream, TokenStream}, tokenizer::{Ident, Literal}};

pub type Result<T> = std::result::Result<T, RuntimeError>;

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

pub fn run(ast: AST, file_path: impl AsRef<Path>, context: RuntimeContext) -> Result<ScopeMut> {
    let parent = file_path.as_ref().parent().expect("path has a parent").to_owned();
    let mut modules = HashMap::new();

    for r#use in ast.mods {
        let mut path = parent.clone();
        path.push(r#use.file.name.clone() + ".la");
        let file = File::open(path.clone()).map_err(|_| runtime_error!("file `{}` does not exist", r#use.file))?;

        let ast = lantern_parse::tokenizer::tokenize(FileStream::new(file)
            .map_err(|e| RuntimeError::new(format!("error occurred when reading file {}: {e}", r#use.file)))?)
            .and_then(|tokens| lantern_parse::ast::parse(TokenStream::new(tokens)))
            .map_err(|e| RuntimeError::new(format!("error occurred when loading module {}: {e}", r#use.file)))?;

        // BUG: circlular mods causes stack overflow
        let scope = run(ast, path, context.clone())?;
        let module = LanternModule { name: r#use.file.name, scope };
        modules.insert(module.name.clone(), module);
    };

    let head = Scope::Head { modules };
    let scope = Rc::new(RefCell::new(Scope::Context { parent: Rc::new(RefCell::new(head)), context }));
    let module_context = hoisted_scope(ast.block.hoisted_funs.clone(), ast.block.hoisted_recs.clone(), scope.clone());
    match execute(ast.block,scope)? {
        ret @ ReturnType::Break | ret @ ReturnType::Continue => return Err(RuntimeError::new(InvalidReturnType(ret))),
        _ => {},
    };
    module_context
}

pub fn execute(Block { stmts, hoisted_funs, hoisted_recs }: Block, scope: ScopeMut) -> Result<ReturnType<LanternValue>> {
    let scope = hoisted_scope(hoisted_funs, hoisted_recs, scope)?;

    for stmt in stmts {
        match stmt {
            Stmt::Expr(expr) => {
                eval_or_ret!(expr, scope.clone());
            },
            Stmt::ValBinding(ValBinding { ident: Ident { name, .. }, r#type, init }) => {
                let value = eval_or_ret!(init, scope.clone());

                if !value.r#type().applies_to(&r#type) {
                    return Err(RuntimeError::new(MismatchedTypesError { expected: r#type, found: value.r#type() }))
                }

                scope.borrow_mut().add_variable(LanternVariable { name, r#type, value });
            },
            Stmt::If(if_statement) => {
                match eval_if_branch(IfBranch::Elif(if_statement), scope.clone())? {
                    ReturnType::None => {},
                    ret_type => return Ok(ret_type),
                }
            },
            // NOTE: this will never be reached because hoisting removes fun definitions
            Stmt::FunDefinition(fun_definition) => scope.borrow_mut().add_function(gen_fun(fun_definition, scope.clone())?),
            // NOTE: this will never be reached because hoisting removes rec definitions
            Stmt::RecDefinition(rec_definition) => scope.borrow_mut().add_record(Rc::new(gen_rec(rec_definition, scope.clone())?)),
            Stmt::Loop(LoopStatement { block }) => {
                loop {
                    // TODO: dont clone on every loop
                    match execute(block.clone(), scope.clone())? {
                        ReturnType::Return(ret) => return Ok(ReturnType::Return(ret)),
                        ReturnType::Break => break,
                        _ => {},
                    }
                }
            },
            Stmt::While(WhileStatement { condition, block }) => {
                loop {
                    // TODO: clones
                    let condition = match eval_or_ret!(condition.clone(), scope.clone()) {
                        LanternValue::Bool(condition) => condition,
                        value => return Err(RuntimeError::new(MismatchedTypesError { expected: LanternType::Bool, found: value.r#type() })),
                    };

                    if !condition { break; }

                    match execute(block.clone(), scope.clone())? {
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

pub fn eval_expr(expr: Expr, scope: ScopeMut) -> Result<ControlFlow<ReturnType<LanternValue>, LanternValue>> {
    match expr {
        Expr::Group(expr) => eval_expr(*expr, scope),
        Expr::FunCall(FunCall { ident: Ident { name, .. }, args }) => {
            let function = scope.borrow().function(&name).ok_or_else(|| runtime_error!(UnknownItem::Function))?;
            eval_fun(scope.clone(), args, function, RuntimeContext::new())
        },
        Expr::NewRec(NewRec { ident: Ident { name, .. }, args }) => {
            let rec = scope.borrow().record(&name).ok_or_else(|| runtime_error!(UnknownItem::Record))?;
            if rec.private_init { return Err(runtime_error!("cannot init record with private init")); };

            let mut init_args = Vec::with_capacity(args.len());
            for arg in args {
                let value = match eval_expr(arg, scope.clone())? {
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
        Expr::Block(block) => {
            execute(block, scope.clone()).and_then(|ret_value| match ret_value {
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
            eval_fun(scope.clone(), args, method.function, context)
        },
        Expr::CallModuleFunction(CallModuleFunction {
            module: Ident { name, .. },
            fun_call: FunCall { ident: Ident { name: fun_name, .. }, args },
        }) => {
            let module = scope.borrow().module(&name).ok_or_else(|| RuntimeError::new(UnknownItem::Module))?;
            let function = module.scope.borrow().function(&fun_name).ok_or_else(|| RuntimeError::new(UnknownItem::Function))?;
            eval_fun(scope.clone(), args, function, RuntimeContext::new())
        },
        Expr::PipeBlock(PipeBlock { base, block }) => {
            let base = eval_or_break!(*base, scope.clone());
            let value = match base {
                LanternValue::Option(Some(val)) => LanternValue::Option(Some(Box::new(in_block(*val, block, scope.clone())?))),
                LanternValue::Result(Ok(val)) => LanternValue::Result(Ok(Box::new(in_block(*val, block, scope.clone())?))),
                LanternValue::Option(None) | LanternValue::Result(Err(_)) => base,
                _ => return Err(runtime_error!("left side of pipe block operator must be either result or option")),
            };

            Ok(ControlFlow::Continue(value))
        },
        Expr::CoerceBlock(CoerceBlock { base, block }) => {
            let base = eval_or_break!(*base, scope.clone());
            let value = match base {
                LanternValue::Option(None) => in_block(LanternValue::Null, block, scope.clone())?,
                LanternValue::Result(Err(err)) => in_block(*err, block, scope.clone())?,
                LanternValue::Option(Some(val)) | LanternValue::Result(Ok(val)) => *val,
                _ => return Err(runtime_error!("left side of coerce block operator must be either result or option")),
            };

            Ok(ControlFlow::Continue(value))
        },
        Expr::Branch(Branch { base, block }) => {
            let base = eval_or_break!(*base, scope.clone());
            match (base, block) {
                (LanternValue::Option(Some(val)) | LanternValue::Result(Ok(val)), _) => Ok(ControlFlow::Continue(*val)),
                (LanternValue::Option(None), Some(block)) => in_block_ret(LanternValue::Null, block, scope.clone()).map(ControlFlow::Break),
                (LanternValue::Result(Err(err)), Some(block)) => in_block_ret(*err, block, scope.clone()).map(ControlFlow::Break),
                (ret @ LanternValue::Option(None) | ret @ LanternValue::Result(Err(_)), None) => Ok(ControlFlow::Break(ReturnType::Return(ret))),
                _ => Err(runtime_error!("left side of branch operator must be either result or option")),
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

fn eval_if_branch(branch: IfBranch, scope: ScopeMut) -> Result<ReturnType<LanternValue>> {
    match branch {
        IfBranch::Elif(IfStatement { condition, block, branch }) => {
            match (eval_or_ret!(condition, scope.clone()), branch) {
                (LanternValue::Bool(bool), _) if bool => execute(block, scope),
                (LanternValue::Bool(_), Some(branch)) => eval_if_branch(*branch, scope),
                (LanternValue::Bool(_), None) => Ok(ReturnType::None),
                (value, _) => Err(RuntimeError::new(MismatchedTypesError { expected: LanternType::Bool, found: value.r#type() })),
            }
        },
        IfBranch::Else(block) => execute(block, scope),
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
    Ok(LanternFunction {
        name: ident.name,
        args,
        ret_type: ret,
        body: LanternFunctionBody::Custom(block),
        scope: scope.clone(),
    })
}

fn gen_rec(RecDefinition { ident, fields, methods, private_init }: RecDefinition, scope: ScopeMut) -> Result<LanternRecordFrame> {
    let methods = methods.into_iter()
        .map(|fun_definition| gen_fun(fun_definition, scope.clone()).map(|function| LanternMethod { function }))
        .collect::<Result<Vec<LanternMethod>>>()?;
    let rec_frame = LanternRecordFrame { ident, fields: gen_args(fields, scope.clone())?, methods, private_init };

    Ok(rec_frame)
}

fn eval_fun(
    args_scope: ScopeMut,
    args: Vec<Expr>,
    function: LanternFunction,
    mut execute_context: RuntimeContext,
) -> Result<ControlFlow<ReturnType<LanternValue>, LanternValue>> {
    let fun_args = function.args;

    if args.len() != fun_args.len() {
        return Err(runtime_error!("expected {} arg(s), but got {} arg(s) instead", fun_args.len(), args.len()));
    };

    for (expr, arg) in args.into_iter().zip(fun_args) {
        let value = eval_or_break!(expr, args_scope.clone());
        if !value.r#type().applies_to(&arg.r#type) {
            return Err(RuntimeError::new(MismatchedTypesError { expected: arg.r#type, found: value.r#type() }));
        };

        execute_context.add_variable(LanternVariable::new(arg.name.to_string(), value));
    }

    let scope = Rc::new(RefCell::new(Scope::nested(function.scope, execute_context)));
    let ret = match function.body {
        LanternFunctionBody::Native(fn_pointer) => fn_pointer(scope),
        LanternFunctionBody::Custom(block) => {
            // HACK: this creates double nested scopes
            match execute(block, scope)? {
                ReturnType::Return(ret) => Ok(ret),
                ReturnType::None => Ok(LanternValue::Null),
                ret_type => Err(runtime_error!("{} not allowed here", ret_type.keyword().expect("return type is not None"))),
            }
        },
    }?;
    if !ret.r#type().applies_to(&function.ret_type) {
        return Err(RuntimeError::new(MismatchedTypesError { expected: function.ret_type, found: ret.r#type() }));
    };

    Ok(ControlFlow::Continue(ret))
}

fn hoisted_scope(
    hoisted_funs: HashMap<String, FunDefinition>,
    hoisted_recs: HashMap<String, RecDefinition>,
    scope: ScopeMut,
) -> Result<ScopeMut> {
    let scope = Rc::new(RefCell::new(Scope::nested(scope.clone(), RuntimeContext::new())));
    let scope_cloned = scope.clone();
    let mut borrow = scope_cloned.borrow_mut();
    for (_, fun) in hoisted_funs {
        borrow.add_function(gen_fun(fun, scope.clone())?);
    };
    for (_, rec) in hoisted_recs {
        borrow.add_record(Rc::new(gen_rec(rec, scope.clone())?));
    };
    Ok(scope)
}

fn in_block(in_var: LanternValue, block: Block, scope: ScopeMut) -> Result<LanternValue> {
    let mut context = RuntimeContext::new();
    context.add_variable(LanternVariable { name: "in".to_string(), r#type: in_var.r#type(), value: in_var });

    // HACK: this creates a double nested scope
    match execute(block, Rc::new(RefCell::new(Scope::nested(scope.clone(), context))))? {
        ReturnType::Return(ret) => Ok(ret),
        ReturnType::None => Err(RuntimeError::new(ExpectedError("return".to_string()))),
        ret_type => Err(RuntimeError::new(InvalidReturnType(ret_type))),
    }
}

fn in_block_ret(in_var: LanternValue, block: Block, scope: ScopeMut) -> Result<ReturnType<LanternValue>> {
    let mut context = RuntimeContext::new();
    context.add_variable(LanternVariable { name: "in".to_string(), r#type: in_var.r#type(), value: in_var });

    // HACK: this creates a double nested scope
    match execute(block, Rc::new(RefCell::new(Scope::nested(scope.clone(), context))))? {
        ReturnType::None => Err(RuntimeError::new(ExpectedError("return".to_string()))),
        ret_type => Ok(ret_type),
    }
}

