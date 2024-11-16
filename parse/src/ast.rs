use std::{collections::{HashMap, VecDeque}, fmt::{Display, Formatter}};

use expr::Expr;

use crate::{diagnostic, error::{Diagnostics, ExpectedError, Recoverable, Span}, read::{ItemStream, Read, TokenStream}, tokenizer::{Delimiter, DoublePunct, DoublePunctKind, Group, Ident, Keyword, KeywordKind, Punct, PunctKind, Token}, try_ordered};

pub mod expr;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    ValBinding(ValBinding),
    FunDefinition(FunDefinition),
    RecDefinition(RecDefinition),
    If(IfStatement),
    Loop(LoopStatement),
    While(WhileStatement),
    Break,
    Cont,
    Ret(Ret),
    ValAssignment(ValAssignment),
    Expr(Expr),
}

impl Read<TokenStream, Diagnostics> for Stmt {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        try_ordered! {
            val_binding = stream.read(), if ValBinding::can_read_token(stream) => Self::ValBinding(val_binding),
            fun_definition = stream.read(), if FunDefinition::can_read_token(stream) => Self::FunDefinition(fun_definition),
            rec_definition = stream.read(), if RecDefinition::can_read_tokens(stream) => Self::RecDefinition(rec_definition),
            if_statement = stream.read(), if IfStatement::can_read_tokens(stream) => Self::If(if_statement),
            loop_statement = stream.read(), if LoopStatement::can_read_token(stream) => Self::Loop(loop_statement),
            while_statement = stream.read(), if WhileStatement::can_read_tokens(stream) => Self::While(while_statement),
            _ = read_keyword(stream, KeywordKind::Break).map_err(Recoverable::Nonfatal), if => Self::Break,
            _ = read_keyword(stream, KeywordKind::Cont).map_err(Recoverable::Nonfatal), if => Self::Cont,
            ret = stream.read(), if Ret::can_read_token(stream) => Self::Ret(ret),
            val_assignment = stream.read(), if ValAssignment::can_read_tokens(stream) => Self::ValAssignment(val_assignment),
            expr = stream.read() => Self::Expr(expr),
        };
    }
}

// TODO: move to lang after generics
#[derive(Debug, Clone, PartialEq)]
pub enum LanternType {
    String,
    Num,
    Bool,
    Nil,
    Option(Option<Box<LanternType>>),
    Result(Option<Box<LanternType>>, Option<Box<LanternType>>),
    // TODO: path struct
    Custom(String),
    Any,
}

impl Read<TokenStream, Diagnostics> for LanternType {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        let ident: Ident = stream.read()?;

        match ident.name.as_ref() {
            "str" => Ok(Self::String),
            "num" => Ok(Self::Num),
            "bool" => Ok(Self::Bool),
            "nil" => Ok(Self::Nil),
            "any" => Ok(Self::Any),
            "option" => {
                let type_tokens = read_group_delimiter(stream, Delimiter::Paren)?.tokens;
                Ok(Self::Option(Some(Box::new(TokenStream::new(type_tokens).read()?))))
            },
            "result" => {
                let type_group = read_group_delimiter(stream, Delimiter::Paren)?;
                let group_span = type_group.span;
                let mut types = read_delimited(&mut TokenStream::new(type_group.tokens), PunctKind::Comma)?;

                if types.len() != 2 { return Err(diagnostic!(Error, group_span => ExpectedError("ok and err type".to_string())).into()); };

                Ok(Self::Result(Some(Box::new(types.swap_remove(0))), Some(Box::new(types.swap_remove(0)))))
            },
            _ => Ok(Self::Custom(ident.name)),
        }
    }
}

impl LanternType {
    pub fn applies_to(&self, other: &Self) -> bool {
        match (self, other) {
            (_, Self::Any) => true,
            (Self::Option(Some(l)), Self::Option(Some(r))) => l.applies_to(r),
            (Self::Option(_), Self::Option(_)) => true,
            (Self::Result(Some(l_ok), Some(l_err)), Self::Result(Some(r_ok), Some(r_err))) => l_ok.applies_to(r_ok) && l_err.applies_to(r_err),
            (Self::Result(Some(l), _), Self::Result(Some(r), _)) |
                (Self::Result(_, Some(l)), Self::Result(_, Some(r))) => l.applies_to(r),
            (_, _) => self == other,
        }
    }
}

impl Display for LanternType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Self::String => "str",
            Self::Num => "num",
            Self::Bool => "bool",
            Self::Nil => "nil",
            Self::Custom(name) => name,
            Self::Option(Some(inner)) => &format!("option({inner})"),
            Self::Option(None) => "option({unknown})",
            Self::Result(Some(ok), Some(err)) => &format!("result({ok}, {err})"),
            Self::Result(Some(ok), None) => &format!("result({ok}, {{unknown}})"),
            Self::Result(None, Some(err)) => &format!("result({{uknown}}, {err})"),
            Self::Result(None, None) => "result({unknown}, {unknown})",
            Self::Any => "any",
        };
        f.write_str(str)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValBinding {
    pub ident: Ident,
    pub r#type: LanternType,
    pub init: Expr,
}

impl ValBinding {
    pub fn can_read_token(stream: &mut TokenStream) -> bool {
        can_read_keyword(stream, KeywordKind::Val)
    }
}

impl Read<TokenStream, Diagnostics> for ValBinding {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        read_keyword(stream, KeywordKind::Val)?;

        let ident: Ident = stream.read()?;

        read_punct(stream, PunctKind::Colon)
            .map_err(|_| diagnostic!(Error, ident.span => ExpectedError("variable type".to_string())))?;

        let r#type = stream.read()?;

        read_punct(stream, PunctKind::Eq)?;
        let init = stream.read()?;

        Ok(Self { ident, r#type, init })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunDefinition {
    pub ident: Ident,
    pub args: FunArgs,
    pub ret: Option<LanternType>,
    pub block: Block,
}

impl FunDefinition {
    pub fn can_read_token(stream: &mut TokenStream) -> bool {
        can_read_keyword(stream, KeywordKind::Fun)
    }
}

impl Read<TokenStream, Diagnostics> for FunDefinition {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        read_keyword(stream, KeywordKind::Fun)?;

        let ident = stream.read()?;

        let args_tokens = read_group_delimiter(stream, Delimiter::Paren)?.tokens;
        let args = TokenStream::new(args_tokens).read()?;

        let ret = if read_punct(stream, PunctKind::Colon).is_ok() {
            Some(stream.read()?)
        } else { None };

        let block = stream.read()?;

        Ok(Self { ident, args, ret, block })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunArgs {
    pub args: Vec<FunArg>,
}

impl Read<TokenStream, Diagnostics> for FunArgs {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        let args = read_delimited(stream, PunctKind::Comma)?;
        Ok(FunArgs { args })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunArg {
    pub name: Ident,
    pub r#type: LanternType,
}

impl Read<TokenStream, Diagnostics> for FunArg {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        let name = stream.read()?;

        read_punct(stream, PunctKind::Colon)?;

        let r#type = stream.read()?;

        Ok(Self { name, r#type })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecDefinition {
    pub ident: Ident,
    pub fields: FunArgs,
    pub methods: Vec<FunDefinition>,
    pub private_init: bool,
}

impl RecDefinition {
    pub fn can_read_tokens(stream: &mut TokenStream) -> bool {
        can_read_keyword(stream, KeywordKind::Rec)
    }
}

impl Read<TokenStream, Diagnostics> for RecDefinition {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        read_keyword(stream, KeywordKind::Rec)?;

        let ident = stream.read()?;
        let (fields_tokens, private_init) = match stream.read()? {
            Group { delimiter: Delimiter::Paren, tokens, .. } => (tokens, false),
            Group { delimiter: Delimiter::Bracket, tokens, .. } => (tokens, true),
            group => return Err(diagnostic!(Error, group.span => ExpectedError("paren or bracket group".to_string())).into()),
        };
        let fields = TokenStream::new(fields_tokens).read()?;

        let mut methods_stream = TokenStream::new(read_group_delimiter(stream, Delimiter::Brace)?.tokens);
        let mut methods = Vec::new();
        while !methods_stream.is_empty() {
            methods.push(methods_stream.read()?);
            methods_stream.skip_while(|token| matches!(token, Token::Newline(_)));
        }

        Ok(Self { ident, fields, methods, private_init })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub condition: Expr,
    pub block: Block,
    pub branch: Option<Box<IfBranch>>,
}

impl IfStatement {
    pub fn can_read_tokens(stream: &mut TokenStream) -> bool {
        can_read_keyword(stream, KeywordKind::If)
    }
}

impl Read<TokenStream, Diagnostics> for IfStatement {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        read_keyword(stream, KeywordKind::If)?;

        let condition_tokens = read_group_delimiter(stream, Delimiter::Paren)?.tokens;
        let condition = TokenStream::new(condition_tokens).read()?;
        let block = stream.read()?;
        stream.skip_while(|token| matches!(token, Token::Newline(_)));
        let branch = if IfBranch::can_read_token(stream) {
            match stream.read() {
                Ok(branch) => Some(Box::new(branch)),
                Err(err) => return Err(err),
            }
        } else {
            None
        };

        Ok(Self { condition, block, branch })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfBranch {
    Elif(IfStatement),
    Else(Block),
}

impl IfBranch {
    pub fn can_read_token(stream: &mut TokenStream) -> bool {
        can_read_keyword(stream, KeywordKind::Else)
    }
}

impl Read<TokenStream, Diagnostics> for IfBranch {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        read_keyword(stream, KeywordKind::Else)?;

        stream.skip_while(|token| matches!(token, Token::Newline(_)));
        if !IfStatement::can_read_tokens(stream) { return Ok(IfBranch::Else(stream.read()?)); };
        match stream.read() {
            Ok(statement) => Ok(IfBranch::Elif(statement)),
            Err(err) => Err(err),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoopStatement {
    pub block: Block,
}

impl LoopStatement {
    pub fn can_read_token(stream: &mut TokenStream) -> bool {
        can_read_keyword(stream, KeywordKind::Loop)
    }
}

impl Read<TokenStream, Diagnostics> for LoopStatement {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        read_keyword(stream, KeywordKind::Loop)?;

        Ok(Self { block: stream.read()? })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatement {
    pub condition: Expr,
    pub block: Block,
}

impl WhileStatement {
    pub fn can_read_tokens(stream: &mut TokenStream) -> bool {
        can_read_keyword(stream, KeywordKind::While)
    }
}

impl Read<TokenStream, Diagnostics> for WhileStatement {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        read_keyword(stream, KeywordKind::While)?;

        let condition_tokens = read_group_delimiter(stream, Delimiter::Paren)?.tokens;
        let condition = TokenStream::new(condition_tokens).read()?;

        Ok(Self { condition, block: stream.read()? })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ret {
    pub expr: Option<Expr>,
}

impl Ret {
    pub fn can_read_token(stream: &mut TokenStream) -> bool {
        can_read_keyword(stream, KeywordKind::Ret)
    }
}

impl Read<TokenStream, Diagnostics> for Ret {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        read_keyword(stream, KeywordKind::Ret)?;

        let expr = if !(stream.get().is_none() || matches!(stream.get(), Some(Token::Newline(_)))) {
            Some(stream.read()?)
        } else {
            None
        };

        Ok(Self {
            expr
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValAssignment {
    pub ident: Ident,
    pub val: Expr,
}

impl ValAssignment {
    pub fn can_read_tokens(stream: &mut TokenStream) -> bool {
        let res = stream.get().is_some_and(|token| matches!(token, Token::Ident(_))) &&
            stream.peek().is_some_and(|token| matches!(token, Token::Punct(Punct { kind: PunctKind::Eq, .. })));
        stream.reset();

        res
    }
}

impl Read<TokenStream, Diagnostics> for ValAssignment {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        let ident = stream.read()?;

        read_punct(stream, PunctKind::Eq)?;

        Ok(Self { ident, val: stream.read()? })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub file: Ident,
    // TODO: span covers use keyword as well
    pub file_span: Span,
}

impl Module {
    pub fn can_read_tokens(stream: &mut TokenStream) -> bool {
        can_read_keyword(stream, KeywordKind::Mod)
    }
}

impl Read<TokenStream, Diagnostics> for Module {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        read_keyword(stream, KeywordKind::Mod)?;

        let file: Ident = stream.read()?;

        Ok(Self { file_span: file.span, file })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub hoisted_funs: HashMap<String, FunDefinition>,
    pub hoisted_recs: HashMap<String, RecDefinition>,
}

impl Read<TokenStream, Diagnostics> for Block {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        read_group_delimiter(stream, Delimiter::Brace)
            .and_then(|group| to_stmts(TokenStream::new(group.tokens)))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AST {
    pub mods: Vec<Module>,
    pub block: Block,
}

// TODO: duplicate code
pub fn parse(mut stream: TokenStream) -> Result<AST, Diagnostics> {
    let mut stmts = Vec::new();
    let mut hoisted_funs = HashMap::new();
    let mut hoisted_recs = HashMap::new();

    let mut uses = Vec::new();
    let mut past_uses = false;

    loop {
        stream.skip_while(|token| matches!(token, Token::Newline(_)));

        if stream.get().is_none() { break; }

        if !past_uses {
            if Module::can_read_tokens(&mut stream) {
                uses.push(stream.read()?);
                continue;
            } else {
                past_uses = true;
            }
        }

        match Stmt::read(&mut stream) {
            Ok(Stmt::FunDefinition(fun_def)) => {
                if hoisted_funs.contains_key(&fun_def.ident.name) {
                    return Err(diagnostic!(Error, fun_def.ident.span, "function already defined").into());
                };
                hoisted_funs.insert(fun_def.ident.name.clone(), fun_def);
            },
            Ok(Stmt::RecDefinition(rec_def)) => {
                if hoisted_recs.contains_key(&rec_def.ident.name) {
                    return Err(diagnostic!(Error, rec_def.ident.span, "record already defined").into());
                };
                hoisted_recs.insert(rec_def.ident.name.clone(), rec_def);
            },
            Ok(stmt) => stmts.push(stmt),
            Err(err) => return Err(err),
        }
    }

    Ok(AST { mods: uses, block: Block { stmts, hoisted_funs, hoisted_recs } })
}

pub fn to_stmts(mut stream: TokenStream) -> Result<Block, Diagnostics> {
    let mut stmts = Vec::new();
    let mut hoisted_funs = HashMap::new();
    let mut hoisted_recs = HashMap::new();

    loop {
        stream.skip_while(|token| matches!(token, Token::Newline(_)));

        if stream.get().is_none() { break; }

        match Stmt::read(&mut stream) {
            Ok(Stmt::FunDefinition(fun_def)) => {
                if hoisted_funs.contains_key(&fun_def.ident.name) {
                    return Err(diagnostic!(Error, fun_def.ident.span, "function already defined").into());
                };
                hoisted_funs.insert(fun_def.ident.name.clone(), fun_def);
            },
            Ok(Stmt::RecDefinition(rec_def)) => {
                if hoisted_recs.contains_key(&rec_def.ident.name) {
                    return Err(diagnostic!(Error, rec_def.ident.span, "record already defined").into());
                };
                hoisted_recs.insert(rec_def.ident.name.clone(), rec_def);
            },
            Ok(stmt) => stmts.push(stmt),
            Err(err) => return Err(err),
        }
    }

    Ok(Block { stmts, hoisted_funs, hoisted_recs })
}

// TODO: fn can_read_group_delimiter

fn read_group_delimiter(stream: &mut TokenStream, delimiter: Delimiter) -> Result<Group, Diagnostics> {
    let paren_group = stream.get_consume(|token| matches!(token, Token::Group(Group { delimiter: group_delim, .. }) if *group_delim == delimiter))
        .map_err(|token| diagnostic!(Error, *Token::span_of(token) => ExpectedError(format!("{delimiter} group"))))?;

    if let Token::Group(group) = paren_group {
        Ok(group)
    } else {
        unreachable!()
    }
}

fn can_read_keyword(stream: &mut TokenStream, keyword: KeywordKind) -> bool {
    matches!(stream.get(), Some(Token::Keyword(Keyword { kind, .. })) if *kind == keyword)
}

fn read_keyword(stream: &mut TokenStream, keyword: KeywordKind) -> Result<Token, Diagnostics> {
    can_read_keyword(stream, keyword).then(|| stream.consume().expect("can consume"))
        .ok_or_else(|| diagnostic!(Error, *Token::span_of(stream.get()) => ExpectedError(format!("keyword `{keyword}`"))).into())
}

fn can_read_punct(stream: &mut TokenStream, punct: PunctKind) -> bool {
    matches!(stream.get(), Some(Token::Punct(Punct { kind, .. })) if *kind == punct)
}

fn read_punct(stream: &mut TokenStream, punct: PunctKind) -> Result<Token, Diagnostics> {
    can_read_punct(stream, punct).then(|| stream.consume().expect("can consume"))
        .ok_or_else(|| diagnostic!(Error, *Token::span_of(stream.get()) => ExpectedError(format!("punct `{punct}`"))).into())
}

fn can_read_double_punct(stream: &mut TokenStream, punct: DoublePunctKind) -> bool {
    matches!(stream.get(), Some(Token::DoublePunct(DoublePunct { kind, .. })) if *kind == punct)
}

fn read_double_punct(stream: &mut TokenStream, punct: DoublePunctKind) -> Result<Token, Diagnostics> {
    can_read_double_punct(stream, punct).then(|| stream.consume().expect("can consume"))
        .ok_or_else(|| diagnostic!(Error, *Token::span_of(stream.get()) => ExpectedError(format!("punct `{punct}`"))).into())
}

fn read_delimited<T: Read<TokenStream, E>, E>(stream: &mut TokenStream, delimiter: PunctKind) -> Result<Vec<T>, E>
{
    let mut items = Vec::new();

    if stream.is_empty() { return Ok(items); };

    loop {
        let (mut inner_stream, reached) = collect_until(stream, |token| matches!(token, Token::Punct(Punct { kind, .. }) if *kind == delimiter));
        items.push(T::read(&mut inner_stream)?);
        if !reached { break; };
    }

    Ok(items)
}

fn collect_until<F>(stream: &mut TokenStream, mut until: F) -> (TokenStream, bool)
where F: FnMut(&Token) -> bool
{
    let mut tokens = VecDeque::new();

    let reached = loop {
        match stream.get() {
            Some(token) if until(token) => {
                stream.consume();
                break true;
            },
            Some(_) => tokens.push_back(stream.consume().expect("get exists")),
            None => break false,
        }
    };

    (TokenStream::new(tokens), reached)
}

