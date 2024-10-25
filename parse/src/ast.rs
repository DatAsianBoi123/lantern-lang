use std::{collections::VecDeque, fmt::{Display, Formatter}};

use expr::Expr;

use crate::{diagnostic, error::{Diagnostics, ExpectedError, Recoverable}, read::{ItemStream, Read, TokenStream}, tokenizer::{Delimiter, DoublePunct, DoublePunctKind, Group, Ident, Keyword, KeywordKind, Punct, PunctKind, Token}, try_ordered};

pub mod expr;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    ValBinding(ValBinding),
    FunDefinition(FunDefinition),
    RecDefinition(RecDefinition),
    // TODO: if expression
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

#[derive(Debug, Clone, PartialEq)]
pub enum LanternType {
    String,
    Num,
    Bool,
    Nil,
    // TODO: path struct
    Custom(String),
    Option(Option<Box<LanternType>>),
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
                let type_tokens = read_group_delimiter(stream, Delimiter::Paren)?;
                Ok(Self::Option(Some(Box::new(TokenStream::new(type_tokens).read()?))))
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

        let args_tokens = read_group_delimiter(stream, Delimiter::Paren)?;
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
        let fields_tokens = read_group_delimiter(stream, Delimiter::Paren)?;
        let fields = TokenStream::new(fields_tokens).read()?;

        let mut methods_stream = TokenStream::new(read_group_delimiter(stream, Delimiter::Brace)?);
        let mut methods = Vec::new();
        while !methods_stream.is_empty() {
            methods.push(methods_stream.read()?);
            methods_stream.skip_while(|token| matches!(token, Token::Newline(_)));
        }

        Ok(Self { ident, fields, methods })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub condition: Expr,
    pub block: Block,
    pub branch: Option<IfBranch>,
}

impl IfStatement {
    pub fn can_read_tokens(stream: &mut TokenStream) -> bool {
        can_read_keyword(stream, KeywordKind::If)
    }
}

impl Read<TokenStream, Diagnostics> for IfStatement {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        read_keyword(stream, KeywordKind::If)?;

        let condition_tokens = read_group_delimiter(stream, Delimiter::Paren)?;
        let condition = TokenStream::new(condition_tokens).read()?;
        let block = stream.read()?;
        stream.skip_while(|token| matches!(token, Token::Newline(_)));
        let branch = if IfBranch::can_read_token(stream) {
            match stream.read() {
                Ok(branch) => Some(branch),
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
    Elif(Expr, Block, Option<Box<IfBranch>>),
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
            Ok(IfStatement { condition, block, branch }) => Ok(IfBranch::Elif(condition, block, branch.map(Box::new))),
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

        let condition_tokens = read_group_delimiter(stream, Delimiter::Paren)?;
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

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

impl Read<TokenStream, Diagnostics> for Block {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        read_group_delimiter(stream, Delimiter::Brace)
            .and_then(|tokens| to_stmts(TokenStream::new(tokens)).map(|stmts| Self { stmts }))
    }
}

// TODO: fn can_read_group_delimiter

fn read_group_delimiter(stream: &mut TokenStream, delimiter: Delimiter) -> Result<Vec<Token>, Diagnostics> {
    let paren_group = stream.get_consume(|token| matches!(token, Token::Group(Group { delimiter: group_delim, .. }) if *group_delim == delimiter))
        .map_err(|token| diagnostic!(Error, *Token::span_of(token) => ExpectedError(format!("{delimiter} group"))))?;

    if let Token::Group(Group { tokens, .. }) = paren_group {
        Ok(tokens)
    } else {
        unreachable!()
    }
}

pub fn to_stmts(mut stream: TokenStream) -> Result<Vec<Stmt>, Diagnostics> {
    let mut stmts = Vec::new();

    loop {
        stream.skip_while(|token| matches!(token, Token::Newline(_)));

        if stream.get().is_none() { break; }

        match Stmt::read(&mut stream) {
            Ok(stmt) => stmts.push(stmt),
            Err(err) => return Err(err),
        }
    }

    Ok(stmts)
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

