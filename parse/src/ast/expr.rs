use std::{collections::VecDeque, fmt::{Display, Formatter}};

use crate::{ast::read_group_delimiter, diagnostic, error::{Diagnostics, ExpectedError, InvalidTokenError, Location, Recoverable, Span, UnexpectedTokenError}, read::{ItemStream, Read, TokenStream}, tokenizer::{Delimiter, DoublePunct, DoublePunctKind, Group, Ident, Literal, Punct, PunctKind, Token}, try_ordered};

use super::{can_read_double_punct, can_read_punct, read_delimited, read_double_punct, Block};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Group(Box<Expr>),
    Lit(Literal),
    Val(Ident),
    FunCall(FunCall),
    NewRec(NewRec),
    Block(Block),

    AccessField(AccessField),
    CallMethod(CallMethod),

    PipeBlock(PipeBlock),
    CoerceBlock(CoerceBlock),
    Branch(Branch),

    BinaryOperation(BinaryOperation),
    UnaryOperation(UnaryOperation),
}

impl Expr {
    pub fn eval(mut segments: Vec<ExprSegment>) -> Result<Self, Diagnostics> {
        if segments.is_empty() { panic!("cannot eval nothing") }
        if segments.len() == 1 {
            return if let ExprSegment::Tokens(mut stream) = segments.remove(0) {
                let expr = read_single(&mut stream)?;
                if !stream.is_empty() { Err(diagnostic!(Error, Location::new(0, 0) => UnexpectedTokenError).into()) }
                else { Ok(expr) }
            } else {
                Err(diagnostic!(Error, Location::new(0, 0) => UnexpectedTokenError).into())
            }
        }

        let Some((split_idx, op)) = segments.iter()
            .enumerate()
            .filter_map(|(i, segment)| {
                if let ExprSegment::Op(op) = segment {
                    Some((i, op))
                } else {
                    None
                }
            })
            .fold(None, |acc: Option<(usize, Operator)>, (i, curr)| {
                match acc {
                    Some((_, op)) if curr.order() <= op.order() => Some((i, *curr)),
                    None => Some((i, *curr)),
                    _ => acc,
                }
            // TODO: span
            }) else { return Err(diagnostic!(Error, Location::new(0, 0) => ExpectedError("operator".to_string())).into()); };

        let mut right = segments.split_off(split_idx);
        right.remove(0);
        let left = segments;

        match op {
            Operator::Binary(op) => BinaryOperation::from_segments(left, op, right).map(Self::BinaryOperation),
            Operator::Unary(op) => UnaryOperation::from_segment(op, right).map(Self::UnaryOperation),
        }
    }
}

impl Read<TokenStream, Diagnostics> for Expr {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        let mut segments = Vec::new();
        let mut tokens = VecDeque::new();

        let mut prev_is_value = false;
        loop {
            match stream.get() {
                None | Some(Token::Newline(_)) => break,
                _ => {
                    if let Ok(op) = Operator::read(stream, prev_is_value) {
                        prev_is_value = false;

                        if !tokens.is_empty() {
                            segments.push(ExprSegment::Tokens(TokenStream::new(tokens)));
                            tokens = VecDeque::new();
                        }
                        segments.push(ExprSegment::Op(op));
                    } else {
                        prev_is_value = true;

                        tokens.push_back(stream.consume().expect("get exists"));
                    }
                },
            };
        };
        segments.push(ExprSegment::Tokens(TokenStream::new(tokens)));

        Self::eval(segments)
    }
}

fn read_single(stream: &mut TokenStream) -> Result<Expr, Diagnostics> {
    try_ordered! {
        access_field = stream.read(), if AccessField::can_read_tokens(stream) => Expr::AccessField(access_field),
        call_method = stream.read(), if CallMethod::can_read_tokens(stream) => Expr::CallMethod(call_method),
        pipe_block = stream.read(), if PipeBlock::can_read_tokens(stream) => Expr::PipeBlock(pipe_block),
        coerce_block = stream.read(), if CoerceBlock::can_read_tokens(stream) => Expr::CoerceBlock(coerce_block),
        branch = stream.read(), if Branch::can_read_tokens(stream) => Expr::Branch(branch),

        lit = stream.read().map_err(Recoverable::Nonfatal), if => Expr::Lit(lit),
        fun_call = stream.read(), if FunCall::can_read_tokens(stream) => Expr::FunCall(fun_call),
        new_rec = stream.read(), if NewRec::can_read_tokens(stream) => Expr::NewRec(new_rec),
        val = stream.read().map_err(Recoverable::Nonfatal), if => Expr::Val(val),
        group = {
            read_group_delimiter(stream, Delimiter::Paren)
                .map_err(Recoverable::Nonfatal)
                .and_then(|group| TokenStream::new(group.tokens).read().map(Box::new).map_err(Recoverable::Fatal))
        }, if => Expr::Group(group),
        block = stream.read().map_err(Recoverable::Nonfatal), if => Expr::Block(block),
    };

    Err(diagnostic!(Error, *Token::span_of(stream.get()) => UnexpectedTokenError).into())
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunCall {
    pub ident: Ident,
    pub args: Vec<Expr>,
}

impl FunCall {
    pub fn can_read_tokens(stream: &mut TokenStream) -> bool {
        let can = stream.get().is_some_and(|token| matches!(token, Token::Ident(_))) &&
            stream.peek().is_some_and(|token| matches!(token, Token::Group(Group { delimiter: Delimiter::Paren, .. })));
        stream.reset();
        can
    }
}

impl Read<TokenStream, Diagnostics> for FunCall {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        let ident = stream.read()?;
        let args_tokens = read_group_delimiter(stream, Delimiter::Paren)?.tokens;
        let args = read_delimited(&mut TokenStream::new(args_tokens), PunctKind::Comma)?;

        Ok(Self { ident, args })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NewRec {
    pub ident: Ident,
    pub args: Vec<Expr>,
}

impl NewRec {
    pub fn can_read_tokens(stream: &mut TokenStream) -> bool {
        let first_is_ident = stream.get().is_some_and(|token| matches!(token, Token::Ident(_)));
        stream.peek();
        let second_is_doublecolon = can_read_double_punct(stream, DoublePunctKind::Colon);
        stream.reset();
        first_is_ident && second_is_doublecolon
    }
}

impl Read<TokenStream, Diagnostics> for NewRec {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        let ident = stream.read()?;

        read_double_punct(stream, DoublePunctKind::Colon)?;

        let group_tokens = read_group_delimiter(stream, Delimiter::Paren)?.tokens;
        let args = read_delimited(&mut TokenStream::new(group_tokens), PunctKind::Comma)?;

        Ok(Self { ident, args })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AccessField {
    pub base: Box<Expr>,
    pub field: Ident,
}

impl AccessField {
    pub fn can_read_tokens(stream: &mut TokenStream) -> bool {
        if stream.remaining() < 2 { return false; };

        *stream.cursor_mut() = stream.remaining() - 1;

        let last_is_ident = stream.get().is_some_and(|token| matches!(token, Token::Ident(_)));
        *stream.cursor_mut() -= 1;

        let last_two_is_dot = stream.get().is_some_and(|token| matches!(token, Token::Punct(Punct { kind: PunctKind::Period, .. })));

        stream.reset();

        last_is_ident && last_two_is_dot
    }
}

impl Read<TokenStream, Diagnostics> for AccessField {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        // FIX: error type
        if stream.remaining() < 2 { return Err(diagnostic!(Error, Span::Point(None) => UnexpectedTokenError).into()); };

        let dot_idx = stream.remaining() - 2;
        *stream.cursor_mut() = dot_idx;

        let dot = match stream.get() {
            Some(Token::Punct(punct @ Punct { kind: PunctKind::Period, .. })) => punct.clone(),

            get => return Err(diagnostic!(Error, *Token::span_of(get) => ExpectedError("dot operator".to_string())).into()),
        };

        stream.reset();

        let mut left = VecDeque::new();

        for _ in 0..dot_idx {
            left.push_back(stream.consume().expect("consume exists"));
        }
        if left.is_empty() { return Err(diagnostic!(Error, dot.span => ExpectedError("left side of dot operator".to_string())).into()); };

        stream.consume();
        Ok(Self { base: Box::new(TokenStream::new(left).read()?), field: stream.read()? })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallMethod {
    pub base: Box<Expr>,
    pub fun_call: FunCall,
}

impl CallMethod {
    pub fn can_read_tokens(stream: &mut TokenStream) -> bool {
        if stream.remaining() < 3 { return false; };

        *stream.cursor_mut() = stream.remaining() - 1;

        let last_is_group = stream.get().is_some_and(|token| matches!(token, Token::Group(Group { delimiter: Delimiter::Paren, .. })));
        *stream.cursor_mut() -= 1;

        let last_two_is_ident = stream.get().is_some_and(|token| matches!(token, Token::Ident(_)));
        *stream.cursor_mut() -= 1;

        let last_three_is_dot = can_read_punct(stream, PunctKind::Period);

        stream.reset();

        last_is_group && last_two_is_ident && last_three_is_dot
    }
}

impl Read<TokenStream, Diagnostics> for CallMethod {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        // FIX: error type
        if stream.remaining() < 3 { return Err(diagnostic!(Error, Span::Point(None) => UnexpectedTokenError).into()); };

        let dot_idx = stream.remaining() - 3;
        *stream.cursor_mut() = dot_idx;

        let dot = match stream.get() {
            Some(Token::Punct(punct @ Punct { kind: PunctKind::Period, .. })) => punct.clone(),

            get => return Err(diagnostic!(Error, *Token::span_of(get) => ExpectedError("dot operator".to_string())).into()),
        };

        stream.reset();

        let mut left = VecDeque::new();

        for _ in 0..dot_idx {
            left.push_back(stream.consume().expect("consume exists"));
        }
        if left.is_empty() { return Err(diagnostic!(Error, dot.span => ExpectedError("left side of dot operator".to_string())).into()); };

        stream.consume();
        Ok(Self { base: Box::new(TokenStream::new(left).read()?), fun_call: stream.read()? })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Branch {
    pub base: Box<Expr>,
    pub block: Option<Block>,
}

impl Branch {
    pub fn can_read_tokens(stream: &mut TokenStream) -> bool {
        if stream.remaining() < 2 { return false; };

        *stream.cursor_mut() = stream.remaining() - 1;

        if can_read_punct(stream, PunctKind::QuestionMark) {
            stream.reset();
            return true;
        };

        *stream.cursor_mut() -= 1;

        let last_two = can_read_punct(stream, PunctKind::QuestionMark);

        stream.reset();

        last_two
    }
}

impl Read<TokenStream, Diagnostics> for Branch {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        // FIX: error type
        if stream.remaining() < 2 { return Err(diagnostic!(Error, Span::Point(None) => UnexpectedTokenError).into()); };

        let mut branch_idx = stream.remaining() - 1;
        *stream.cursor_mut() = branch_idx;

        if !can_read_punct(stream, PunctKind::QuestionMark) {
            // if the last token isn't a branch, check the 2nd to last token
            branch_idx -= 1;
            *stream.cursor_mut() = branch_idx;
            if !can_read_punct(stream, PunctKind::QuestionMark) {
                return Err(diagnostic!(Error, *Token::span_of(stream.get()) => ExpectedError("branch operator".to_string())).into());
            }
        }

        stream.reset();

        let mut left = VecDeque::new();

        for _ in 0..branch_idx {
            left.push_back(stream.consume().expect("consume exists"));
        }
        if left.is_empty() {
            return Err(diagnostic!(Error, *Token::span_of(stream.get()) => ExpectedError("left side of branch operator".to_string())).into());
        };

        stream.consume();

        let block = if stream.is_empty() {
            None
        } else {
            Some(stream.read()?)
        };

        Ok(Self { base: Box::new(TokenStream::new(left).read()?), block })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct PipeBlock {
    pub base: Box<Expr>,
    pub block: Block,
}

impl PipeBlock {
    pub fn can_read_tokens(stream: &mut TokenStream) -> bool {
        if stream.remaining() < 2 { return false; };

        *stream.cursor_mut() = stream.remaining() - 2;

        let can = can_read_double_punct(stream, DoublePunctKind::PipeBlock);

        stream.reset();

        can
    }
}

impl Read<TokenStream, Diagnostics> for PipeBlock {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        // FIX: error type
        if stream.remaining() < 2 { return Err(diagnostic!(Error, Span::Point(None) => UnexpectedTokenError).into()); };

        let pipe_idx = stream.remaining() - 2;
        *stream.cursor_mut() = pipe_idx;

        if !can_read_double_punct(stream, DoublePunctKind::PipeBlock) {
            return Err(diagnostic!(Error, *Token::span_of(stream.get()) => ExpectedError("pipe block operator".to_string())).into());
        }

        stream.reset();

        let mut left = VecDeque::new();

        for _ in 0..pipe_idx {
            left.push_back(stream.consume().expect("consume exists"));
        }
        if left.is_empty() {
            return Err(diagnostic!(Error, *Token::span_of(stream.get()) => ExpectedError("left side of branch operator".to_string())).into());
        };

        stream.consume();
        Ok(Self { base: Box::new(TokenStream::new(left).read()?), block: stream.read()? })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CoerceBlock {
    pub base: Box<Expr>,
    pub block: Block,
}

impl CoerceBlock {
    pub fn can_read_tokens(stream: &mut TokenStream) -> bool {
        if stream.remaining() < 2 { return false; };

        *stream.cursor_mut() = stream.remaining() - 2;

        let can = can_read_double_punct(stream, DoublePunctKind::CoerceBlock);

        stream.reset();

        can
    }
}

impl Read<TokenStream, Diagnostics> for CoerceBlock {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        // FIX: error type
        if stream.remaining() < 3 { return Err(diagnostic!(Error, Span::Point(None) => UnexpectedTokenError).into()); };

        let pipe_idx = stream.remaining() - 2;
        *stream.cursor_mut() = pipe_idx;

        if !can_read_double_punct(stream, DoublePunctKind::CoerceBlock) {
            return Err(diagnostic!(Error, *Token::span_of(stream.get()) => ExpectedError("coerce block operator".to_string())).into());
        }

        stream.reset();

        let mut left = VecDeque::new();

        for _ in 0..pipe_idx {
            left.push_back(stream.consume().expect("consume exists"));
        }
        stream.consume();

        Ok(Self { base: Box::new(TokenStream::new(left).read()?), block: stream.read()? })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprSegment {
    Tokens(TokenStream),
    Op(Operator),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Operator {
    Binary(BinaryOperator),
    Unary(UnaryOperator),
}

impl Operator {
    pub fn read(stream: &mut TokenStream, last_is_value: bool) -> Result<Self, Diagnostics> {
        if last_is_value {
            Ok(Self::Binary(stream.read()?))
        } else {
            Ok(Self::Unary(stream.read()?))
        }
    }

    pub fn order(&self) -> usize {
        match self {
            Self::Binary(BinaryOperator::And) | Self::Binary(BinaryOperator::Or) => 0,
            Self::Binary(BinaryOperator::Eq) |
                Self::Binary(BinaryOperator::NotEq) |
                Self::Binary(BinaryOperator::LessThan) |
                Self::Binary(BinaryOperator::GreaterThan) |
                Self::Binary(BinaryOperator::LessOrEq) |
                Self::Binary(BinaryOperator::GreaterOrEq) => 1,
            Self::Binary(BinaryOperator::Add) | Self::Binary(BinaryOperator::Sub) => 2,
            Self::Binary(BinaryOperator::Mult) | Self::Binary(BinaryOperator::Div) | Self::Binary(BinaryOperator::Mod) => 3,
            Self::Binary(BinaryOperator::Coerce) | Self::Binary(BinaryOperator::Pipe) => 4,
            Self::Unary(UnaryOperator::Neg) | Self::Unary(UnaryOperator::Not) => 5,
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Binary(op) => op.fmt(f),
            Self::Unary(op) => op.fmt(f),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mult,
    Div,
    Mod,

    And,
    Or,

    Eq,
    NotEq,
    LessThan,
    GreaterThan,
    LessOrEq,
    GreaterOrEq,

    Pipe,
    Coerce,
}

impl Read<TokenStream, Diagnostics> for BinaryOperator {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        let get = stream.get().ok_or_else(|| diagnostic!(Error, Span::Point(None) => ExpectedError("binary operator".to_string())))?;
        let op = match get {
            Token::Punct(Punct { kind: PunctKind::Plus, .. }) => Self::Add,
            Token::Punct(Punct { kind: PunctKind::Dash, .. }) => Self::Sub,
            Token::Punct(Punct { kind: PunctKind::Asterisk, .. }) => Self::Mult,
            Token::Punct(Punct { kind: PunctKind::Slash, .. }) => Self::Div,
            Token::Punct(Punct { kind: PunctKind::Percent, .. }) => Self::Mod,

            Token::Punct(Punct { kind: PunctKind::LessThan, .. }) => Self::LessThan,
            Token::Punct(Punct { kind: PunctKind::GreaterThan, .. }) => Self::GreaterThan,

            Token::DoublePunct(DoublePunct { kind: DoublePunctKind::Ampersand, .. }) => Self::And,
            Token::DoublePunct(DoublePunct { kind: DoublePunctKind::Pipe, .. }) => Self::Or,

            Token::DoublePunct(DoublePunct { kind: DoublePunctKind::Eq, .. }) => Self::Eq,
            Token::DoublePunct(DoublePunct { kind: DoublePunctKind::NotEq, .. }) => Self::NotEq,
            Token::DoublePunct(DoublePunct { kind: DoublePunctKind::LessOrEq, .. }) => Self::LessOrEq,
            Token::DoublePunct(DoublePunct { kind: DoublePunctKind::GreaterOrEq, .. }) => Self::GreaterOrEq,

            Token::Punct(Punct { kind: PunctKind::Pipe, .. }) => Self::Pipe,
            Token::Punct(Punct { kind: PunctKind::Tilde, .. }) => Self::Coerce,

            _ => return Err(diagnostic!(Error, *Token::span_of(stream.get()) => InvalidTokenError("binary operator".to_string())).into()),
        };

        stream.consume();
        Ok(op)
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mult => "*",
            Self::Div => "/",
            Self::Mod => "%",

            Self::And => "&&",
            Self::Or => "||",

            Self::Eq => "==",
            Self::NotEq => "!=",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::LessOrEq => "<=",
            Self::GreaterOrEq => ">=",

            Self::Coerce => "~",
            Self::Pipe => "|",
        };
        f.write_str(str)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnaryOperator {
    Neg,
    Not,
}

impl Read<TokenStream, Diagnostics> for UnaryOperator {
    fn read(stream: &mut TokenStream) -> Result<Self, Diagnostics> {
        let get = stream.get().ok_or_else(|| diagnostic!(Error, Span::Point(None) => ExpectedError("unary operator".to_string())))?;
        let op = match get {
            Token::Punct(Punct { kind: PunctKind::Dash, .. }) => Self::Neg,
            Token::Punct(Punct { kind: PunctKind::Bang, .. }) => Self::Not,

            _ => return Err(diagnostic!(Error, Location::new(0, 0) => ExpectedError("unary operator".to_string())).into()),
        };

        stream.consume();
        Ok(op)
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Self::Neg => "-",
            Self::Not => "!",
        };
        f.write_str(str)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOperation {
    pub left: Box<Expr>,
    pub op: BinaryOperator,
    pub right: Box<Expr>,
}

impl BinaryOperation {
    pub fn new(left: impl Into<Box<Expr>>, op: BinaryOperator, right: impl Into<Box<Expr>>) -> Self {
        Self { left: left.into(), op, right: right.into() }
    }

    pub fn from_segments(left: Vec<ExprSegment>, op: BinaryOperator, right: Vec<ExprSegment>) -> Result<Self, Diagnostics> {
        if right.is_empty() || left.is_empty() {
            // TODO: span
            Err(diagnostic!(Error, Location::new(0, 0) => ExpectedError("expression".to_string())).into())
        } else {
            Ok(Self::new(Expr::eval(left)?, op, Expr::eval(right)?))
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOperation {
    pub op: UnaryOperator,
    pub expr: Box<Expr>
}

impl UnaryOperation {
    pub fn new(op: UnaryOperator, expr: impl Into<Box<Expr>>) -> Self {
        Self { op, expr: expr.into() }
    }

    pub fn from_segment(op: UnaryOperator, segments: Vec<ExprSegment>) -> Result<Self, Diagnostics> {
        if segments.is_empty() {
            // TODO: span
            Err(diagnostic!(Error, Location::new(0, 0) => ExpectedError("expression".to_string())).into())
        } else {
            Ok(Self::new(op, Expr::eval(segments)?))
        }
    }
}

