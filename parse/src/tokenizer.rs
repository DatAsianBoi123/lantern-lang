use crate::{diagnostic, error::{Diagnostics, ExpectedError, InvalidTokenError, Recoverable, Span, UnexpectedTokenError}, read::{FileStream, ItemStream, Read}, try_ordered};
use std::{collections::VecDeque, fmt::{Display, Formatter, Write}, ops::ControlFlow, str::FromStr};

macro_rules! impl_read_token {
    ($m: pat_param | $pat: pat in $expr: expr => $ty: ty : $name: literal) => {
        impl Read<$crate::read::TokenStream, Diagnostics> for $ty {
            fn read(stream: &mut $crate::read::TokenStream) -> Result<Self, Diagnostics> {
                let lit = stream.get_consume(|token| matches!(token, $m))
                    .map_err(|token| {
                        let name = $name;
                        diagnostic!(Error, *Token::span_of(token) => ExpectedError(name.to_string()))
                    })?;

                match lit {
                    $pat => Ok($expr),
                    _ => unreachable!(),
                }
            }
        }
    };
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Group(Group),
    Ident(Ident),
    Keyword(Keyword),
    Literal(Literal),
    DoublePunct(DoublePunct),
    Punct(Punct),
    Newline(Span),
}

impl Token {
    pub fn span_of(some: Option<&Token>) -> &Span {
        some.map(Token::span).unwrap_or(&Span::Point(None))
    }

    pub fn span(&self) -> &Span {
        match self {
            Self::Group(Group { span, .. }) => span,
            Self::Ident(Ident { span, .. }) => span,
            Self::Keyword(Keyword { span, .. }) => span,
            Self::Literal(Literal { span, .. }) => span,
            Self::DoublePunct(DoublePunct { span, .. }) => span,
            Self::Punct(Punct { span, .. }) => span,
            Self::Newline(span) => span,
        }
    }
}

impl Read<FileStream, Diagnostics> for Token {
    fn read(stream: &mut FileStream) -> Result<Self, Diagnostics>
    {
        try_ordered! {
            group = stream.read(), if Group::can_read_char(stream) => Self::Group(group),
            double_punct = stream.read().map_err(Recoverable::Nonfatal), if => Self::DoublePunct(double_punct),
            punct = stream.read().map_err(Recoverable::Nonfatal), if => Self::Punct(punct),
            lit = stream.read(), if => Self::Literal(lit),
            keyword = stream.read().map_err(Recoverable::Nonfatal), if => Self::Keyword(keyword),
            ident = stream.read().map_err(Recoverable::Nonfatal), if => Self::Ident(ident),
        };

        Err(diagnostic!(Error, *stream.location() => UnexpectedTokenError).into())
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Group(Group { delimiter, .. }) => write!(f, "{delimiter} group"),
            Self::Ident(_) => write!(f, "identifier"),
            Self::Keyword(Keyword { kind, .. }) => write!(f, "`{kind}`"),
            Self::Literal(_) => write!(f, "literal"),
            Self::DoublePunct(DoublePunct { kind, .. }) => write!(f, "{kind}"),
            Self::Punct(Punct { kind, .. }) => write!(f, "{kind}"),
            Self::Newline(_) => write!(f, "<newline>"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Group {
    pub delimiter: Delimiter,
    pub tokens: Vec<Token>,
    pub span: Span,
}

impl Group {
    pub fn can_read_char(stream: &mut FileStream) -> bool {
        stream.get().is_some_and(|char| Delimiter::from_open(*char).is_some())
    }
}

impl Read<FileStream, Diagnostics> for Group {
    fn read(stream: &mut FileStream) -> Result<Self, Diagnostics>
    {
        let get = *stream.get().ok_or(diagnostic!(Error, Span::Point(None) => ExpectedError("open delimiter".to_string())))?;

        let before = *stream.location();
        if let Some(delimiter) = Delimiter::from_open(get) {
            stream.consume();
            stream.skip_while(|char| char == &'\n' || char == &'\r');

            let mut chars = VecDeque::new();
            let mut opens = 1;
            let location = *stream.location();

            let reached = loop {
                match stream.get() {
                    Some(char) if *char == delimiter.closing() => {
                        opens -= 1;
                        if opens == 0 { break true; };
                        chars.push_back(*char);
                        stream.consume();
                    },
                    Some(char) => {
                        if *char == delimiter.opening() { opens += 1; };

                        chars.push_back(*char);
                        stream.consume();
                    },
                    None => break false,
                };
            };

            let mut inner_stream = FileStream::with_chars(chars);
            *inner_stream.location_mut() = location;
    
            if !reached {
                return Err(diagnostic!(Error, *stream.location() => ExpectedError("closing delimiter".to_string())).into());
            };
            stream.consume();

            Ok(Self {
                delimiter,
                tokens: tokenize(inner_stream)?,
                span: stream.span_since(before)
            })
        } else {
            Err(diagnostic!(Error, *stream.location() => InvalidTokenError("open delimiter".to_string())).into())
        }
    }
}

impl_read_token!(Token::Group(_) | Token::Group(group) in group => Group: "group");

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delimiter {
    Paren,
    Brace,
    Bracket,
}

impl Display for Delimiter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Paren => f.write_str("parenthesis"),
            Self::Brace => f.write_str("brace"),
            Self::Bracket => f.write_str("bracket"),
        }
    }
}

impl Delimiter {
    pub fn from_open(value: char) -> Option<Self> {
        match value {
            '(' => Some(Self::Paren),
            '{' => Some(Self::Brace),
            '[' => Some(Self::Bracket),
            _ => None,
        }
    }
}

impl Delimiter {
    pub fn opening(&self) -> char {
        match self {
            Self::Paren => '(',
            Self::Brace => '{',
            Self::Bracket => '[',
        }
    }

    pub fn closing(&self) -> char {
        match self {
            Self::Paren => ')',
            Self::Brace => '}',
            Self::Bracket => ']',
        }
    }
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

impl Eq for Ident { }

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

impl Read<FileStream, Diagnostics> for Ident {
    fn read(stream: &mut FileStream) -> Result<Self, Diagnostics> {
        let get = match stream.get() {
            Some(get) if is_valid_ident(get) => get,
            _ => return Err(diagnostic!(Error, *stream.location() => InvalidTokenError("ident character".to_string())).into()),
        };

        let before = *stream.location();
        let mut name = get.to_string();

        let span = loop {
            match stream.peek() {
                Some(char) if is_valid_ident(char) => {
                    name.push(*char);
                    stream.consume();
                },
                Some(_) => break stream.span_since(before),
                None => break Span::Range(before, None),
            }
        };
        stream.consume();

        Ok(Self { name, span })
    }
}

impl_read_token!(Token::Ident(_) | Token::Ident(ident) in ident => Ident : "ident");

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Keyword {
    pub kind: KeywordKind,
    pub span: Span,
}

impl Keyword {
    pub fn new(kind: KeywordKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl Read<FileStream, Diagnostics> for Keyword {
    fn read(stream: &mut FileStream) -> Result<Self, Diagnostics> {
        let before = *stream.location();
        let kind = stream.read()?;
        
        Ok(Self { kind, span: stream.span_since(before) })
    }
}

impl_read_token!(Token::Keyword(_) | Token::Keyword(keyword) in keyword => Keyword: "keyword");

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum KeywordKind {
    Val,
    Fun,
    If,
    Else,
    Loop,
    While,
    Break,
    Cont,
    Ret,
    Rec,
}

impl Read<FileStream, Diagnostics> for KeywordKind {
    fn read(stream: &mut FileStream) -> Result<Self, Diagnostics> {
        let before = *stream.location();
        stream.peek_try_fold_accept(String::new(), |char, mut str| {
            if char.is_whitespace() {
                ControlFlow::Break(str)
            } else {
                str.push(*char);
                ControlFlow::Continue(str)
            }
        }, |str| str.parse().ok())
            .ok_or_else(|| diagnostic!(Error, stream.span_since(before) => InvalidTokenError("keyword".to_string())).into())
    }
}

impl Display for KeywordKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Self::Val => "val",
            Self::Fun => "fun",
            Self::If => "if",
            Self::Else => "else",
            Self::Loop => "loop",
            Self::While => "while",
            Self::Break => "break",
            Self::Cont => "cont",
            Self::Ret => "ret",
            Self::Rec => "rec",
        };
        f.write_str(name)
    }
}

impl FromStr for KeywordKind {
    type Err = ();

    fn from_str(str: &str) -> Result<Self, Self::Err> {
        match str {
            "val" => Ok(Self::Val),
            "fun" => Ok(Self::Fun),
            "if" => Ok(Self::If),
            "else" => Ok(Self::Else),
            "loop" => Ok(Self::Loop),
            "while" => Ok(Self::While),
            "break" => Ok(Self::Break),
            "cont" => Ok(Self::Cont),
            "ret" => Ok(Self::Ret),
            "rec" => Ok(Self::Rec),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
    pub kind: LiteralKind,
    pub span: Span,
}

impl Literal {
    pub fn new(kind: LiteralKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl Read<FileStream, Recoverable<Diagnostics>> for Literal {
    fn read(stream: &mut FileStream) -> Result<Self, Recoverable<Diagnostics>> {
        let before = *stream.location();
        let kind = stream.read()?;
        Ok(Self { kind, span: stream.span_since(before) })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
    String(String),
    Num(f64),
    Bool(bool),
    // TODO: find a better way to do this
    Some(Vec<Token>),
    None,
    Ok(Vec<Token>),
    Err(Vec<Token>),
    Null,
}

impl Read<FileStream, Recoverable<Diagnostics>> for LiteralKind {
    fn read(stream: &mut FileStream) -> Result<Self, Recoverable<Diagnostics>> {
        if let Ok((init, decimal)) = stream.get_map_consume(|char| {
            char.to_digit(10)
                .map(|digit| (digit, None))
                .or((*char == '.').then_some((0, Some(0u32))))
        }) {
            let num = stream.peek_try_fold((init, decimal, 0), |char, (num, decimal, decimal_digits)| {
                if let Some(decimal) = decimal {
                    let Some(digit) = char.to_digit(10) else { return ControlFlow::Break((num, decimal, decimal_digits)); };
                    ControlFlow::Continue((num, Some(decimal * 10 + digit), decimal_digits + 1))
                } else if *char == '.' {
                    ControlFlow::Continue((num, Some(0), 0))
                } else {
                    let Some(digit) = char.to_digit(10) else { return ControlFlow::Break((num, 0, 0)); };
                    ControlFlow::Continue((num * 10 + digit, None, 0))
                }
            });

            let num = match num {
                ControlFlow::Continue((num, None, _)) => num as f64,

                ControlFlow::Continue((num, Some(decimal), decimal_digits)) |
                ControlFlow::Break((num, decimal, decimal_digits)) 
                    => num as f64 + decimal as f64 / 10u32.pow(decimal_digits) as f64,
            };

            return Ok(LiteralKind::Num(num));
        };

        if let Ok(quote) = stream.get_consume(|char| matches!(char, '\'' | '"')) {
            let str = stream.peek_try_fold(String::new(), |char, mut str| {
                if *char != quote {
                    str.push(*char);
                    ControlFlow::Continue(str)
                } else {
                    ControlFlow::Break(str)
                }
            });

            let str = match str {
                ControlFlow::Continue(_) => {
                    return Err(Recoverable::Fatal(diagnostic!(Error, *stream.location() => ExpectedError("ending quotes".to_string())).into()))
                },
                ControlFlow::Break(str) => str,
            };

            // skip end quote
            stream.consume();
            return Ok(LiteralKind::String(str));
        }

        let mut keyword_lit = String::new();
        while let Some(char) = stream.get() {
            if !is_valid_ident(char) { break; };

            keyword_lit.push(*char);
            stream.peek();
        };
        let keyword_lit = match keyword_lit.as_ref() {
            "true" => Some(Ok(LiteralKind::Bool(true))),
            "false" => Some(Ok(LiteralKind::Bool(false))),
            "null" => Some(Ok(LiteralKind::Null)),
            "some" | "ok" | "err" => {
                stream.jump();
                let group: Group = stream.read().map_err(Recoverable::Fatal)?;
                if group.delimiter != Delimiter::Paren {
                    return Err(Recoverable::Fatal(diagnostic!(Error, group.span => ExpectedError("parenthesis after some literal".to_string())).into()));
                };

                let kind = match keyword_lit.as_ref() {
                    "some" => LiteralKind::Some(group.tokens),
                    "ok" => LiteralKind::Ok(group.tokens),
                    "err" => LiteralKind::Err(group.tokens),
                    _ => unreachable!(),
                };
                Some(Ok(kind))
            },
            "none" => Some(Ok(LiteralKind::None)),
            _ => None,
        };

        if let Some(lit) = keyword_lit {
            stream.jump();
            return lit;
        };

        stream.reset();
        Err(Recoverable::Nonfatal(diagnostic!(Error, *stream.location(), InvalidTokenError("literal".to_string())).into()))
    }
}

impl_read_token!(Token::Literal(_) | Token::Literal(lit) in lit => Literal: "literal");

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Punct {
    pub kind: PunctKind,
    pub span: Span,
}

impl Read<FileStream, Diagnostics> for Punct {
    fn read(stream: &mut FileStream) -> Result<Self, Diagnostics> {
        let before = *stream.location();
        let kind = stream.read()?;

        Ok(Self { kind, span: stream.span_since(before) })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum PunctKind {
    Eq,
    Plus,
    Dash,
    Asterisk,
    Slash,
    Percent,

    LessThan,
    GreaterThan,

    Comma,
    Period,
    Bang,

    Colon,

    Pipe,
    Tilde,
    QuestionMark,
}

impl Read<FileStream, Diagnostics> for PunctKind {
    fn read(stream: &mut FileStream) -> Result<Self, Diagnostics> {
        use PunctKind as S;

        let location = *stream.location();
        stream.get_map_consume(|char| {
            match char {
                '=' => Some(S::Eq),
                '+' => Some(S::Plus),
                '-' => Some(S::Dash),
                '*' => Some(S::Asterisk),
                '/' => Some(S::Slash),
                '%' => Some(S::Percent),

                '<' => Some(S::LessThan),
                '>' => Some(S::GreaterThan),

                ',' => Some(S::Comma),
                '.' => Some(S::Period),
                '!' => Some(S::Bang),

                ':' => Some(S::Colon),

                '|' => Some(S::Pipe),
                '~' => Some(S::Tilde),
                '?' => Some(S::QuestionMark),

                _ => None,
            }
        }).map_err(|_| diagnostic!(Error, location => InvalidTokenError("punct".to_string())).into())
    }
}

impl Display for PunctKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let punct = match self {
            Self::Eq => '=',
            Self::Plus => '+',
            Self::Dash => '-',
            Self::Asterisk => '*',
            Self::Slash => '/',
            Self::Percent => '%',

            Self::LessThan => '<',
            Self::GreaterThan => '>',

            Self::Comma => ',',
            Self::Period => '.',
            Self::Bang => '!',

            Self::Colon => ':',

            Self::Pipe => '|',
            Self::Tilde => '~',
            Self::QuestionMark => '?',
        };
        f.write_char(punct)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DoublePunct {
    pub kind: DoublePunctKind,
    pub span: Span,
}

impl Read<FileStream, Diagnostics> for DoublePunct {
    fn read(stream: &mut FileStream) -> Result<Self, Diagnostics> {
        let before = *stream.location();
        let kind = stream.read()?;

        Ok(Self { kind, span: stream.span_since(before) })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DoublePunctKind {
    Eq,
    NotEq,
    LessOrEq,
    GreaterOrEq,

    Plus,
    Dash,

    Colon,

    Ampersand,
    Pipe,

    PipeBlock,
    CoerceBlock,
}

impl FromStr for DoublePunctKind {
    type Err = ParseDoublePunctError;

    fn from_str(str: &str) -> Result<Self, ParseDoublePunctError> {
        match str {
            "==" => Ok(Self::Eq),
            "!=" => Ok(Self::NotEq),
            "<=" => Ok(Self::LessOrEq),
            ">=" => Ok(Self::GreaterOrEq),

            "++" => Ok(Self::Plus),
            "--" => Ok(Self::Dash),

            "::" => Ok(Self::Colon),

            "&&" => Ok(Self::Ampersand),
            "||" => Ok(Self::Pipe),

            "|?" => Ok(Self::PipeBlock),
            "~?" => Ok(Self::CoerceBlock),

            _ => Err(ParseDoublePunctError),
        }
    }
}

#[derive(thiserror::Error, Debug, Clone, Copy)]
#[error("could not parse into a DoublePunct")]
pub struct ParseDoublePunctError;

impl Read<FileStream, Diagnostics> for DoublePunctKind {
    fn read(stream: &mut FileStream) -> Result<Self, Diagnostics> {
        let curr = stream.get().copied();
        let before = *stream.location();
        match (curr, stream.peek()) {
            (Some(first), Some(second)) => {
                let mut str = first.to_string();
                str.push(*second);
                let punct = str.parse()
                    .map_err(|_| diagnostic!(Error, stream.span_since(before) => InvalidTokenError("double punct".to_string())))?;
                stream.consume();
                stream.consume();
                Ok(punct)
            },
            _ => Err(diagnostic!(Error, stream.span_since(before) => ExpectedError("double puct".to_string())).into()),
        }
    }
}

impl Display for DoublePunctKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Self::Eq => "==",
            Self::NotEq => "!=",
            Self::LessOrEq => "<=",
            Self::GreaterOrEq => ">=",

            Self::Plus => "++",
            Self::Dash => "--",

            Self::Colon => "::",

            Self::Ampersand => "&&",
            Self::Pipe => "||",

            Self::PipeBlock => "|?",
            Self::CoerceBlock => "~?",
        };
        f.write_str(str)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Comment {
    pub text: String,
    pub span: Span,
}

impl Comment {
    pub fn can_read_char(stream: &mut FileStream) -> bool {
        let can = stream.get().is_some_and(|char| *char == '/') &&
            stream.peek().is_some_and(|char| *char == '/');

        can
    }
}

impl Read<FileStream, Diagnostics> for Comment {
    fn read(stream: &mut FileStream) -> Result<Self, Diagnostics> {
        let before = *stream.location();
        match (stream.consume(), stream.consume()) {
            (Some('/'), Some('/')) => {},
            _ => return Err(diagnostic!(Error, stream.span_since(before) => ExpectedError("token //".to_string())).into()),
        };

        let mut text = String::new();
        loop {
            match stream.get() {
                Some('\n') | Some('\r') | None => break,
                Some(char) => {
                    text.push(*char);
                    stream.consume();
                },
            };
        };

        Ok(Self { text, span: stream.span_since(before) })
    }
}

pub fn tokenize(mut stream: FileStream) -> Result<Vec<Token>, Diagnostics> {
    let mut tokens = Vec::new();

    loop {
        while let Some(char) = stream.get() {
            if char.is_whitespace() {
                if *char == '\n' { tokens.push(Token::Newline(Span::Point(Some(*stream.location())))); };
                stream.consume();
            } else { break; }
        }

        if stream.get().is_none() { break false; }

        // TODO: move this somewhere else
        if Comment::can_read_char(&mut stream) {
            stream.read::<Comment, Diagnostics>()?;
            continue;
        }
        match Token::read(&mut stream) {
            Ok(token) => tokens.push(token),
            Err(err) => return Err(err),
        }
    };

    Ok(tokens)
}

fn is_valid_ident(char: &char) -> bool {
    char == &'_' || char.is_ascii_alphanumeric()
}

