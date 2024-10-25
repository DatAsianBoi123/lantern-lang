use std::{char, collections::VecDeque, fs::File, ops::ControlFlow};

use crate::{error::{Location, Span}, tokenizer::Token};

pub trait Read<S: ItemStream, E> {
    fn read(stream: &mut S) -> Result<Self, E>
    where Self: Sized;
}

pub trait ItemStream {
    type Item;

    fn cursor(&self) -> usize;

    fn cursor_mut(&mut self) -> &mut usize;

    fn get(&self) -> Option<&Self::Item>;

    fn consume(&mut self) -> Option<Self::Item>;

    fn peek(&mut self) -> Option<&Self::Item>;

    fn reset(&mut self);

    fn remaining(&self) -> usize;

    fn read<R: Read<Self, E>, E>(&mut self) -> Result<R, E> where Self: Sized {
        R::read(self)
    }

    fn skip_while<F>(&mut self, filter: F)
    where F: Fn(&Self::Item) -> bool
    {
        while self.get().is_some_and(&filter) {
            self.consume();
        }
    }

    fn jump(&mut self) {
        let times = self.cursor();
        for _ in 0..times {
            self.consume();
        }
    }

    fn get_consume<F>(&mut self, mut filter: F) -> Result<Self::Item, Option<&Self::Item>>
    where F: FnMut(&Self::Item) -> bool
    {
        self.reset();

        if filter(self.get().ok_or(None)?) {
            Ok(self.consume().expect("next"))
        } else {
            Err(self.get())
        }
    }

    fn get_map_consume<F, M>(&mut self, mut mapper: F) -> Result<M, Option<&Self::Item>>
    where F: FnMut(&Self::Item) -> Option<M>,
    {
        self.reset();

        match mapper(self.get().ok_or(None)?) {
            Some(mapped) => {
                self.consume();
                Ok(mapped)
            },
            _ => Err(self.get()),
        }
    }

    fn peek_try_fold<F, M, B>(&mut self, init: M, mut mapper: F) -> ControlFlow<B, M>
    where F: FnMut(&Self::Item, M) -> ControlFlow<B, M>
    {
        self.reset();

        let mut acc = init;

        while let Some(get) = self.get() {
            match mapper(get, acc) {
                ControlFlow::Continue(new) => {
                    self.consume();
                    acc = new
                },
                ControlFlow::Break(last) => return ControlFlow::Break(last),
            };
        };

        ControlFlow::Continue(acc)
    }

    fn peek_try_fold_accept<F, A, B, R>(&mut self, init: B, mut mapper: F, accept: A) -> Option<R>
    where
        A: FnOnce(B) -> Option<R>,
        F: FnMut(&Self::Item, B) -> ControlFlow<B, B>,
    {
        self.reset();

        let mut acc = init;

        while let Some(get) = self.get() {
            match mapper(get, acc) {
                ControlFlow::Continue(new) => {
                    self.peek();
                    acc = new
                },
                ControlFlow::Break(last) => {
                    acc = last;
                    break;
                },
            };
        };

        let accept = accept(acc);
        if accept.is_some() { self.jump(); }
        else { self.reset(); };
        accept
    }

    fn is_empty(&self) -> bool {
        self.remaining() == 0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileStream {
    internal: VecDeque<char>,
    cursor: usize,

    location: Location,
}

impl FileStream {
    pub fn new(mut file: File) -> Result<Self, std::io::Error> {
        let mut buf = String::new();
        std::io::Read::read_to_string(&mut file, &mut buf)?;

        Ok(Self {
            internal: buf.chars().collect(),
            cursor: 0,

            location: Default::default(),
        })
    }

    pub fn with_chars(chars: impl Into<VecDeque<char>>) -> Self {
        Self {
            internal: chars.into(),
            cursor: 0,

            location: Default::default(),
        }
    }

    pub fn with_string(str: String) -> Self {
        Self {
            internal: str.chars().collect::<VecDeque<char>>(),
            cursor: 0,

            location: Default::default(),
        }
    }

    pub fn location(&self) -> &Location {
        &self.location
    }

    pub fn location_mut(&mut self) -> &mut Location {
        &mut self.location
    }

    pub fn span_since(&self, before: Location) -> Span {
        let after = (!self.is_empty()).then_some(self.location);
        Span::Range(before, after)
    }
}

impl ItemStream for FileStream {
    type Item = char;

    fn cursor(&self) -> usize {
        self.cursor
    }

    fn cursor_mut(&mut self) -> &mut usize {
        &mut self.cursor
    }

    fn get(&self) -> Option<&char> {
        self.internal.get(self.cursor)
    }

    fn consume(&mut self) -> Option<char> {
        self.location.col += 1;
        self.cursor = 0;
        let front = self.internal.pop_front();
        if front.is_some_and(|char| char == '\n') {
            self.location.line += 1;
            self.location.col = 1;
        }

        front
    }

    fn peek(&mut self) -> Option<&char> {
        self.cursor += 1;
        self.get()
    }

    fn reset(&mut self) {
        self.cursor = 0;
    }

    fn remaining(&self) -> usize {
        self.internal.len()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenStream {
    internal: VecDeque<Token>,
    cursor: usize,
}

impl TokenStream {
    pub fn new(tokens: impl Into<VecDeque<Token>>) -> Self {
        Self { internal: tokens.into(), cursor: 0 }
    }
}

impl ItemStream for TokenStream {
    type Item = Token;

    fn cursor(&self) -> usize {
        self.cursor
    }

    fn cursor_mut(&mut self) -> &mut usize {
        &mut self.cursor
    }

    fn get(&self) -> Option<&Token> {
        self.internal.get(self.cursor)
    }

    fn consume(&mut self) -> Option<Token> {
        self.cursor = 0;
        self.internal.pop_front()
    }

    fn peek(&mut self) -> Option<&Token> {
        self.cursor += 1;
        self.get()
    }

    fn reset(&mut self) {
        self.cursor = 0;
    }

    fn remaining(&self) -> usize {
        self.internal.len()
    }
}

