use super::TokenStream;
use std::cell::RefCell;
use std::rc::Rc;

pub struct Pos {
    pub offset: usize,
    pub line: usize,
    pub row: usize,
}

pub struct Span {
    pub from: Pos,
    pub to: Pos,
}

// Trying again with LL(1) PEG. The other parsers are too slow.
pub struct Parser<Err, T, A> {
    base: Rc<dyn ParserBase<Err, T, A>>,
}

enum ParserBaseResult<Err, T, A> {
    Suspend(Parser<Err, T, A>),
    Success(A),
    Error { msg: Err, consumed: bool },
}

trait ParserBase<Err, T, A> {
    fn run(&self, tokens: TokenStream<T>) -> ParserBaseResult<Err, T, A>;
}

struct EmptyParser;

impl<Err, T> ParserBase<Err, T, ()> for EmptyParser {
    fn run(&self, _tokens: TokenStream<T>) -> ParserBaseResult<Err, T, ()> {
        ParserBaseResult::Success(())
    }
}

struct SatisfyParser<T> {
    pred: Rc<RefCell<dyn FnMut(&T) -> bool>>,
}

/*
impl<Err: From<String>,T> ParserBase<Err,T,T> for SatisfyParser<T> {
    fn run(&self, tokens: TokenStream<T>) -> ParserBaseResult<Err,T,T> {
        tokens.read();
    }
}*/
