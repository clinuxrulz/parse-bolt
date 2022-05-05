use crate::parser::TokenStream;

use std::any::Any;
use std::cell::RefCell;
use std::marker::PhantomData;
use std::rc::Rc;

pub struct Parser<Err, T, A> {
    err_phantom: PhantomData<Err>,
    a_phantom: PhantomData<A>,
    arrow: ParserArrow<T>,
}

impl<Err, T, A> Clone for Parser<Err, T, A> {
    fn clone(&self) -> Self {
        Parser {
            err_phantom: PhantomData,
            a_phantom: PhantomData,
            arrow: self.arrow.clone(),
        }
    }
}

impl<Err, T, A> Parser<Err, T, A> {
    #[inline(always)]
    fn wrap_arrow(arrow: ParserArrow<T>) -> Parser<Err, T, A> {
        Parser {
            err_phantom: PhantomData,
            a_phantom: PhantomData,
            arrow,
        }
    }

    pub fn run(&self, str: &[char], len: usize, index: usize) -> Result<(A, usize), Err>
    where
        Err: From<String>,
        T: From<char> + Clone + 'static,
        A: 'static,
    {
        let str: String = str[index..index + len].iter().collect();
        let tokens: Vec<T> = str.chars().map(Into::into).collect();
        let mut token_stream = TokenStream::from_vec(tokens);
        self.arrow.run(&mut token_stream, Box::new(())).map(|a| {
            let a: Box<A> = a.into_box_any().downcast().ok().unwrap();
            (*a, token_stream.save())
        })
    }

    pub fn run_str(&self, str: &str) -> Result<A, Err>
    where
        Err: From<String>,
        T: From<char> + Clone + 'static,
        A: 'static,
    {
        let tokens: Vec<T> = str.chars().map(Into::into).collect();
        let mut token_stream = TokenStream::from_vec(tokens);
        self.arrow.run(&mut token_stream, Box::new(())).map(|a| {
            let a: Box<A> = a.into_box_any().downcast().ok().unwrap();
            *a
        })
    }

    #[inline(always)]
    pub fn run_token_stream(&self, tokens: &mut TokenStream<T>) -> Result<A, Err>
    where
        Err: From<String>,
        T: Clone + 'static,
        A: 'static,
    {
        self.arrow.run(tokens, Box::new(())).map(|a| {
            let a: Box<A> = a.into_box_any().downcast().ok().unwrap();
            *a
        })
    }

    #[inline(always)]
    pub fn map<B: Clone + 'static, F: FnMut(A) -> B + 'static>(&self, mut f: F) -> Parser<Err, T, B>
    where
        A: 'static,
    {
        Parser::wrap_arrow(
            self.arrow
                .clone()
                .compose(&ParserArrow::arr(Rc::new(RefCell::new(
                    move |a: Box<dyn CloneableAny>| {
                        let a: Box<A> = a.into_box_any().downcast().ok().unwrap();
                        Box::new(f(*a)) as Box<dyn CloneableAny>
                    },
                )))),
        )
    }

    #[inline(always)]
    pub fn seq2<B>(&self, p2: &Parser<Err,T,B>) -> Parser<Err,T,(A,B)>
    where
        A: Clone + 'static,
        B: Clone + 'static,
    {
        Parser::wrap_arrow(
            ParserArrow::arr(Rc::new(RefCell::new(|x: Box<dyn CloneableAny>| Box::new(CloneableAnyTuple(x.clone_any(), x)) as Box<dyn CloneableAny>)))
                .compose(&ParserArrow::first(Rc::new(self.arrow.clone())))
                .compose(
                    &ParserArrow::arr(Rc::new(RefCell::new(|x: Box<dyn CloneableAny>| {
                        let mut x: Box<CloneableAnyTuple> = x.into_box_any().downcast().ok().unwrap();
                        std::mem::swap(&mut x.0, &mut x.1);
                        return x as Box<dyn CloneableAny>;
                    })))
                )
                .compose(&ParserArrow::first(Rc::new(p2.arrow.clone())))
                .compose(
                    &ParserArrow::arr(Rc::new(RefCell::new(|x: Box<dyn CloneableAny>| {
                        let mut x: Box<CloneableAnyTuple> = x.into_box_any().downcast().ok().unwrap();
                        std::mem::swap(&mut x.0, &mut x.1);
                        return x as Box<dyn CloneableAny>;
                    })))
                )
                .compose(
                    &ParserArrow::arr(Rc::new(RefCell::new(|x: Box<dyn CloneableAny>| {
                        let x: Box<CloneableAnyTuple> = x.into_box_any().downcast().ok().unwrap();
                        let a: Box<A> = x.0.into_box_any().downcast().ok().unwrap();
                        let b: Box<B> = x.1.into_box_any().downcast().ok().unwrap();
                        let r: Box<(A,B)> = Box::new((*a, *b));
                        return r as Box<dyn CloneableAny>;
                    })))
                )
        )
    }


    #[inline(always)]
    pub fn seq_left<B: 'static>(&self, parser2: &Parser<Err, T, B>) -> Parser<Err, T, A>
    where
        Err: 'static,
        T: 'static,
        A: Clone + 'static,
        B: Clone + 'static,
    {
        self.seq2(parser2).map(|(a, _)| a)
    }

    #[inline(always)]
    pub fn seq_right<B: 'static>(&self, parser2: &Parser<Err, T, B>) -> Parser<Err, T, B>
    where
        Err: 'static,
        T: 'static,
        A: Clone + 'static,
        B: Clone + 'static,
    {
        self.seq2(parser2).map(|(_, b)| b)
    }

    #[inline(always)]
    pub fn optional(&self) -> Parser<Err, T, Option<A>>
    where
        Err: Clone + From<String> + 'static,
        T: std::fmt::Display + 'static,
        A: Clone + 'static,
    {
        Parser::choice(vec![self.map(Some), Parser::empty().map(|_| None)])
    }

    #[inline(always)]
    pub fn choice(parsers: Vec<Parser<Err,T,A>>) -> Parser<Err,T,A> {
        Parser::wrap_arrow(
            ParserArrow::choice(
                parsers.iter().map(|parser| Rc::new(parser.arrow.clone())).collect()
            )
        )
    }

    pub fn return_string(&self) -> Parser<Err,T,String>
    where
        T: Into<char> + Clone,
    {
        Parser::wrap_arrow(ParserArrow::return_string(Rc::new(self.arrow.clone())))
    }
}

impl<Err, T> Parser<Err, T, T> {
    pub fn satisfy<Pred: FnMut(&T) -> bool + 'static>(mut pred: Pred) -> Parser<Err, T, T> {
        Parser::wrap_arrow(ParserArrow::satisfy(Rc::new(RefCell::new(pred))))
    }
}

impl<Err, T> Parser<Err, T, ()> {
    pub fn empty() -> Parser<Err, T, ()> {
        Parser::wrap_arrow(ParserArrow::empty())
    }

    pub fn eof() -> Parser<Err, T, ()>
    where
        T: 'static,
    {
        Parser::wrap_arrow(ParserArrow::eof())
    }

    pub fn match_string(str: &str) -> Parser<Err, T, ()>
    where
        T: Into<char> + Clone,
    {
        Parser::wrap_arrow(ParserArrow::match_string(str.chars().collect()))
    }
}

enum VecBuilder<A> {
    Push(A),
    Append(Rc<VecBuilder<A>>,Rc<VecBuilder<A>>),
}

fn rc_vec_builder_into_vec<A: Clone>(x: &Rc<VecBuilder<A>>) -> Vec<A> {
    let mut r = Vec::new();
    let mut stack = vec![Rc::clone(x)];
    while let Some(at) = stack.pop() {
        match &*at {
            VecBuilder::Push(a) => r.push(a.clone()),
            VecBuilder::Append(lhs, rhs) => {
                // Note: This is not a bug, the rhs gets pushed first before the lhs, because
                //       they get poped off the stack in the reverse order.
                stack.push(Rc::clone(rhs));
                stack.push(Rc::clone(lhs));
            }
        }
    }
    r
}

struct ParserArrow<T> {
    composition: Rc<VecBuilder<ParserArrowF<T>>>,
}

impl<T> Clone for ParserArrow<T> {
    fn clone(&self) -> Self {
        ParserArrow {
            composition: Rc::clone(&self.composition),
        }
    }
}


trait CloneableAny {
    fn clone_any(&self) -> Box<dyn CloneableAny>;
    fn into_box_any(self: Box<Self>) -> Box<dyn Any>;
}

impl<Obj: Clone + 'static> CloneableAny for Obj {
    fn clone_any(&self) -> Box<dyn CloneableAny> {
        Box::new(self.clone())
    }
    fn into_box_any(self: Box<Self>) -> Box<dyn Any> {
        self as Box<dyn Any>
    }
}

struct CloneableAnyTuple(pub Box<dyn CloneableAny>, pub Box<dyn CloneableAny>);

impl CloneableAny for CloneableAnyTuple {
    fn clone_any(&self) -> Box<dyn CloneableAny> {
        Box::new(CloneableAnyTuple(self.0.clone_any(), self.1.clone_any()))
    }
    fn into_box_any(self: Box<Self>) -> Box<dyn Any> {
        self as Box<dyn Any>
    }
}

impl<T> ParserArrow<T> {
    fn run<Err>(&self, tokens: &mut TokenStream<T>, mut val: Box<dyn CloneableAny>) -> Result<Box<dyn CloneableAny>,Err> where Err: From<String>, T: Clone + 'static {
        let instructions = rc_vec_builder_into_vec(&self.composition);
        for instruction in instructions {
            match instruction {
                ParserArrowF::Arr(f) => {
                    val = f.borrow_mut()(val);
                }
                ParserArrowF::First(arrow) => {
                    let mut val2: Box<CloneableAnyTuple> = val.into_box_any().downcast().ok().unwrap();
                    match arrow.run(tokens, val2.0) {
                        Ok(r) => {
                            val2.0 = r;
                        },
                        Err(error) => {
                            return Err(error);
                        }
                    }
                    val = val2;
                }
                ParserArrowF::Empty => {}
                ParserArrowF::Eof => {
                    let t_op = tokens.read();
                    if t_op.is_some() {
                        return Err("fail".to_owned().into());
                    }
                }
                ParserArrowF::Satisfy(pred) => {
                    let t_op = tokens.read();
                    if let Some(t) = t_op {
                        if pred.borrow_mut()(&t) {
                            val = Box::new(t) as Box<dyn CloneableAny>;
                        } else {
                            return Err("fail".to_owned().into());
                        }
                    } else {
                        return Err("fail".to_owned().into());
                    }
                }
                ParserArrowF::MatchString(t_to_char, chars) => {
                    for c in chars {
                        let t_op = tokens.read();
                        if let Some(t) = t_op {
                            if t_to_char(&t) != c {
                                return Err("fail".to_owned().into());
                            }
                        } else {
                            return Err("fail".to_owned().into());
                        }
                    }
                }
                ParserArrowF::Choice(arrows) => {
                    // TODO: For backtracking, compose the arrows with the remaining instructions before executing them.
                    let pos = tokens.save();
                    let mut found = false;
                    for arrow in arrows {
                        tokens.restore(pos);
                        let r: Result<_,Err> = arrow.run(tokens, val.clone_any());
                        if let Ok(a) = r {
                            found = true;
                            val = a;
                            break;
                        }
                    }
                    if !found {
                        return Err("fail".to_owned().into());
                    }
                }
                ParserArrowF::ReturnString(t_to_char, arrow) => {
                    let start_pos = tokens.save();
                    let r = arrow.run(tokens, val);
                    if let Err(error) = r {
                        return Err(error);
                    }
                    let end_pos = tokens.save();
                    tokens.restore(start_pos);
                    let mut str = "".to_owned();
                    for _i in start_pos..end_pos {
                        if let Some(t) = tokens.read() {
                            str.push(t_to_char(&t));
                        }
                    }
                    tokens.restore(end_pos);
                    val = Box::new(str);
                }
            }
        }
        return Ok(val);
    }

    fn lift_f(arrow: ParserArrowF<T>) -> ParserArrow<T> {
        ParserArrow {
            composition: Rc::new(VecBuilder::Push(arrow)),
        }
    }

    fn arr(f: Rc<RefCell<dyn FnMut(Box<dyn CloneableAny>) -> Box<dyn CloneableAny>>>) -> ParserArrow<T> {
        ParserArrow::lift_f(ParserArrowF::Arr(f))
    }

    fn first(arrow: Rc<ParserArrow<T>>) -> ParserArrow<T> {
        ParserArrow::lift_f(ParserArrowF::First(arrow))
    }

    fn empty() -> ParserArrow<T> {
        ParserArrow::lift_f(ParserArrowF::Empty)
    }

    fn eof() -> ParserArrow<T> {
        ParserArrow::lift_f(ParserArrowF::Eof)
    }

    fn satisfy(pred: Rc<RefCell<dyn FnMut(&T) -> bool>>) -> ParserArrow<T> {
        ParserArrow::lift_f(ParserArrowF::Satisfy(pred))
    }

    fn match_string(str: Vec<char>) -> ParserArrow<T>
    where
        T: Clone + Into<char>,
    {
        ParserArrow::lift_f(ParserArrowF::MatchString(|t: &T| t.clone().into(), str))
    }

    fn choice(arrows: Vec<Rc<ParserArrow<T>>>) -> ParserArrow<T> {
        ParserArrow::lift_f(ParserArrowF::Choice(arrows))
    }

    fn return_string(arrow: Rc<ParserArrow<T>>) -> ParserArrow<T>
    where
        T: Clone + Into<char>,
    {
        ParserArrow::lift_f(ParserArrowF::ReturnString(|t: &T| t.clone().into(), arrow))
    }

    // (a ~> b) -> (b ~> c) -> (a ~> c)
    fn compose(&self, other: &ParserArrow<T>) -> ParserArrow<T> {
        return ParserArrow { composition: Rc::new(VecBuilder::Append(Rc::clone(&self.composition), Rc::clone(&other.composition))) };
    }
}

impl<T> ParserArrow<T> {}

// Parser Err T A = () ~> A
// ParserArrow Err T A B = A ~> B
enum ParserArrowF<T> {
    // (A -> B) -> A ~> B
    Arr(Rc<RefCell<dyn FnMut(Box<dyn CloneableAny>) -> Box<dyn CloneableAny>>>),

    // (A ~> B) -> (A,C) ~> (B,C)
    First(Rc<ParserArrow<T>>),

    // () -> () ~> ()
    Empty,

    // () -> () ~> ()
    Eof,

    // (T -> bool) -> () ~> T
    Satisfy(Rc<RefCell<dyn FnMut(&T) -> bool>>),

    // (T -> char) -> Vec char -> () ~> ()
    MatchString(fn(&T) -> char, Vec<char>),

    // Vec (A ~> B) -> A ~> B
    Choice(Vec<Rc<ParserArrow<T>>>),

    // (A ~> B) -> A ~> String
    ReturnString(fn(&T) -> char, Rc<ParserArrow<T>>),
}

impl<T> Clone for ParserArrowF<T> {
    fn clone(&self) -> Self {
        match self {
            &ParserArrowF::Arr(ref f) => ParserArrowF::Arr(Rc::clone(f)),
            &ParserArrowF::First(ref arrow) => ParserArrowF::First(Rc::clone(arrow)),
            &ParserArrowF::Empty => ParserArrowF::Empty,
            &ParserArrowF::Eof => ParserArrowF::Eof,
            &ParserArrowF::Satisfy(ref pred) => ParserArrowF::Satisfy(Rc::clone(pred)),
            &ParserArrowF::MatchString(ref token_to_char, ref chars) => {
                ParserArrowF::MatchString(*token_to_char, chars.clone())
            }
            &ParserArrowF::Choice(ref arrows) => {
                ParserArrowF::Choice(arrows.iter().map(Rc::clone).collect())
            }
            &ParserArrowF::ReturnString(ref token_to_char, ref arrow) => ParserArrowF::ReturnString(*token_to_char, Rc::clone(arrow)),
        }
    }
}

#[test]
fn test_arrow_parser() {
    let parser: Parser<String, char, _> =
        Parser::choice(vec![
            Parser::satisfy(|t| '0' <= *t && *t <= '9')
                .seq2(&Parser::satisfy(|t| *t == '2'))
                .map(|_| ()),
            Parser::match_string("ab"),
        ])
        .return_string();
    {
        let input = "123";
        let r = parser.run_str(input);
        println!("{:?}", r);
    }
    {
        let input = "ab";
        let r = parser.run_str(input);
        println!("{:?}", r);
    }
}