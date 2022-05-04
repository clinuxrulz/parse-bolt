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
    fn wrap_arrow(arrow: ParserArrow<T>) -> Parser<Err, T, A> {
        Parser {
            err_phantom: PhantomData,
            a_phantom: PhantomData,
            arrow,
        }
    }

    pub fn run(&self, tokens: &mut TokenStream<T>) -> Result<A, Err>
    where
        Err: From<String>,
        T: Clone + 'static,
        A: 'static,
    {
        self.arrow.run(tokens).map(|a| {
            let a: Box<A> = a.downcast().ok().unwrap();
            *a
        })
    }

    pub fn map<B: 'static, F: FnMut(A) -> B + 'static>(&self, mut f: F) -> Parser<Err, T, B>
    where
        A: 'static,
    {
        Parser::wrap_arrow(
            self.arrow
                .clone()
                .compose(&ParserArrow::lift(Rc::new(RefCell::new(
                    move |a: Box<dyn Any>| {
                        let a: Box<A> = a.downcast().ok().unwrap();
                        Box::new(f(*a)) as Box<dyn Any>
                    },
                )))),
        )
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
                stack.push(Rc::clone(lhs));
                stack.push(Rc::clone(rhs));
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

impl<T> ParserArrow<T> {
    fn run<Err>(&self, tokens: &mut TokenStream<T>) -> Result<Box<dyn Any>,Err> where Err: From<String>, T: Clone + 'static {
        let instructions = rc_vec_builder_into_vec(&self.composition);
        let mut val = Box::new(()) as Box<dyn Any>;
        for instruction in instructions {
            match instruction {
                ParserArrowF::Lift(f) => {
                    val = f.borrow_mut()(val);
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
                            val = Box::new(t) as Box<dyn Any>;
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
                        let r: Result<_,Err> = arrow.run(tokens);
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
                    let r = arrow.run(tokens);
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

    fn lift(f: Rc<RefCell<dyn FnMut(Box<dyn Any>) -> Box<dyn Any>>>) -> ParserArrow<T> {
        ParserArrow::lift_f(ParserArrowF::Lift(f))
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
    Lift(Rc<RefCell<dyn FnMut(Box<dyn Any>) -> Box<dyn Any>>>),

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
            &ParserArrowF::Lift(ref f) => ParserArrowF::Lift(Rc::clone(f)),
        }
    }
}

#[test]
fn test_arrow_parser() {
    let input = "123";
    let parser: Parser<String, char, char> = Parser::satisfy(|t| '0' <= *t && *t <= '9');
    let mut token_stream = TokenStream::from_str(input);
    let r = parser.run(&mut token_stream);
    println!("{:?}", r);
}