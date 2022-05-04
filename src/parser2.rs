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
            arrow: self.arrow.clone()
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

    pub fn run(&self, tokens: &mut TokenStream<T>) -> Result<A, Err> where Err: From<String>, T: Clone + 'static, A: 'static {
        let instructions = &self.arrow.composition;
        let mut val = Box::new(()) as Box<dyn Any>;
        for instruction in instructions {
            match instruction {
                ParserArrowF::Empty => {},
                ParserArrowF::Eof => {
                    let t_op = tokens.read();
                    if t_op.is_some() {
                        return Err("fail".to_owned().into());
                    }
                },
                ParserArrowF::Lift(f) => {
                    val = f.borrow_mut()(val);
                },
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
                },
                _ => todo!(),
            }
        }
        let r: Box<A> = val.downcast().ok().unwrap();
        return Ok(*r);
    }

    pub fn map<B: 'static,F:FnMut(A)->B+'static>(&self, mut f: F) -> Parser<Err,T,B> where A: 'static {
        Parser::wrap_arrow(self.arrow.clone().compose(&ParserArrow::lift(Rc::new(RefCell::new(
            move |a: Box<dyn Any>| {
                let a: Box<A> = a.downcast().ok().unwrap();
                Box::new(f(*a)) as Box<dyn Any>
            }
        )))))
    }
}

impl<Err, T> Parser<Err, T, T> {
    pub fn satisfy<Pred: FnMut(&T)->bool+'static>(mut pred: Pred) -> Parser<Err, T, T> {
        Parser::wrap_arrow(ParserArrow::satisfy(Rc::new(RefCell::new(pred))))
    }
}

impl<Err, T> Parser<Err, T, ()> {
    pub fn empty() -> Parser<Err, T, ()> {
        Parser::wrap_arrow(ParserArrow::empty())
    }

    pub fn eof() -> Parser<Err, T, ()> where T: 'static {
        Parser::wrap_arrow(ParserArrow::eof())
    }
}

struct ParserArrow<T> {
    composition: Vec<ParserArrowF<T>>,
}

impl<T> Clone for ParserArrow<T> {
    fn clone(&self) -> Self {
        ParserArrow { composition: self.composition.clone() }
    }
}

impl<T> ParserArrow<T> {
    fn lift_f(arrow: ParserArrowF<T>) -> ParserArrow<T> {
        ParserArrow { composition: vec![arrow], }
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

    fn match_string(str: Vec<char>) -> ParserArrow<T> where T: Clone + Into<char> {
        ParserArrow::lift_f(ParserArrowF::MatchString(|t: &T| t.clone().into(), str))
    }

    // (a ~> b) -> (b ~> c) -> (a ~> c)
    fn compose(&self, other: &ParserArrow<T>) -> ParserArrow<T> {
        let mut tmp = self.composition.clone();
        for arrow_f in &other.composition {
            tmp.push((*arrow_f).clone());
        }
        return ParserArrow { composition: tmp };
    }
}

impl<T> ParserArrow<T> {
}

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
    Choice(Vec<Rc<ParserArrowF<T>>>),

    // (A ~> B) -> A ~> String
    ReturnString(Rc<ParserArrowF<T>>),
}

impl<T> Clone for ParserArrowF<T> {
    fn clone(&self) -> Self {
        match self {
            &ParserArrowF::Empty => ParserArrowF::Empty,
            &ParserArrowF::Eof => ParserArrowF::Eof,
            &ParserArrowF::Satisfy(ref pred) => ParserArrowF::Satisfy(Rc::clone(pred)),
            &ParserArrowF::MatchString(ref token_to_char, ref chars) => ParserArrowF::MatchString(*token_to_char, chars.clone()),
            &ParserArrowF::Choice(ref arrows) => ParserArrowF::Choice(arrows.iter().map(Rc::clone).collect()),
            &ParserArrowF::ReturnString(ref arrow) => ParserArrowF::ReturnString(Rc::clone(arrow)),
            &ParserArrowF::Lift(ref f) => ParserArrowF::Lift(Rc::clone(f)),
        }
    }
}
