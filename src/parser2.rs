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

enum LinkList<A> {
    Empty,
    Cons(A, Rc<LinkList<A>>),
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
    pub fn lazy<Unbox: FnMut() -> Parser<Err, T, A> + 'static>(
        mut unbox: Unbox,
    ) -> Parser<Err, T, A> {
        Parser::wrap_arrow(ParserArrow::lazy(Rc::new(RefCell::new(move || {
            Rc::new(unbox().arrow)
        }))))
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
    pub fn seq2<B>(&self, p2: &Parser<Err, T, B>) -> Parser<Err, T, (A, B)>
    where
        A: Clone + 'static,
        B: Clone + 'static,
    {
        Parser::wrap_arrow(
            ParserArrow::arr(Rc::new(RefCell::new(|x: Box<dyn CloneableAny>| {
                Box::new(CloneableAnyTuple(x.clone_any(), x)) as Box<dyn CloneableAny>
            })))
            .compose(&ParserArrow::first(Rc::new(self.arrow.clone())))
            .compose(&ParserArrow::arr(Rc::new(RefCell::new(
                |x: Box<dyn CloneableAny>| {
                    let mut x: Box<CloneableAnyTuple> = x.into_box_any().downcast().ok().unwrap();
                    std::mem::swap(&mut x.0, &mut x.1);
                    return x as Box<dyn CloneableAny>;
                },
            ))))
            .compose(&ParserArrow::first(Rc::new(p2.arrow.clone())))
            .compose(&ParserArrow::arr(Rc::new(RefCell::new(
                |x: Box<dyn CloneableAny>| {
                    let mut x: Box<CloneableAnyTuple> = x.into_box_any().downcast().ok().unwrap();
                    std::mem::swap(&mut x.0, &mut x.1);
                    return x as Box<dyn CloneableAny>;
                },
            ))))
            .compose(&ParserArrow::arr(Rc::new(RefCell::new(
                |x: Box<dyn CloneableAny>| {
                    let x: Box<CloneableAnyTuple> = x.into_box_any().downcast().ok().unwrap();
                    let a: Box<A> = x.0.into_box_any().downcast().ok().unwrap();
                    let b: Box<B> = x.1.into_box_any().downcast().ok().unwrap();
                    let r: Box<(A, B)> = Box::new((*a, *b));
                    return r as Box<dyn CloneableAny>;
                },
            )))),
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
    pub fn choice(parsers: Vec<Parser<Err, T, A>>) -> Parser<Err, T, A> {
        Parser::wrap_arrow(ParserArrow::choice(
            parsers
                .iter()
                .map(|parser| Rc::new(parser.arrow.clone()))
                .collect(),
        ))
    }

    #[inline(always)]
    pub fn zero_or_more_vec(&self) -> Parser<Err, T, Vec<A>>
    where
        Err: Clone + From<String> + 'static,
        T: std::fmt::Display + 'static,
        A: Clone + 'static,
    {
        self.zero_or_more_vec_greedy()
            .map(|mut xs| {
                let mut xs2 = Vec::new();
                while let &LinkList::Cons(ref x, ref xs3) = &*xs {
                    xs2.push(x.clone());
                    xs = Rc::clone(xs3);
                }
                xs2
            })
    }

    #[inline(always)]
    pub fn one_or_more_vec(&self) -> Parser<Err, T, Vec<A>>
    where
        Err: Clone + From<String> + 'static,
        T: std::fmt::Display + 'static,
        A: Clone + 'static,
    {
        self.seq2(&self.zero_or_more_vec()).map(|(x, mut xs)| {
            xs.insert(0, x);
            xs
        })
    }

    #[inline(always)]
    fn zero_or_more_vec_greedy(&self) -> Parser<Err, T, Rc<LinkList<A>>>
    where
        Err: Clone + From<String> + 'static,
        T: std::fmt::Display + 'static,
        A: Clone + 'static,
    {
        let self2 = self.clone();
        Parser::choice(vec![
            self.seq2(&Parser::lazy(move || self2.zero_or_more_vec_greedy())).map(move |(x, xs)| {
                Rc::new(LinkList::Cons(x, Rc::clone(&xs)))
            }),
            Parser::empty().map(move |_| Rc::new(LinkList::Empty)),
        ])
    }

    #[inline(always)]
    pub fn exactly_vec(&self, n: usize) -> Parser<Err, T, Vec<A>>
    where
        Err: Clone + From<String> + 'static,
        T: std::fmt::Display + 'static,
        A: Clone + 'static,
    {
        if n == 0 {
            return Parser::empty().map(|_| Vec::new());
        } else {
            return Parser::exactly_vec(&self, n - 1)
                .seq2(self)
                .map(|(mut xs, x)| {
                    xs.push(x);
                    xs
                });
        }
    }

    pub fn return_string(&self) -> Parser<Err, T, String>
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
    Append(Rc<VecBuilder<A>>, Rc<VecBuilder<A>>),
    PushMany(Rc<Vec<A>>),
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
            },
            VecBuilder::PushMany(xs) => {
                for x in &**xs as &Vec<A> {
                    r.push((*x).clone());
                }
            },
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
    fn optimise(&self) -> ParserArrow<T> {
        ParserArrow {
            composition: Rc::new(VecBuilder::PushMany(Rc::new(rc_vec_builder_into_vec(&self.composition))))
        }
    }

    fn run<Err>(
        &self,
        tokens: &mut TokenStream<T>,
        mut val: Box<dyn CloneableAny>,
    ) -> Result<Box<dyn CloneableAny>, Err>
    where
        Err: From<String>,
        T: Clone + 'static,
    {
        struct BoxedCloneable {
            x: Box<dyn CloneableAny>,
        }
        impl Clone for BoxedCloneable {
            fn clone(&self) -> Self {
                BoxedCloneable { x: self.x.clone_any() }
            }
        }
        #[derive(Clone)]
        enum Instruction<T> {
            RunArrow(Rc<ParserArrow<T>>),
            RunArrowF(ParserArrowF<T>),
            MakeStringFromPosIntoVal(fn(&T)->char,usize),
            PushVal,
            PopVal,
            InjectFirstHalfOfTuple(BoxedCloneable),
            SuspendError,
            UnsuspendError,
        }
        let mut run_arrow_stack: Vec<(usize, Box<dyn CloneableAny>, Rc<ParserArrow<T>>, Rc<Vec<Instruction<T>>>)> = Vec::new();
        let mut value_stack: Vec<Box<dyn CloneableAny>> = Vec::new();
        let mut instruction_stack = Vec::new();
        let mut last_error_op: Option<(usize, Err)> = None;
        let mut suspended_error_op: Option<(usize, Err)> = None;
        fn assign_error_if_further<Err>(last_error_op: Option<(usize, Err)>, pos: usize, err: Err) -> Option<(usize, Err)> {
            if let Some((pos2, _)) = &last_error_op {
                if *pos2 < pos {
                    return Some((pos, err));
                } else {
                    return last_error_op;
                }
            } else if last_error_op.is_none() {
                return Some((pos, err));
            } else {
                return last_error_op;
            }
        }
        run_arrow_stack.push((tokens.save(), Box::new(()), Rc::new(self.clone()), Rc::new(Vec::new())));
        'arrow_loop: while let Some((pos, start_val, arrow, init_instruction_stack)) = run_arrow_stack.pop() {
            val = start_val;
            tokens.restore(pos);
            instruction_stack.clear();
            for instruction in &*init_instruction_stack as &Vec<Instruction<T>> {
                instruction_stack.push(instruction.clone())
            }
            instruction_stack.push(Instruction::RunArrow(arrow));
            'instruction_loop: while let Some(instruction) = instruction_stack.pop() {
                match instruction {
                    Instruction::RunArrow(arrow) => {
                        let arrowf_vec = rc_vec_builder_into_vec(&arrow.composition);
                        for arrow_f in arrowf_vec.into_iter().rev() {
                            instruction_stack.push(Instruction::RunArrowF(arrow_f));
                        }
                    },
                    Instruction::RunArrowF(arrow_instruction) => {
                        match arrow_instruction {
                            ParserArrowF::Lazy(arrow) => {
                                let arrow = arrow.borrow_mut()();
                                instruction_stack.push(Instruction::RunArrow(arrow));
                            }
                            ParserArrowF::Arr(f) => {
                                val = f.borrow_mut()(val);
                            }
                            ParserArrowF::First(arrow) => {
                                let val2: Box<CloneableAnyTuple> = val.into_box_any().downcast().ok().unwrap();
                                val = val2.0;
                                let second_part = val2.1;
                                instruction_stack.push(Instruction::InjectFirstHalfOfTuple(BoxedCloneable { x: second_part }));
                                instruction_stack.push(Instruction::RunArrow(arrow));
                            }
                            ParserArrowF::Empty => {}
                            ParserArrowF::Eof => {
                                let t_op = tokens.read();
                                if t_op.is_some() {
                                    last_error_op = assign_error_if_further(last_error_op, tokens.save(), "Expected end of file".to_owned().into());
                                    break 'instruction_loop;
                                }
                            }
                            ParserArrowF::Satisfy(pred) => {
                                let t_op = tokens.read();
                                if let Some(t) = t_op {
                                    if pred.borrow_mut()(&t) {
                                        val = Box::new(t) as Box<dyn CloneableAny>;
                                    } else {
                                        last_error_op = assign_error_if_further(last_error_op, tokens.save(), "Predicate failed".to_owned().into());
                                        break 'instruction_loop;
                                    }
                                } else {
                                    last_error_op = assign_error_if_further(last_error_op, tokens.save(), "Predicate failed".to_owned().into());
                                    break 'instruction_loop;
                                }
                            }
                            ParserArrowF::MatchString(t_to_char, chars) => {
                                for c in chars {
                                    let t_op = tokens.read();
                                    if let Some(t) = t_op {
                                        if t_to_char(&t) != c {
                                            last_error_op = assign_error_if_further(last_error_op, tokens.save(), "fail".to_owned().into());
                                            break 'instruction_loop;
                                        }
                                    } else {
                                        last_error_op = assign_error_if_further(last_error_op, tokens.save(), "fail".to_owned().into());
                                        break 'instruction_loop;
                                    }
                                }
                            }
                            ParserArrowF::Choice(arrows) => {
                                let mut remaining_instructions = Vec::new();
                                std::mem::swap(&mut remaining_instructions, &mut instruction_stack);
                                remaining_instructions.push(Instruction::SuspendError);
                                let remaining_instructions = Rc::new(remaining_instructions);
                                let pos = tokens.save();
                                run_arrow_stack.push(
                                    (
                                        pos,
                                        val.clone_any(),
                                        Rc::new(ParserArrow::empty()),
                                        Rc::new(vec![Instruction::UnsuspendError])
                                    )
                                );
                                for arrow in arrows.into_iter().rev() {
                                    run_arrow_stack.push((pos, val.clone_any(), arrow, Rc::clone(&remaining_instructions)));
                                }
                                last_error_op = None;
                                continue 'arrow_loop;
                            }
                            ParserArrowF::ReturnString(t_to_char, arrow) => {
                                instruction_stack.push(Instruction::MakeStringFromPosIntoVal(t_to_char, tokens.save()));
                                instruction_stack.push(Instruction::RunArrow(arrow));
                            }
                        }
                    }
                    Instruction::MakeStringFromPosIntoVal(t_to_char, start_pos) => {
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
                    Instruction::PushVal => {
                        value_stack.push(val.clone_any());
                    }
                    Instruction::PopVal => {
                        val = value_stack.pop().unwrap();
                    }
                    Instruction::InjectFirstHalfOfTuple(second_part) => {
                        val = Box::new(CloneableAnyTuple(val, second_part.x));
                    }
                    Instruction::SuspendError => {
                        if let Some(last_error) = last_error_op {
                            suspended_error_op = assign_error_if_further(suspended_error_op, last_error.0, last_error.1)
                        }
                        last_error_op = None;
                    }
                    Instruction::UnsuspendError => {
                        if let Some(suspended_error) = suspended_error_op {
                            last_error_op = assign_error_if_further(last_error_op, suspended_error.0, suspended_error.1);
                        }
                        suspended_error_op = None;
                    }
                }
            }
            if instruction_stack.is_empty() {
                break;
            }
        }
        if let Some((pos, error)) = last_error_op {
            tokens.restore(pos);
            return Err(error);
        }
        return Ok(val);
    }

    fn lift_f(arrow: ParserArrowF<T>) -> ParserArrow<T> {
        ParserArrow {
            composition: Rc::new(VecBuilder::Push(arrow)),
        }
    }

    fn lazy(unbox: Rc<RefCell<dyn FnMut() -> Rc<ParserArrow<T>>>>) -> ParserArrow<T> {
        ParserArrow::lift_f(ParserArrowF::Lazy(unbox))
    }

    fn arr(
        f: Rc<RefCell<dyn FnMut(Box<dyn CloneableAny>) -> Box<dyn CloneableAny>>>,
    ) -> ParserArrow<T> {
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
        return ParserArrow {
            composition: Rc::new(VecBuilder::Append(
                Rc::clone(&self.composition),
                Rc::clone(&other.composition),
            )),
        };
    }
}

impl<T> ParserArrow<T> {}

// Parser Err T A = () ~> A
// ParserArrow Err T A B = A ~> B
enum ParserArrowF<T> {
    Lazy(Rc<RefCell<dyn FnMut() -> Rc<ParserArrow<T>>>>),

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
            &ParserArrowF::Lazy(ref unbox) => ParserArrowF::Lazy(Rc::clone(unbox)),
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
            &ParserArrowF::ReturnString(ref token_to_char, ref arrow) => {
                ParserArrowF::ReturnString(*token_to_char, Rc::clone(arrow))
            }
        }
    }
}

#[test]
fn test_arrow_parser_simple_1() {
    let parser: Parser<String,_,_> =
        Parser::choice(vec![
            Parser::satisfy(|t| '0' <= *t && *t <= '9'),
            Parser::satisfy(|t| 'A' <= *t && *t <= 'Z'),
        ]);
    let input = "E";
    let r = parser.run_str(input);
    println!("{:?}", r);
}

#[test]
fn test_arrow_parser() {
    let parser: Parser<String, char, _> = Parser::choice(vec![
        Parser::satisfy(|t| '0' <= *t && *t <= '9')
            .seq2(&Parser::satisfy(|t| *t == '2'))
            .seq2(&Parser::satisfy(|t| *t == '3').one_or_more_vec())
            .seq2(&Parser::satisfy(|t| *t == '4'))
            .seq2(&Parser::satisfy(|t| *t == '4'))
            .map(|_| ()),
        Parser::match_string("ab"),
    ])
    .return_string();
    {
        let input = "12333344";
        let r = parser.run_str(input);
        println!("{:?}", r);
    }
    {
        let input = "ab";
        let r = parser.run_str(input);
        println!("{:?}", r);
    }
    {
        let input = "777";
        let r = parser.run_str(input);
        println!("{:?}", r);
    }
}
