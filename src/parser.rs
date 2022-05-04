use std::any::Any;
use std::cell::RefCell;
use std::marker::PhantomData;
use std::rc::Rc;

pub struct TokenStream<T> {
    tokens: Vec<T>,
    pos: usize,
}

impl<T> TokenStream<T> {
    pub fn read(&mut self) -> Option<T>
    where
        T: Clone,
    {
        if self.pos >= self.tokens.len() {
            return None;
        }
        let t = self.tokens[self.pos].clone();
        self.pos += 1;
        return Some(t);
    }

    pub fn save(&mut self) -> usize {
        self.pos
    }

    pub fn restore(&mut self, at: usize) {
        self.pos = at;
    }
}

impl TokenStream<char> {
    pub fn from_str(str: &str) -> TokenStream<char> {
        TokenStream {
            tokens: str.chars().collect(),
            pos: 0,
        }
    }
}

impl<T> TokenStream<T> {
    pub fn from_vec(tokens: Vec<T>) -> TokenStream<T> {
        TokenStream {
            tokens,
            pos: 0,
        }
    }
}

pub struct Parser<Err, T, A> {
    base: Rc<dyn ParserBase<Err, T, A>>,
}

impl<Err, T, A> Clone for Parser<Err, T, A> {
    fn clone(&self) -> Self {
        Parser {
            base: Rc::clone(&self.base),
        }
    }
}

enum LinkList<A> {
    Empty,
    Cons(A, Rc<LinkList<A>>),
}

impl<Err, T, A> Parser<Err, T, A> {
    #[inline(always)]
    fn wrap_base<BP: ParserBase<Err, T, A> + 'static>(base: BP) -> Parser<Err, T, A> {
        Parser {
            base: Rc::new(base),
        }
    }

    pub fn run(&self, str: &[char], len: usize, index: usize) -> Result<(A, usize), Err>
    where
        T: From<char>,
    {
        let str: String = str[index..index + len].iter().collect();
        let generator = self.base.run();
        let tokens: Vec<T> = str.chars().map(Into::into).collect();
        let mut token_stream = TokenStream {
            tokens: tokens,
            pos: 0,
        };
        match generator.resume(&mut token_stream) {
            GeneratorState::Yielded(r, _) => {
                return Ok((r, token_stream.pos));
            }
            GeneratorState::Complete(r) => {
                return r.map(|r2| (r2, token_stream.pos));
            }
        }
    }

    pub fn run_str(&self, str: &str) -> Result<A, Err>
    where
        T: From<char>,
    {
        let generator = self.base.run();
        let tokens: Vec<T> = str.chars().map(Into::into).collect();
        let mut token_stream = TokenStream {
            tokens: tokens,
            pos: 0,
        };
        match generator.resume(&mut token_stream) {
            GeneratorState::Yielded(r, _) => {
                return Ok(r);
            }
            GeneratorState::Complete(r) => {
                return r;
            }
        }
    }

    #[inline(always)]
    pub fn choice(parsers: Vec<Parser<Err, T, A>>) -> Parser<Err, T, A>
    where
        Err: Clone + From<String> + 'static,
        T: 'static,
        A: 'static,
    {
        Parser::wrap_base(OrderedChoiceParser { parsers })
    }

    #[inline(always)]
    pub fn unordered_choice(parsers: Vec<Parser<Err, T, A>>) -> Parser<Err, T, A>
    where
        Err: Clone + 'static,
        T: 'static,
        A: 'static,
    {
        Parser::wrap_base(UnorderedChoiceParser { parsers })
    }

    #[inline(always)]
    pub fn map<B, F: FnMut(A) -> B + 'static>(&self, f: F) -> Parser<Err, T, B>
    where
        Err: 'static,
        T: 'static,
        A: 'static,
        B: 'static,
    {
        Parser::wrap_base(MapParser {
            parser: self.clone(),
            f: Rc::new(RefCell::new(f)),
        })
    }

    #[inline(always)]
    pub fn map_to<B: Clone>(&self, b: B) -> Parser<Err, T, B>
    where
        Err: 'static,
        T: 'static,
        A: 'static,
        B: 'static,
    {
        self.map(move |_| b.clone())
    }

    #[inline(always)]
    pub fn flat_map<B, Cont: FnMut(A) -> Parser<Err, T, B> + 'static>(
        &self,
        cont: Cont,
    ) -> Parser<Err, T, B>
    where
        Err: 'static,
        T: 'static,
        A: 'static,
        B: 'static,
    {
        Parser::wrap_base(FlatMapParser {
            parser: self.clone(),
            cont: Rc::new(RefCell::new(cont)),
        })
    }

    #[inline(always)]
    pub fn seq2<B: 'static>(&self, parser2: &Parser<Err, T, B>) -> Parser<Err, T, (A, B)>
    where
        Err: 'static,
        T: 'static,
        A: Clone + 'static,
    {
        let parser2 = parser2.clone();
        self.flat_map(move |a| parser2.map(move |b| (a.clone(), b)))
    }

    #[inline(always)]
    pub fn seq_left<B: 'static>(&self, parser2: &Parser<Err, T, B>) -> Parser<Err, T, A>
    where
        Err: 'static,
        T: 'static,
        A: Clone + 'static,
    {
        self.seq2(parser2).map(|(a, _)| a)
    }

    #[inline(always)]
    pub fn seq_right<B: 'static>(&self, parser2: &Parser<Err, T, B>) -> Parser<Err, T, B>
    where
        Err: 'static,
        T: 'static,
        A: Clone + 'static,
    {
        self.seq2(parser2).map(|(_, b)| b)
    }

    #[inline(always)]
    pub fn optional(&self) -> Parser<Err, T, Option<A>>
    where
        Err: Clone + From<String> + 'static,
        T: std::fmt::Display + 'static,
        A: 'static,
    {
        Parser::choice(vec![self.map(Some), Parser::empty().map(|_| None)])
    }

    #[inline(always)]
    pub fn optional_unordered_choice(&self) -> Parser<Err, T, Option<A>>
    where
        Err: Clone + From<String> + 'static,
        T: std::fmt::Display + 'static,
        A: 'static,
    {
        Parser::unordered_choice(vec![self.map(Some), Parser::empty().map(|_| None)])
    }

    #[inline(always)]
    pub fn lazy<Unbox: FnMut() -> Parser<Err, T, A> + 'static>(
        mut unbox: Unbox,
    ) -> Parser<Err, T, A>
    where
        Err: From<String> + 'static,
        T: std::fmt::Display + 'static,
        A: 'static,
    {
        Parser::empty().flat_map(move |_| unbox())
    }

    #[inline(always)]
    pub fn zero_or_more_vec(&self) -> Parser<Err, T, Vec<A>>
    where
        Err: Clone + From<String> + 'static,
        T: std::fmt::Display + 'static,
        A: Clone + 'static,
    {
        self.zero_or_more_vec_rev_greedy(Rc::new(LinkList::Empty))
            .map(|mut xs| {
                let mut xs2 = Vec::new();
                while let &LinkList::Cons(ref x, ref xs3) = &*xs {
                    xs2.push(x.clone());
                    xs = xs3.clone();
                }
                xs2.reverse();
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
    pub fn zero_or_more_vec_unordered_choice(&self) -> Parser<Err, T, Vec<A>>
    where
        Err: Clone + From<String> + 'static,
        T: std::fmt::Display + 'static,
        A: Clone + 'static,
    {
        self.zero_or_more_vec_rev_unordered_choice(Rc::new(LinkList::Empty))
            .map(|mut xs| {
                let mut xs2 = Vec::new();
                while let &LinkList::Cons(ref x, ref xs3) = &*xs {
                    xs2.push(x.clone());
                    xs = xs3.clone();
                }
                xs2.reverse();
                xs2
            })
    }

    #[inline(always)]
    pub fn one_or_more_vec_unordered_choice(&self) -> Parser<Err, T, Vec<A>>
    where
        Err: Clone + From<String> + 'static,
        T: std::fmt::Display + 'static,
        A: Clone + 'static,
    {
        self.seq2(&self.zero_or_more_vec_unordered_choice())
            .map(|(x, mut xs)| {
                xs.insert(0, x);
                xs
            })
    }

    #[inline(always)]
    fn zero_or_more_vec_rev_unordered_choice(
        &self,
        xs: Rc<LinkList<A>>,
    ) -> Parser<Err, T, Rc<LinkList<A>>>
    where
        Err: Clone + From<String> + 'static,
        T: std::fmt::Display + 'static,
        A: Clone + 'static,
    {
        let self2 = self.clone();
        let xs2 = Rc::clone(&xs);
        Parser::unordered_choice(vec![
            self.flat_map(move |x| {
                let xs2 = Rc::new(LinkList::Cons(x, Rc::clone(&xs)));
                self2.zero_or_more_vec_rev_unordered_choice(xs2)
            }),
            Parser::empty().map(move |_| Rc::clone(&xs2)),
        ])
    }

    #[inline(always)]
    fn zero_or_more_vec_rev_greedy(&self, xs: Rc<LinkList<A>>) -> Parser<Err, T, Rc<LinkList<A>>>
    where
        Err: Clone + From<String> + 'static,
        T: std::fmt::Display + 'static,
        A: Clone + 'static,
    {
        let self2 = self.clone();
        let xs2 = Rc::clone(&xs);
        Parser::choice(vec![
            self.flat_map(move |x| {
                let xs2 = Rc::new(LinkList::Cons(x, Rc::clone(&xs)));
                self2.zero_or_more_vec_rev_greedy(xs2)
            }),
            Parser::empty().map(move |_| Rc::clone(&xs2)),
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

    #[inline(always)]
    pub fn filter<Pred: FnMut(&A) -> bool + 'static>(
        &self,
        pred: Pred,
        error: Err,
    ) -> Parser<Err, T, A>
    where
        Err: Clone + 'static,
        T: 'static,
        A: 'static,
    {
        Parser::wrap_base(FilterParser {
            parser: self.clone(),
            pred: Rc::new(RefCell::new(pred)),
            error,
        })
    }

    #[inline(always)]
    pub fn return_string(&self) -> Parser<Err, T, String>
    where
        Err: 'static,
        T: Clone + Into<char> + 'static,
        A: 'static,
    {
        Parser::wrap_base(ReturnStringParser {
            parser: self.clone(),
        })
    }

    #[inline(always)]
    pub fn unimplemented() -> Parser<Err, T, A>
    where
        Err: 'static,
        T: 'static,
        A: 'static,
    {
        Parser::empty().map(|_| unimplemented!())
    }
}

impl<Err, T> Parser<Err, T, T> {
    #[inline(always)]
    pub fn any() -> Parser<Err, T, T>
    where
        Err: From<String> + 'static,
        T: Clone + 'static,
    {
        Parser::satisfy(|_| true)
    }

    #[inline(always)]
    pub fn match_(t: T) -> Parser<Err, T, T>
    where
        Err: From<String> + 'static,
        T: Clone + PartialEq + 'static,
    {
        Parser::satisfy(move |t2| *t2 == t)
    }

    #[inline(always)]
    pub fn satisfy<Pred: FnMut(&T) -> bool + 'static>(pred: Pred) -> Parser<Err, T, T>
    where
        Err: From<String> + 'static,
        T: Clone + 'static,
    {
        Parser::wrap_base(SatisfyParser {
            pred: Rc::new(RefCell::new(pred)),
        })
    }
}

impl<Err, T> Parser<Err, T, ()> {
    #[inline(always)]
    pub fn empty() -> Parser<Err, T, ()>
    where
        Err: 'static,
    {
        Parser::wrap_base(EmptyParser)
    }

    #[inline(always)]
    pub fn eof() -> Parser<Err, T, ()>
    where
        Err: From<String> + 'static,
        T: Clone + std::fmt::Display,
    {
        Parser::wrap_base(EofParser)
    }

    #[inline(always)]
    pub fn match_string(str: &str) -> Parser<Err, T, ()>
    where
        Err: From<String> + 'static,
        T: Clone + Into<char>,
    {
        Parser::wrap_base(MatchStringParser {
            str: Rc::new(str.chars().collect()),
        })
    }
}

trait ParserBase<Err, T, A> {
    fn run(&self) -> Box<dyn Generator<TokenStream<T>, Yield = A, Result = Result<A, Err>>>;
}

pub struct Parser2<Err, T, A> {
    err_phantom: PhantomData<Err>,
    a_phantom: PhantomData<A>,
    base: Rc<ParserBase2<T>>,
}

impl<Err, T, A> Clone for Parser2<Err, T, A> {
    fn clone(&self) -> Self {
        Parser2 {
            err_phantom: PhantomData,
            a_phantom: PhantomData,
            base: Rc::clone(&self.base),
        }
    }
}

impl<Err, T, A> Parser2<Err, T, A> {
    fn wrap_base(base: ParserBase2<T>) -> Parser2<Err, T, A> {
        Parser2 {
            err_phantom: PhantomData,
            a_phantom: PhantomData,
            base: Rc::new(base),
        }
    }

    pub fn run(&self, str: &[char], len: usize, index: usize) -> Result<(A, usize), Err>
    where
        Err: Clone + From<String> + 'static,
        T: Clone + From<char> + Into<char> + std::fmt::Display + 'static,
        A: 'static,
    {
        let parser: Parser<Err, T, A> = self.base.into_any_parser().map(|x| {
            let x: Box<A> = x.downcast().ok().unwrap();
            *x
        });
        return parser.run(str, len, index);
    }

    pub fn run_str(&self, str: &str) -> Result<A, Err>
    where
        Err: Clone + From<String> + 'static,
        T: Clone + From<char> + Into<char> + std::fmt::Display + 'static,
        A: 'static,
    {
        let parser: Parser<Err, T, A> = self.base.into_any_parser().map(|x| {
            let x: Box<A> = x.downcast().ok().unwrap();
            *x
        });
        return parser.run_str(str);
    }

    pub fn map<B, F: FnMut(A) -> B + 'static>(&self, mut f: F) -> Parser2<Err, T, B>
    where
        A: 'static,
        B: 'static,
    {
        let f = Rc::new(RefCell::new(move |a: Box<dyn Any>| {
            let a: A = *a.downcast::<A>().ok().unwrap();
            Box::new(f(a)) as Box<dyn Any>
        }));
        Parser2::wrap_base(ParserBase2::MapParser(Rc::clone(&self.base), f))
    }

    #[inline(always)]
    pub fn map_to<B: Clone>(&self, b: B) -> Parser2<Err, T, B>
    where
        Err: 'static,
        T: 'static,
        A: 'static,
        B: 'static,
    {
        self.map(move |_| b.clone())
    }

    pub fn flat_map<B, Cont: FnMut(A) -> Parser2<Err, T, B> + 'static>(
        &self,
        mut cont: Cont,
    ) -> Parser2<Err, T, B>
    where
        T: 'static,
        A: 'static,
    {
        let cont = Rc::new(RefCell::new(move |a: Box<dyn Any>| {
            let a: A = *a.downcast::<A>().ok().unwrap();
            cont(a).base
        }));
        match &*self.base {
            &ParserBase2::FlatMapParser(ref ma, ref k) => {
                let k = Rc::clone(k);
                Parser2::wrap_base(ParserBase2::FlatMapParser(
                    Rc::clone(ma),
                    Rc::new(RefCell::new(move |a| {
                        let cont = cont.clone();
                        Rc::new(ParserBase2::FlatMapParser(k.borrow_mut()(a), cont))
                    })),
                ))
            }
            _ => Parser2::wrap_base(ParserBase2::FlatMapParser(Rc::clone(&self.base), cont)),
        }
    }

    pub fn choice(parsers: Vec<Parser2<Err, T, A>>) -> Parser2<Err, T, A> {
        Parser2::wrap_base(ParserBase2::OrderedChoice(
            parsers.iter().map(|x| Rc::clone(&x.base)).collect(),
        ))
    }

    pub fn unordered_choice(parsers: Vec<Parser2<Err, T, A>>) -> Parser2<Err, T, A> {
        Parser2::wrap_base(ParserBase2::UnorderedChoice(
            parsers.iter().map(|x| Rc::clone(&x.base)).collect(),
        ))
    }

    #[inline(always)]
    pub fn seq2<B: 'static>(&self, parser2: &Parser2<Err, T, B>) -> Parser2<Err, T, (A, B)>
    where
        Err: 'static,
        T: 'static,
        A: Clone + 'static,
    {
        let parser2 = parser2.clone();
        self.flat_map(move |a| parser2.map(move |b| (a.clone(), b)))
    }

    #[inline(always)]
    pub fn seq_left<B: 'static>(&self, parser2: &Parser2<Err, T, B>) -> Parser2<Err, T, A>
    where
        Err: 'static,
        T: 'static,
        A: Clone + 'static,
    {
        self.seq2(parser2).map(|(a, _)| a)
    }

    #[inline(always)]
    pub fn seq_right<B: 'static>(&self, parser2: &Parser2<Err, T, B>) -> Parser2<Err, T, B>
    where
        Err: 'static,
        T: 'static,
        A: Clone + 'static,
    {
        self.seq2(parser2).map(|(_, b)| b)
    }

    #[inline(always)]
    pub fn optional(&self) -> Parser2<Err, T, Option<A>>
    where
        Err: Clone + From<String> + 'static,
        T: std::fmt::Display + 'static,
        A: 'static,
    {
        Parser2::unordered_choice(vec![self.map(Some), Parser2::empty().map(|_| None)])
    }

    #[inline(always)]
    pub fn lazy<Unbox: FnMut() -> Parser2<Err, T, A> + 'static>(
        mut unbox: Unbox,
    ) -> Parser2<Err, T, A>
    where
        Err: From<String> + 'static,
        T: std::fmt::Display + 'static,
        A: 'static,
    {
        Parser2::empty().flat_map(move |_| unbox())
    }

    #[inline(always)]
    pub fn zero_or_more_vec(&self) -> Parser2<Err, T, Vec<A>>
    where
        Err: Clone + From<String> + 'static,
        T: std::fmt::Display + 'static,
        A: Clone + 'static,
    {
        self.zero_or_more_vec_rev_greedy(Rc::new(LinkList::Empty))
            .map(|mut xs| {
                let mut xs2 = Vec::new();
                while let &LinkList::Cons(ref x, ref xs3) = &*xs {
                    xs2.push(x.clone());
                    xs = xs3.clone();
                }
                xs2.reverse();
                xs2
            })
    }

    #[inline(always)]
    pub fn one_or_more_vec(&self) -> Parser2<Err, T, Vec<A>>
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
    pub fn zero_or_more_vec_unordered_choice(&self) -> Parser2<Err, T, Vec<A>>
    where
        Err: Clone + From<String> + 'static,
        T: std::fmt::Display + 'static,
        A: Clone + 'static,
    {
        self.zero_or_more_vec_rev_unordered_choice(Rc::new(LinkList::Empty))
            .map(|mut xs| {
                let mut xs2 = Vec::new();
                while let &LinkList::Cons(ref x, ref xs3) = &*xs {
                    xs2.push(x.clone());
                    xs = xs3.clone();
                }
                xs2.reverse();
                xs2
            })
    }

    #[inline(always)]
    pub fn one_or_more_vec_unordered_choice(&self) -> Parser2<Err, T, Vec<A>>
    where
        Err: Clone + From<String> + 'static,
        T: std::fmt::Display + 'static,
        A: Clone + 'static,
    {
        self.seq2(&self.zero_or_more_vec_unordered_choice())
            .map(|(x, mut xs)| {
                xs.insert(0, x);
                xs
            })
    }

    #[inline(always)]
    fn zero_or_more_vec_rev_unordered_choice(
        &self,
        xs: Rc<LinkList<A>>,
    ) -> Parser2<Err, T, Rc<LinkList<A>>>
    where
        Err: Clone + From<String> + 'static,
        T: std::fmt::Display + 'static,
        A: Clone + 'static,
    {
        let self2 = self.clone();
        let xs2 = Rc::clone(&xs);
        Parser2::unordered_choice(vec![
            self.flat_map(move |x| {
                let xs2 = Rc::new(LinkList::Cons(x, Rc::clone(&xs)));
                self2.zero_or_more_vec_rev_unordered_choice(xs2)
            }),
            Parser2::empty().map(move |_| Rc::clone(&xs2)),
        ])
    }

    #[inline(always)]
    fn zero_or_more_vec_rev_greedy(&self, xs: Rc<LinkList<A>>) -> Parser2<Err, T, Rc<LinkList<A>>>
    where
        Err: Clone + From<String> + 'static,
        T: std::fmt::Display + 'static,
        A: Clone + 'static,
    {
        let self2 = self.clone();
        let xs2 = Rc::clone(&xs);
        Parser2::choice(vec![
            self.flat_map(move |x| {
                let xs2 = Rc::new(LinkList::Cons(x, Rc::clone(&xs)));
                self2.zero_or_more_vec_rev_greedy(xs2)
            }),
            Parser2::empty().map(move |_| Rc::clone(&xs2)),
        ])
    }

    #[inline(always)]
    pub fn exactly_vec(&self, n: usize) -> Parser2<Err, T, Vec<A>>
    where
        Err: Clone + From<String> + 'static,
        T: std::fmt::Display + 'static,
        A: Clone + 'static,
    {
        if n == 0 {
            return Parser2::empty().map(|_| Vec::new());
        } else {
            return Parser2::exactly_vec(&self, n - 1)
                .seq2(self)
                .map(|(mut xs, x)| {
                    xs.push(x);
                    xs
                });
        }
    }

    #[inline(always)]
    pub fn filter<Pred: FnMut(&A) -> bool + 'static>(
        &self,
        mut pred: Pred,
        _error: Err,
    ) -> Parser2<Err, T, A>
    where
        Err: Clone + 'static,
        T: 'static,
        A: 'static,
    {
        Parser2::wrap_base(ParserBase2::FilterParser(
            Rc::clone(&self.base),
            Rc::new(RefCell::new(move |x: &Box<dyn Any>| {
                let a = x.downcast_ref::<A>().unwrap();
                pred(a)
            })),
        ))
    }

    pub fn return_string(&self) -> Parser2<Err, T, String> {
        Parser2::wrap_base(ParserBase2::ReturnString(Rc::clone(&self.base)))
    }

    pub fn unimplemented() -> Parser2<Err, T, A> {
        Parser2::wrap_base(ParserBase2::Unimplemented)
    }
}

impl<Err, T> Parser2<Err, T, ()> {
    pub fn empty() -> Parser2<Err, T, ()> {
        Parser2::wrap_base(ParserBase2::EmptyParser)
    }

    pub fn eof() -> Parser2<Err, T, ()> {
        Parser2::wrap_base(ParserBase2::EofParser)
    }

    pub fn match_string(str: &str) -> Parser2<Err, T, ()>
    where
        T: Clone + Into<char>,
    {
        Parser2::wrap_base(ParserBase2::MatchStringParser(
            Clone::clone,
            Into::into,
            str.chars().collect(),
        ))
    }
}

impl<Err, T> Parser2<Err, T, T> {
    pub fn any() -> Parser2<Err, T, T> {
        Parser2::satisfy(|_| true)
    }

    pub fn match_(t: T) -> Parser2<Err, T, T>
    where
        T: PartialEq + 'static,
    {
        Parser2::satisfy(move |t2| *t2 == t)
    }

    pub fn satisfy<Pred: FnMut(&T) -> bool + 'static>(pred: Pred) -> Parser2<Err, T, T> {
        let pred = Rc::new(RefCell::new(pred));
        Parser2::wrap_base(ParserBase2::SatisfyParser(pred))
    }
}

enum ParserBase2<T> {
    EmptyParser,
    SatisfyParser(Rc<RefCell<dyn FnMut(&T) -> bool>>),
    MatchStringParser(fn(&T) -> T, fn(T) -> char, Vec<char>),
    EofParser,
    MapParser(
        Rc<ParserBase2<T>>,
        Rc<RefCell<dyn FnMut(Box<dyn Any>) -> Box<dyn Any>>>,
    ),
    FlatMapParser(
        Rc<ParserBase2<T>>,
        Rc<RefCell<dyn FnMut(Box<dyn Any>) -> Rc<ParserBase2<T>>>>,
    ),
    OrderedChoice(Vec<Rc<ParserBase2<T>>>),
    UnorderedChoice(Vec<Rc<ParserBase2<T>>>),
    FilterParser(
        Rc<ParserBase2<T>>,
        Rc<RefCell<dyn FnMut(&Box<dyn Any>) -> bool>>,
    ),
    ReturnString(Rc<ParserBase2<T>>),
    Unimplemented,
}

impl<T> ParserBase2<T> {
    fn into_any_parser<Err: Clone + From<String> + 'static>(&self) -> Parser<Err, T, Box<dyn Any>>
    where
        T: Clone + Into<char> + std::fmt::Display + 'static,
    {
        fn parser_to_parser_any<Err: From<String> + 'static, T: Clone + 'static, A: 'static>(
            parser: Parser<Err, T, A>,
        ) -> Parser<Err, T, Box<dyn Any>> {
            parser.map(|x| Box::new(x) as Box<dyn Any>)
        }
        match self {
            &ParserBase2::EmptyParser => parser_to_parser_any(Parser::empty()),
            &ParserBase2::SatisfyParser(ref pred) => {
                let pred = Rc::clone(pred);
                let pred = move |t: &T| pred.borrow_mut()(t);
                parser_to_parser_any(Parser::satisfy(pred))
            }
            &ParserBase2::MatchStringParser(ref _clone_t, ref _t_to_char, ref str) => {
                let str: String = str.iter().collect();
                parser_to_parser_any(Parser::match_string(&str))
            }
            &ParserBase2::EofParser => parser_to_parser_any(Parser::eof()),
            &ParserBase2::MapParser(ref parser, ref f) => {
                let parser = parser.into_any_parser();
                let f = Rc::clone(f);
                let f = move |a| f.borrow_mut()(a);
                parser_to_parser_any(parser.map(f))
            }
            &ParserBase2::FlatMapParser(ref parser, ref cont) => {
                let parser = parser.into_any_parser();
                let cont = Rc::clone(cont);
                let cont = move |a| cont.borrow_mut()(a).into_any_parser();
                parser_to_parser_any(parser.flat_map(cont))
            }
            &ParserBase2::OrderedChoice(ref parsers) => Parser::choice(
                parsers
                    .iter()
                    .map(|parser| parser.into_any_parser())
                    .collect(),
            ),
            &ParserBase2::UnorderedChoice(ref parsers) => Parser::unordered_choice(
                parsers
                    .iter()
                    .map(|parser| parser.into_any_parser())
                    .collect(),
            ),
            &ParserBase2::FilterParser(ref parser, ref pred) => {
                let parser = parser.into_any_parser();
                let pred = Rc::clone(pred);
                let pred = move |x: &Box<dyn Any>| pred.borrow_mut()(x);
                parser.filter(pred, "filter error".to_owned().into())
            }
            &ParserBase2::ReturnString(ref parser) => {
                parser_to_parser_any(Parser::return_string(&parser.into_any_parser()))
            }
            &ParserBase2::Unimplemented => Parser::unimplemented(),
        }
    }
}

struct EmptyParser;

impl<Err: 'static, T> ParserBase<Err, T, ()> for EmptyParser {
    fn run(&self) -> Box<dyn Generator<TokenStream<T>, Yield = (), Result = Result<(), Err>>> {
        struct MyGenerator<Err> {
            err: PhantomData<Err>,
        }
        impl<Err, T> Generator<TokenStream<T>> for MyGenerator<Err> {
            type Yield = ();
            type Result = Result<(), Err>;

            fn resume(
                self: Box<Self>,
                _tokens: &mut TokenStream<T>,
            ) -> GeneratorState<TokenStream<T>, Self::Yield, Self::Result> {
                return GeneratorState::Complete(Ok(()));
            }
        }
        Box::new(MyGenerator { err: PhantomData })
    }
}

struct EofParser;

impl<Err: From<String> + 'static, T: Clone + std::fmt::Display> ParserBase<Err, T, ()>
    for EofParser
{
    fn run(&self) -> Box<dyn Generator<TokenStream<T>, Yield = (), Result = Result<(), Err>>> {
        struct MyGenerator<Err> {
            err: PhantomData<Err>,
        }
        impl<Err: From<String>, T: Clone + std::fmt::Display> Generator<TokenStream<T>>
            for MyGenerator<Err>
        {
            type Yield = ();
            type Result = Result<(), Err>;

            fn resume(
                self: Box<Self>,
                tokens: &mut TokenStream<T>,
            ) -> GeneratorState<TokenStream<T>, Self::Yield, Self::Result> {
                let t_op = tokens.read();
                if let Some(t) = t_op {
                    return GeneratorState::Complete(Err(format!(
                        "Expected end of file, but got '{}'.",
                        t
                    )
                    .into()));
                } else {
                    return GeneratorState::Complete(Ok(()));
                }
            }
        }
        Box::new(MyGenerator { err: PhantomData })
    }
}

struct SatisfyParser<T> {
    pred: Rc<RefCell<dyn FnMut(&T) -> bool>>,
}

impl<Err: From<String> + 'static, T: Clone + 'static> ParserBase<Err, T, T> for SatisfyParser<T> {
    fn run(&self) -> Box<dyn Generator<TokenStream<T>, Yield = T, Result = Result<T, Err>>> {
        struct MyGenerator<Err, T> {
            err: PhantomData<Err>,
            pred: Rc<RefCell<dyn FnMut(&T) -> bool>>,
        }
        impl<Err: From<String> + 'static, T: Clone + 'static> Generator<TokenStream<T>>
            for MyGenerator<Err, T>
        {
            type Yield = T;
            type Result = Result<T, Err>;

            fn resume(
                self: Box<Self>,
                r: &mut TokenStream<T>,
            ) -> GeneratorState<TokenStream<T>, Self::Yield, Self::Result> {
                let t_op = r.read();
                match t_op {
                    Some(t) => {
                        if self.pred.borrow_mut()(&t) {
                            return GeneratorState::Complete(Ok(t));
                        } else {
                            return GeneratorState::Complete(Err("Match predicate failed."
                                .to_owned()
                                .into()));
                        }
                    }
                    None => {
                        return GeneratorState::Complete(Err("Unexpected end of file."
                            .to_owned()
                            .into()));
                    }
                }
            }
        }
        Box::new(MyGenerator {
            err: PhantomData,
            pred: self.pred.clone(),
        })
    }
}

struct MatchStringParser {
    str: Rc<Vec<char>>,
}

impl<Err: From<String> + 'static, T: Clone + Into<char>> ParserBase<Err, T, ()>
    for MatchStringParser
{
    fn run(&self) -> Box<dyn Generator<TokenStream<T>, Yield = (), Result = Result<(), Err>>> {
        struct MyGenerator<Err> {
            err: PhantomData<Err>,
            str: Rc<Vec<char>>,
        }
        impl<Err: From<String>, T: Clone + Into<char>> Generator<TokenStream<T>> for MyGenerator<Err> {
            type Yield = ();
            type Result = Result<(), Err>;

            fn resume(
                self: Box<Self>,
                tokens: &mut TokenStream<T>,
            ) -> GeneratorState<TokenStream<T>, Self::Yield, Self::Result> {
                for c in &*self.str {
                    match tokens.read() {
                        Some(t) => {
                            let c2: char = t.into();
                            if c2 != *c {
                                return GeneratorState::Complete(Err(format!(
                                    "Expected '{}', but got '{}'.",
                                    c, c2
                                )
                                .into()));
                            }
                        }
                        None => {
                            return GeneratorState::Complete(Err(format!(
                                "Expected {}, but got end of file.",
                                c
                            )
                            .into()));
                        }
                    }
                }
                return GeneratorState::Complete(Ok(()));
            }
        }
        Box::new(MyGenerator {
            err: PhantomData,
            str: self.str.clone(),
        })
    }
}

struct OrderedChoiceParser<Err, T, A> {
    parsers: Vec<Parser<Err, T, A>>,
}

impl<Err: From<String> + Clone + 'static, T: 'static, A: 'static> ParserBase<Err, T, A>
    for OrderedChoiceParser<Err, T, A>
{
    fn run(&self) -> Box<dyn Generator<TokenStream<T>, Yield = A, Result = Result<A, Err>>> {
        struct MyGenerator<Err, T, A> {
            generators: Vec<Box<dyn Generator<TokenStream<T>, Yield = A, Result = Result<A, Err>>>>,
            save_pos_op: Option<usize>,
            last_error_op: Option<Err>,
        }
        impl<Err: From<String>, T, A> Generator<TokenStream<T>> for MyGenerator<Err, T, A> {
            type Yield = A;
            type Result = Result<A, Err>;

            fn resume(
                mut self: Box<Self>,
                tokens: &mut TokenStream<T>,
            ) -> GeneratorState<TokenStream<T>, Self::Yield, Self::Result> {
                let save_pos;
                if let Some(save_pos2) = self.save_pos_op {
                    save_pos = save_pos2;
                } else {
                    save_pos = tokens.save();
                }
                for generator in self.generators {
                    match generator.resume(tokens) {
                        GeneratorState::Yielded(a, next) => {
                            return GeneratorState::Yielded(a, next);
                        }
                        GeneratorState::Complete(r) => match r {
                            Ok(a) => {
                                return GeneratorState::Complete(Ok(a));
                            }
                            Err(error) => {
                                self.last_error_op = Some(error);
                                tokens.restore(save_pos);
                            }
                        },
                    }
                }
                if let Some(error) = self.last_error_op {
                    return GeneratorState::Complete(Err(error));
                } else {
                    return GeneratorState::Complete(Err(
                        "No parsers matched in Parser::choice().".to_owned().into(),
                    ));
                }
            }
        }
        let generators: Vec<
            Box<dyn Generator<TokenStream<T>, Yield = A, Result = Result<A, Err>>>,
        > = self
            .parsers
            .iter()
            .map(|parser| parser.base.run())
            .collect();
        Box::new(MyGenerator {
            generators,
            save_pos_op: None,
            last_error_op: None,
        })
    }
}

struct UnorderedChoiceParser<Err, T, A> {
    parsers: Vec<Parser<Err, T, A>>,
}

impl<Err: Clone + 'static, T: 'static, A: 'static> ParserBase<Err, T, A>
    for UnorderedChoiceParser<Err, T, A>
{
    fn run(&self) -> Box<dyn Generator<TokenStream<T>, Yield = A, Result = Result<A, Err>>> {
        let generators: Vec<
            Box<dyn Generator<TokenStream<T>, Yield = A, Result = Result<A, Err>>>,
        > = self
            .parsers
            .iter()
            .map(|parser| parser.base.run())
            .collect();
        Box::new(generator_or_elses(generators))
    }
}

struct FilterParser<Err, T, A> {
    parser: Parser<Err, T, A>,
    pred: Rc<RefCell<dyn FnMut(&A) -> bool>>,
    error: Err,
}

impl<Err: Clone + 'static, T: 'static, A: 'static> ParserBase<Err, T, A>
    for FilterParser<Err, T, A>
{
    fn run(&self) -> Box<dyn Generator<TokenStream<T>, Yield = A, Result = Result<A, Err>>> {
        struct MyGenerator<Err, T, A> {
            generator: Box<dyn Generator<TokenStream<T>, Yield = A, Result = Result<A, Err>>>,
            pred: Rc<RefCell<dyn FnMut(&A) -> bool>>,
            error: Err,
        }
        impl<Err: 'static, T: 'static, A: 'static> Generator<TokenStream<T>> for MyGenerator<Err, T, A> {
            type Yield = A;
            type Result = Result<A, Err>;

            fn resume(
                mut self: Box<Self>,
                tokens: &mut TokenStream<T>,
            ) -> GeneratorState<TokenStream<T>, Self::Yield, Self::Result> {
                loop {
                    match self.generator.resume(tokens) {
                        GeneratorState::Yielded(a, next) => {
                            self.generator = next;
                            if self.pred.borrow_mut()(&a) {
                                return GeneratorState::Yielded(a, self);
                            }
                        }
                        GeneratorState::Complete(r) => match r {
                            Ok(a) => {
                                if !self.pred.borrow_mut()(&a) {
                                    return GeneratorState::Complete(Err(self.error));
                                }
                                return GeneratorState::Complete(Ok(a));
                            }
                            Err(err) => {
                                return GeneratorState::Complete(Err(err));
                            }
                        },
                    }
                }
            }
        }
        Box::new(MyGenerator {
            generator: self.parser.base.run(),
            pred: Rc::clone(&self.pred),
            error: self.error.clone(),
        })
    }
}

struct MapParser<Err, T, A, B> {
    parser: Parser<Err, T, A>,
    f: Rc<RefCell<dyn FnMut(A) -> B>>,
}

impl<Err: 'static, T: 'static, A: 'static, B: 'static> ParserBase<Err, T, B>
    for MapParser<Err, T, A, B>
{
    fn run(&self) -> Box<dyn Generator<TokenStream<T>, Yield = B, Result = Result<B, Err>>> {
        struct MyGenerator<Err, T, A, B> {
            generator: Box<dyn Generator<TokenStream<T>, Yield = A, Result = Result<A, Err>>>,
            f: Rc<RefCell<dyn FnMut(A) -> B>>,
        }
        impl<Err: 'static, T: 'static, A: 'static, B: 'static> Generator<TokenStream<T>>
            for MyGenerator<Err, T, A, B>
        {
            type Yield = B;
            type Result = Result<B, Err>;

            fn resume(
                mut self: Box<Self>,
                tokens: &mut TokenStream<T>,
            ) -> GeneratorState<TokenStream<T>, Self::Yield, Self::Result> {
                match self.generator.resume(tokens) {
                    GeneratorState::Yielded(a, next) => {
                        self.generator = next;
                        let b = self.f.borrow_mut()(a);
                        return GeneratorState::Yielded(b, self);
                    }
                    GeneratorState::Complete(a) => {
                        return GeneratorState::Complete(a.map(|a2| self.f.borrow_mut()(a2)));
                    }
                }
            }
        }
        Box::new(MyGenerator {
            generator: self.parser.base.run(),
            f: self.f.clone(),
        })
    }
}

struct FlatMapParser<Err, T, A, B> {
    parser: Parser<Err, T, A>,
    cont: Rc<RefCell<dyn FnMut(A) -> Parser<Err, T, B>>>,
}

impl<Err: 'static, T: 'static, A: 'static, B: 'static> ParserBase<Err, T, B>
    for FlatMapParser<Err, T, A, B>
{
    fn run(&self) -> Box<dyn Generator<TokenStream<T>, Yield = B, Result = Result<B, Err>>> {
        let generator = self.parser.base.run();
        let cont = self.cont.clone();
        let generator_cont = move |a: A| cont.borrow_mut()(a).base.run();
        Box::new(generator_flat_map(generator, generator_cont))
    }
}

struct ReturnStringParser<Err, T, A> {
    parser: Parser<Err, T, A>,
}

impl<Err: 'static, T: Clone + Into<char> + 'static, A: 'static> ParserBase<Err, T, String>
    for ReturnStringParser<Err, T, A>
{
    fn run(
        &self,
    ) -> Box<dyn Generator<TokenStream<T>, Yield = String, Result = Result<String, Err>>> {
        struct MyGenerator<Err, T, A> {
            generator: Box<dyn Generator<TokenStream<T>, Yield = A, Result = Result<A, Err>>>,
            start_pos_op: Option<usize>,
        }
        impl<Err: 'static, T: Clone + Into<char> + 'static, A: 'static> Generator<TokenStream<T>>
            for MyGenerator<Err, T, A>
        {
            type Yield = String;
            type Result = Result<String, Err>;

            fn resume(
                mut self: Box<Self>,
                tokens: &mut TokenStream<T>,
            ) -> GeneratorState<TokenStream<T>, Self::Yield, Self::Result> {
                let start_pos;
                if let Some(start_pos2) = self.start_pos_op {
                    start_pos = start_pos2;
                } else {
                    start_pos = tokens.save();
                    self.start_pos_op = Some(start_pos);
                }
                let extract_str = |tokens: &mut TokenStream<T>| {
                    let end_pos = tokens.save();
                    tokens.restore(start_pos);
                    let mut str = "".to_owned();
                    for _i in start_pos..end_pos {
                        if let Some(t) = tokens.read() {
                            str.push(t.into());
                        }
                    }
                    tokens.restore(end_pos);
                    return str;
                };
                match self.generator.resume(tokens) {
                    GeneratorState::Yielded(_a, next) => {
                        self.generator = next;
                        return GeneratorState::Yielded(extract_str(tokens), self);
                    }
                    GeneratorState::Complete(r) => match r {
                        Ok(_a) => {
                            return GeneratorState::Complete(Ok(extract_str(tokens)));
                        }
                        Err(err) => {
                            return GeneratorState::Complete(Err(err));
                        }
                    },
                }
            }
        }
        Box::new(MyGenerator {
            generator: self.parser.base.run(),
            start_pos_op: None,
        })
    }
}

trait Generator<R> {
    type Yield;
    type Result;

    fn resume(self: Box<Self>, r: &mut R) -> GeneratorState<R, Self::Yield, Self::Result>;
}

enum GeneratorState<R, Yield, Result> {
    Yielded(Yield, Box<dyn Generator<R, Yield = Yield, Result = Result>>),
    Complete(Result),
}

fn generator_or_elses<Err: Clone + 'static, T: 'static, A: 'static>(
    mut generators: Vec<Box<dyn Generator<TokenStream<T>, Yield = A, Result = Result<A, Err>>>>,
) -> impl Generator<TokenStream<T>, Yield = A, Result = Result<A, Err>> {
    struct MyGenerator<Err, T, A> {
        generators: Vec<Box<dyn Generator<TokenStream<T>, Yield = A, Result = Result<A, Err>>>>,
        last_err: Option<Err>,
        save_pos_op: Option<usize>,
    }
    impl<'a, Err: Clone + 'static, T: 'static, A: 'static> Generator<TokenStream<T>>
        for MyGenerator<Err, T, A>
    {
        type Yield = A;
        type Result = Result<A, Err>;

        fn resume(
            mut self: Box<Self>,
            tokens: &mut TokenStream<T>,
        ) -> GeneratorState<TokenStream<T>, Self::Yield, Self::Result> {
            let save_pos;
            if let Some(save_pos2) = self.save_pos_op {
                save_pos = save_pos2;
            } else {
                save_pos = tokens.save();
                self.save_pos_op = Some(save_pos);
            }
            loop {
                tokens.restore(save_pos);
                if self.generators.is_empty() {
                    return GeneratorState::Complete(Result::Err(self.last_err.clone().unwrap()));
                }
                let generator = self.generators.pop().unwrap();
                match generator.resume(tokens) {
                    GeneratorState::Yielded(r, next) => {
                        self.generators.push(next);
                        return GeneratorState::Yielded(r, self);
                    }
                    GeneratorState::Complete(r) => match r {
                        Ok(r2) => {
                            if self.generators.is_empty() {
                                return GeneratorState::Complete(Ok(r2));
                            } else {
                                return GeneratorState::Yielded(r2, self);
                            }
                        }
                        Err(error) => {
                            if self.generators.is_empty() {
                                return GeneratorState::Complete(Err(error));
                            }
                            self.last_err = Some(error);
                        }
                    },
                }
            }
        }
    }
    generators.reverse();
    MyGenerator {
        generators,
        last_err: None,
        save_pos_op: None,
    }
}

fn generator_flat_map<
    Err: 'static,
    Tokens: 'static,
    A: 'static,
    B: 'static,
    Cont: FnMut(A) -> Box<dyn Generator<Tokens, Yield = B, Result = Result<B, Err>>> + 'static,
>(
    generator: Box<dyn Generator<Tokens, Yield = A, Result = Result<A, Err>>>,
    cont: Cont,
) -> impl Generator<Tokens, Yield = B, Result = Result<B, Err>> {
    struct MyGenerator<Err, Tokens, A, B, Cont> {
        b: PhantomData<B>,
        generator: Box<dyn Generator<Tokens, Yield = A, Result = Result<A, Err>>>,
        cont: Cont,
        generator_out_op: Option<Box<dyn Generator<Tokens, Yield = B, Result = Result<B, Err>>>>,
        last_err: Option<Err>,
    }
    impl<
            'a,
            Err: 'static,
            Tokens: 'static,
            A: 'static,
            B: 'static,
            Cont: FnMut(A) -> Box<dyn Generator<Tokens, Yield = B, Result = Result<B, Err>>> + 'static,
        > Generator<Tokens> for MyGenerator<Err, Tokens, A, B, Cont>
    {
        type Yield = B;
        type Result = Result<B, Err>;

        fn resume(
            mut self: Box<Self>,
            tokens: &mut Tokens,
        ) -> GeneratorState<Tokens, Self::Yield, Self::Result> {
            loop {
                let mut generator_out_op = None;
                std::mem::swap(&mut generator_out_op, &mut self.generator_out_op);
                if let Some(generator_out) = generator_out_op {
                    match generator_out.resume(tokens) {
                        GeneratorState::Yielded(r, next) => {
                            self.generator_out_op = Some(next);
                            return GeneratorState::Yielded(r, self);
                        }
                        GeneratorState::Complete(r) => match r {
                            Ok(r2) => {
                                return GeneratorState::Yielded(r2, self);
                            }
                            Err(error) => {
                                self.last_err = Some(error);
                            }
                        },
                    }
                }
                debug_assert!(self.generator_out_op.is_none());
                match self.generator.resume(tokens) {
                    GeneratorState::Yielded(r, next) => {
                        self.generator = next;
                        self.generator_out_op = Some((self.cont)(r));
                    }
                    GeneratorState::Complete(r) => match r {
                        Ok(r2) => {
                            let next = (self.cont)(r2);
                            return next.resume(tokens);
                        }
                        Err(error) => {
                            return GeneratorState::Complete(Err(error));
                        }
                    },
                }
            }
        }
    }
    MyGenerator {
        b: PhantomData,
        generator,
        cont,
        generator_out_op: None,
        last_err: None,
    }
}
