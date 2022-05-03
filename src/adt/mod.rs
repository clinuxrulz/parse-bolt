use std::cell::RefCell;

pub enum Parser<T> {
    Sequence(Vec<Parser<T>>),
    OrElses(Vec<Parser<T>>),
    Empty,
    Satify(RefCell<Box<dyn FnMut(&T) -> bool>>),
    Reference(usize),
    Recursive(RefCell<Box<dyn FnMut(usize) -> Parser<T>>>),
    Fork(Box<Parser<T>>, RefCell<Box<dyn FnMut(usize) -> Parser<T>>>),
}

impl<T> Parser<T> {
    pub fn sequence(parsers: Vec<Parser<T>>) -> Parser<T> {
        Parser::Sequence(parsers)
    }

    pub fn or_elses(parsers: Vec<Parser<T>>) -> Parser<T> {
        Parser::OrElses(parsers)
    }

    pub fn empty() -> Parser<T> {
        Parser::Empty
    }

    pub fn satify<Pred: FnMut(&T) -> bool + 'static>(pred: Pred) -> Parser<T> {
        Parser::Satify(RefCell::new(Box::new(pred)))
    }

    pub fn reference(id: usize) -> Parser<T> {
        Parser::Reference(id)
    }

    pub fn recursive<FN: FnMut(usize) -> Parser<T> + 'static>(f: FN) -> Parser<T> {
        Parser::Recursive(RefCell::new(Box::new(f)))
    }

    pub fn fork<FN: FnMut(usize) -> Parser<T> + 'static>(self, f: FN) -> Parser<T> {
        Parser::Fork(Box::new(self), RefCell::new(Box::new(f)))
    }

    pub fn match_(t: T) -> Parser<T>
    where
        T: PartialEq + 'static,
    {
        Parser::satify(move |t2| *t2 == t)
    }

    pub fn zero_or_more(self) -> Parser<T>
    where
        T: 'static,
    {
        self.fork(|self_id| {
            Parser::recursive(move |id| {
                Parser::OrElses(vec![
                    Parser::Empty,
                    Parser::Sequence(vec![Parser::reference(self_id), Parser::reference(id)]),
                ])
            })
        })
    }

    pub fn one_or_more(self) -> Parser<T>
    where
        T: 'static,
    {
        self.fork(|self_id| {
            Parser::recursive(move |id| {
                Parser::OrElses(vec![
                    Parser::reference(self_id),
                    Parser::Sequence(vec![Parser::reference(self_id), Parser::reference(id)]),
                ])
            })
        })
    }
}
