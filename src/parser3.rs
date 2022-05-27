use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::marker::PhantomData;
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

pub trait TokenClass {
    type Result;
    fn token_class(&self) -> Self::Result;
}

pub struct Parser<Err, T, TC, A> {
    phantom_err: PhantomData<Err>,
    phantom_t: PhantomData<T>,
    phantom_a: PhantomData<A>,
    base: Rc<ParserBase<TC>>,
}

impl<Err, T, TC, A> Clone for Parser<Err, T, TC, A> {
    fn clone(&self) -> Self {
        Parser {
            phantom_err: PhantomData,
            phantom_t: PhantomData,
            phantom_a: PhantomData,
            base: Rc::clone(&self.base),
        }
    }
}

#[derive(Debug)]
pub struct ParserRunner<Err, T, TC, A> {
    phantom_err: PhantomData<Err>,
    phantom_t: PhantomData<T>,
    phantom_a: PhantomData<A>,
    eof_sym: TC,
    lr1_parser: crate::lr1_parser::Lr1Parser<RuleOrToken<TC>>,
    result_fetched: bool,
}

impl<Err, T, TC, A> ParserRunner<Err, T, TC, A> {
    pub fn advance(&mut self, token: T) -> Result<bool, Err>
    where
        Err: From<String>,
        T: TokenClass<Result = TC> + 'static,
        TC: Clone + PartialEq + Eq + std::hash::Hash + PartialOrd + Ord + std::fmt::Display,
    {
        self.lr1_parser
            .advance(
                &RuleOrToken::Token(token.token_class()),
                if token.token_class() == self.eof_sym {
                    None
                } else {
                    Some(Box::new(token) as Box<dyn Any>)
                },
            )
            .map_err(Err::from)
    }

    pub fn is_finished(&self) -> bool {
        self.lr1_parser.is_finished()
    }

    pub fn get_result(&mut self) -> A
    where
        A: 'static,
    {
        if self.result_fetched {
            panic!("Result already extracted.");
        }
        let result: Box<A> = self
            .lr1_parser
            .get_value_stack_mut()
            .pop()
            .unwrap()
            .downcast()
            .ok()
            .unwrap();
        self.result_fetched = true;
        return *result;
    }
}

impl<Err, T, TC, A> Parser<Err, T, TC, A> {
    fn wrap_base(base: ParserBase<TC>) -> Parser<Err, T, TC, A> {
        Parser::wrap_base_(Rc::new(base))
    }

    fn wrap_base_(base: Rc<ParserBase<TC>>) -> Parser<Err, T, TC, A> {
        Parser {
            phantom_err: PhantomData,
            phantom_t: PhantomData,
            phantom_a: PhantomData,
            base,
        }
    }

    pub fn compile(&self, eof_sym: &TC) -> ParserRunner<Err, T, TC, A>
    where
        TC: Clone + std::fmt::Debug + std::fmt::Display + PartialEq + Eq + std::hash::Hash + PartialOrd + Ord,
    {
        let grammar = self.base.generate_grammar(&eof_sym);
        for rule in &grammar {
            println!("{}", rule);
        }
        let table = crate::lr1_parser::make_table(&grammar, &RuleOrToken::Token(TC::clone(eof_sym)));
        let lr1_parser = crate::lr1_parser::Lr1Parser::new(table);
        ParserRunner {
            phantom_err: PhantomData,
            phantom_t: PhantomData,
            phantom_a: PhantomData,
            eof_sym: TC::clone(eof_sym),
            lr1_parser: lr1_parser,
            result_fetched: false,
        }
    }

    pub fn map<B: 'static, F: FnMut(A) -> B + 'static>(&self, mut f: F) -> Parser<Err, T, TC, B>
    where
        A: 'static,
    {
        Parser::wrap_base(ParserBase::AndThenEffect {
            parser: Rc::clone(&self.base),
            effect: Rc::new(RefCell::new(move |stack: &mut Vec<Box<dyn Any>>| {
                let a: Box<A> = stack.pop().unwrap().downcast().ok().unwrap();
                stack.push(Box::new(f(*a)));
            })),
        })
    }

    pub fn map_to<B: Clone + 'static>(&self, b: B) -> Parser<Err, T, TC, B>
    where
        A: 'static,
    {
        self.map(move |_| b.clone())
    }

    pub fn seq2<B: 'static>(&self, parser2: &Parser<Err, T, TC, B>) -> Parser<Err, T, TC, (A, B)>
    where
        A: 'static,
    {
        Parser::wrap_base(ParserBase::AndThenEffect {
            parser: Rc::new(ParserBase::Seq {
                parsers: vec![Rc::clone(&self.base), Rc::clone(&parser2.base)],
            }),
            effect: Rc::new(RefCell::new(|stack: &mut Vec<Box<dyn Any>>| {
                let rhs: Box<B> = stack.pop().unwrap().downcast().ok().unwrap();
                let lhs: Box<A> = stack.pop().unwrap().downcast().ok().unwrap();
                stack.push(Box::new((*lhs, *rhs)));
            })),
        })
    }

    pub fn seq_left<B: 'static>(&self, parser2: &Parser<Err, T, TC, B>) -> Parser<Err, T, TC, A>
    where
        A: 'static,
    {
        self.seq2(parser2).map(|(x, _)| x)
    }

    pub fn seq_right<B: 'static>(&self, parser2: &Parser<Err, T, TC, B>) -> Parser<Err, T, TC, B>
    where
        A: 'static,
    {
        self.seq2(parser2).map(|(_, x)| x)
    }

    pub fn choice<'a>(parsers: Vec<&'a Parser<Err, T, TC, A>>) -> Parser<Err, T, TC, A> {
        Parser::wrap_base(ParserBase::Choice {
            parsers: parsers
                .iter()
                .map(|parser| Rc::clone(&(*parser).base))
                .collect(),
        })
    }

    pub fn optional(&self) -> Parser<Err, T, TC, Option<A>>
    where
        A: 'static,
    {
        Parser::choice(vec![
            &Parser::empty().map(|_| None),
            &self.map(Some),
        ])
    }

    pub fn many1(&self) -> Parser<Err, T, TC, Vec<A>>
    where
        Err: 'static,
        T: 'static,
        TC: 'static,
        A: 'static,
    {
        Parser::fix_point(|result: &Parser<Err, T, TC, Vec<A>>| {
            Parser::choice(vec![
                &self.map(|x| vec![x]),
                &result.seq2(self).map(|(mut xs, x)| {
                    xs.push(x);
                    xs
                }),
            ])
        })
    }

    pub fn many0(&self) -> Parser<Err, T, TC, Vec<A>>
    where
        Err: 'static,
        T: 'static,
        TC: 'static,
        A: 'static,
    {
        Parser::choice(vec![
            &Parser::empty().map(|_| Vec::new()),
            &self.many1()
        ])
    }

    pub fn many1_sep<B>(&self, sep: &Parser<Err, T, TC, B>) -> Parser<Err, T, TC, Vec<A>>
    where
        Err: 'static,
        T: 'static,
        TC: 'static,
        A: 'static,
        B: 'static,
    {
        self
            .seq2(&sep.seq_right(self).many0())
            .map(|(x, mut xs)| { xs.insert(0, x); xs })
    }

    pub fn fix_point<Fix: FnOnce(&Parser<Err, T, TC, A>) -> Parser<Err, T, TC, A>>(
        fix: Fix,
    ) -> Parser<Err, T, TC, A> {
        let ref_name = ParserReferenceName::new();
        Parser::wrap_base(ParserBase::FixPoint(ref_name.clone(), fix(&Parser::wrap_base(ParserBase::Reference { name: ref_name })).base))
    }
}

impl<Err, T, TC> Parser<Err, T, TC, T> {
    pub fn match_(sym: TC) -> Parser<Err, T, TC, T> {
        Parser::wrap_base(ParserBase::Match { sym })
    }
}

impl<Err, T, TC> Parser<Err, T, TC, ()> {
    pub fn empty() -> Parser<Err, T, TC, ()> {
        Parser::wrap_base(
            ParserBase::AndThenEffect {
                parser: Rc::new(ParserBase::Seq {
                    parsers: Vec::new(),
                }),
                effect: Rc::new(RefCell::new(|stack: &mut Vec<Box<dyn Any>>| {
                    stack.push(Box::new(()));
                }))
            }
        )
    }
}

struct GrammarNameGen<S> {
    next_id: usize,
    grammar_to_name: HashMap<ParserBase<S>, usize>,
}

impl<S> GrammarNameGen<S> {
    fn new() -> GrammarNameGen<S> {
        GrammarNameGen {
            next_id: 0,
            grammar_to_name: HashMap::new(),
        }
    }
}

impl<S> GrammarNameGen<S> {
    fn gen_fresh_id(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        return id;
    }

    fn gen_name(&mut self, parser: &ParserBase<S>, fix_point_map: &mut HashMap<ParserReferenceName,RuleOrToken<S>>) -> (RuleOrToken<S>, bool)
    where
        S: Clone + PartialEq + Eq + std::hash::Hash,
    {
        match parser {
            ParserBase::Match { sym } => {
                return (RuleOrToken::Token(S::clone(sym)), false);
            }
            ParserBase::Seq { parsers: _ } => {
                if let Some(id) = self.grammar_to_name.get(parser) {
                    return (RuleOrToken::Rule(*id), false);
                }
                let id = self.next_id;
                self.next_id += 1;
                self.grammar_to_name.insert(ParserBase::clone(parser), id);
                return (RuleOrToken::Rule(id), true);
            }
            ParserBase::Choice { parsers: _ } => {
                if let Some(id) = self.grammar_to_name.get(parser) {
                    return (RuleOrToken::Rule(*id), false);
                }
                let id = self.next_id;
                self.next_id += 1;
                self.grammar_to_name.insert(ParserBase::clone(parser), id);
                return (RuleOrToken::Rule(id), true);
            }
            ParserBase::AndThenEffect {
                parser: _,
                effect: _,
            } => {
                if let Some(id) = self.grammar_to_name.get(parser) {
                    return (RuleOrToken::Rule(*id), false);
                }
                let id = self.next_id;
                self.next_id += 1;
                self.grammar_to_name.insert(ParserBase::clone(parser), id);
                return (RuleOrToken::Rule(id), true);
            }
            ParserBase::Reference { name } => {
                return (RuleOrToken::clone(fix_point_map.get(name).unwrap()), false);
            }
            ParserBase::FixPoint(_, _) => {
                if let Some(id) = self.grammar_to_name.get(parser) {
                    return (RuleOrToken::Rule(*id), false);
                }
                let id = self.next_id;
                self.next_id += 1;
                self.grammar_to_name.insert(ParserBase::clone(parser), id);
                return (RuleOrToken::Rule(id), true);
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum RuleOrToken<S> {
    Rule(usize),
    Token(S),
}

impl<S: std::fmt::Display> std::fmt::Display for RuleOrToken<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Rule(id) => write!(f, "rule_{}", id)?,
            Self::Token(s) => s.fmt(f)?,
        }
        Ok(())
    }
}

pub struct ParserReferenceName {
    x: Rc<u32>,
}

impl ParserReferenceName {
    pub fn new() -> ParserReferenceName {
        ParserReferenceName {
            x: Rc::new(0),
        }
    }
}

impl Clone for ParserReferenceName {
    fn clone(&self) -> Self {
        Self { x: Rc::clone(&self.x) }
    }
}

impl PartialEq for ParserReferenceName {
    fn eq(&self, other: &Self) -> bool {
        let lhs: *const u32 = &*self.x;
        let rhs: *const u32 = &*other.x;
        let lhs = lhs as usize;
        let rhs = rhs as usize;
        return lhs == rhs;
    }
}

impl Eq for ParserReferenceName {}

impl std::hash::Hash for ParserReferenceName {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let x: *const u32 = &*self.x;
        let x: usize = x as usize;
        x.hash(state);
    }
}

enum ParserBase<S> {
    Match {
        sym: S,
    },
    Seq {
        parsers: Vec<Rc<ParserBase<S>>>,
    },
    Choice {
        parsers: Vec<Rc<ParserBase<S>>>,
    },
    AndThenEffect {
        parser: Rc<ParserBase<S>>,
        effect: Rc<RefCell<dyn FnMut(&mut Vec<Box<dyn Any>>)>>,
    },
    Reference {
        name: ParserReferenceName,
    },
    FixPoint(ParserReferenceName, Rc<ParserBase<S>>),
}

impl<S: PartialEq> PartialEq for ParserBase<S> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Match { sym: l_sym }, Self::Match { sym: r_sym }) => *l_sym == *r_sym,
            (Self::Seq { parsers: l_parsers }, Self::Seq { parsers: r_parsers }) => {
                *l_parsers == *r_parsers
            }
            (Self::Choice { parsers: l_parsers }, Self::Choice { parsers: r_parsers }) => {
                *l_parsers == *r_parsers
            }
            (
                Self::AndThenEffect {
                    parser: l_parser,
                    effect: l_effect,
                },
                Self::AndThenEffect {
                    parser: r_parser,
                    effect: r_effect,
                },
            ) => {
                let l_ptr: *const RefCell<dyn FnMut(&mut Vec<Box<dyn Any>>)> = &**l_effect;
                let r_ptr: *const RefCell<dyn FnMut(&mut Vec<Box<dyn Any>>)> = &**r_effect;
                *l_parser == *r_parser && l_ptr == r_ptr
            }
            (Self::Reference { name: l_name }, Self::Reference { name: r_name }) => {
                *l_name == *r_name
            }
            (Self::FixPoint(l_name, l_parser), Self::FixPoint(r_name, r_parser)) => {
                l_name == r_name && l_parser == r_parser
            }
            _ => false,
        }
    }
}

impl<S: PartialEq + Eq> Eq for ParserBase<S> {}

impl<S: PartialEq + Eq + std::hash::Hash> std::hash::Hash for ParserBase<S> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Match { sym } => {
                state.write_usize(0);
                sym.hash(state);
            }
            Self::Seq { parsers } => {
                state.write_usize(1);
                parsers.hash(state);
            }
            Self::Choice { parsers } => {
                state.write_usize(2);
                parsers.hash(state);
            }
            Self::AndThenEffect { parser, effect } => {
                state.write_usize(3);
                parser.hash(state);
                let ptr: *const RefCell<dyn FnMut(&mut Vec<Box<dyn Any>>)> = &**effect;
                ptr.hash(state);
            }
            Self::Reference { name } => {
                state.write_usize(4);
                name.hash(state);
            }
            Self::FixPoint(name, parser) => {
                state.write_usize(5);
                name.hash(state);
                parser.hash(state);
            }
        }
        core::mem::discriminant(self).hash(state);
    }
}

impl<S: Clone> Clone for ParserBase<S> {
    fn clone(&self) -> Self {
        match self {
            ParserBase::Match { sym } => ParserBase::Match { sym: S::clone(sym) },
            ParserBase::Seq { parsers } => ParserBase::Seq {
                parsers: parsers.iter().map(Rc::clone).collect(),
            },
            ParserBase::Choice { parsers } => ParserBase::Choice {
                parsers: parsers.iter().map(Rc::clone).collect(),
            },
            ParserBase::AndThenEffect { parser, effect } => ParserBase::AndThenEffect {
                parser: Rc::clone(parser),
                effect: Rc::clone(effect),
            },
            ParserBase::Reference { name } => ParserBase::Reference { name: name.clone(), },
            ParserBase::FixPoint(name, parser) => ParserBase::FixPoint(name.clone(), Rc::clone(parser)),
        }
    }
}

impl<S> ParserBase<S> {
    fn generate_grammar(&self, eof_sym: &S) -> Vec<crate::lr1_parser::Rule<RuleOrToken<S>>>
    where
        S: Clone + PartialEq + Eq + std::hash::Hash,
    {
        let mut name_gen = GrammarNameGen::new();
        let mut rules = Vec::new();
        let gap = crate::lr1_parser::Rule::new(None, Vec::new(), None);
        rules.push(gap);
        let mut fix_point_map = HashMap::new();
        self.generate_grammar_(&mut name_gen, &mut rules, &mut fix_point_map);
        rules[0] = crate::lr1_parser::Rule::new(None, vec![name_gen.gen_name(self, &mut fix_point_map).0, RuleOrToken::Token(S::clone(eof_sym))], None);
        rules
    }

    fn generate_grammar_(
        &self,
        name_gen: &mut GrammarNameGen<S>,
        rules_out: &mut Vec<crate::lr1_parser::Rule<RuleOrToken<S>>>,
        fix_point_map: &mut HashMap<ParserReferenceName,RuleOrToken<S>>,
    ) where
        S: Clone + PartialEq + Eq + std::hash::Hash,
    {
        match self {
            ParserBase::Match { sym: _ } => {}
            ParserBase::Seq { parsers } => {
                let (name, is_new) = name_gen.gen_name(self, fix_point_map);
                if !is_new {
                    return;
                }
                let gap_idx = rules_out.len();
                let gap = crate::lr1_parser::Rule::new(None, Vec::new(), None);
                rules_out.push(gap);
                let mut parts = Vec::new();
                for parser in parsers {
                    parser.generate_grammar_(name_gen, rules_out, fix_point_map);
                    let (part, _) = name_gen.gen_name(parser, fix_point_map);
                    parts.push(part);
                }
                let rule = crate::lr1_parser::Rule::new(
                    Some(name),
                    parts,
                    None, // <-- effect is none because we are gonna leave all results on the stack for the AndThenEffect of from Parser::seq to manage.
                );
                rules_out[gap_idx] = rule;
            }
            ParserBase::Choice { parsers } => {
                let (name, is_new) = name_gen.gen_name(self, fix_point_map);
                if !is_new {
                    return;
                }
                for parser in parsers {
                    parser.generate_grammar_(name_gen, rules_out, fix_point_map);
                    let (choice_name, _) = name_gen.gen_name(parser, fix_point_map);
                    let rule = crate::lr1_parser::Rule::new(
                        Some(RuleOrToken::clone(&name)),
                        vec![choice_name],
                        None,
                    );
                    rules_out.push(rule);
                }
            }
            ParserBase::AndThenEffect { parser, effect } => {
                let (name, is_new) = name_gen.gen_name(self, fix_point_map);
                if !is_new {
                    return;
                }
                let gap_idx = rules_out.len();
                let gap = crate::lr1_parser::Rule::new(None, Vec::new(), None);
                rules_out.push(gap);
                parser.generate_grammar_(name_gen, rules_out, fix_point_map);
                let (inner_name, _) = name_gen.gen_name(parser, fix_point_map);
                rules_out[gap_idx] = crate::lr1_parser::Rule::new(
                    Some(name),
                    vec![inner_name],
                    Some(Rc::clone(effect)),
                );
            }
            ParserBase::Reference { name } => {
                return;
            }
            ParserBase::FixPoint(ref_name, parser) => {
                let (name, is_new) = name_gen.gen_name(self, fix_point_map);
                if !is_new {
                    return;
                }
                fix_point_map.insert(ParserReferenceName::clone(ref_name), RuleOrToken::clone(&name));
                let gap_idx = rules_out.len();
                let gap = crate::lr1_parser::Rule::new(None, Vec::new(), None);
                rules_out.push(gap);
                parser.generate_grammar_(name_gen, rules_out, fix_point_map);
                let (inner_name, _) = name_gen.gen_name(&*parser, fix_point_map);
                rules_out[gap_idx] =
                    crate::lr1_parser::Rule::new(Some(name), vec![inner_name], None);
            }
        }
    }

    fn find_lexemes(self: &Rc<ParserBase<S>>) -> Vec<S>
    where
        S: Clone + PartialEq + Eq + std::hash::Hash,
    {
        let mut result: HashSet<S> = HashSet::new();
        let mut stack = Vec::new();
        stack.push(Rc::clone(self));
        while let Some(at) = stack.pop() {
            match &*at {
                ParserBase::Match { sym } => {
                    result.insert(S::clone(sym));
                }
                ParserBase::Seq { parsers } => {
                    for parser in parsers.iter().rev() {
                        stack.push(Rc::clone(parser));
                    }
                }
                ParserBase::Choice { parsers } => {
                    for parser in parsers.iter().rev() {
                        stack.push(Rc::clone(parser));
                    }
                }
                ParserBase::AndThenEffect { parser, effect: _ } => {
                    stack.push(Rc::clone(parser));
                }
                ParserBase::Reference { name: _ } => {}
                ParserBase::FixPoint(name, parser) => {
                    stack.push(Rc::clone(parser));
                }
            }
        }
        return result.drain().collect();
    }
}

#[test]
fn test_combinator_to_grammar() {
    let combinator = ParserBase::Seq {
        parsers: vec![
            Rc::new(ParserBase::Match { sym: 'A' }),
            Rc::new(ParserBase::Match { sym: 'B' }),
            Rc::new(ParserBase::Choice {
                parsers: vec![
                    Rc::new(ParserBase::Match { sym: 'C' }),
                    Rc::new(ParserBase::Match { sym: 'D' }),
                ],
            }),
        ],
    };
    let rules = combinator.generate_grammar(&'$');
    println!("Rules:");
    for i in 0..rules.len() {
        println!("  {}: {}", i, rules[i]);
    }
    println!();
    let table = crate::lr1_parser::make_table(&rules, &RuleOrToken::Token('$'));
    println!("Parser Table:");
    println!("{:?}", table);
}

#[test]
fn test_build_parser() {
    #[derive(Debug)]
    struct Token(char);
    impl TokenClass for Token {
        type Result = char;
        fn token_class(&self) -> Self::Result {
            return self.0;
        }
    }
    let parser: Parser<String, Token, char, _> = Parser::match_('A')
        .seq2(&Parser::match_('B'))
        .seq2(&Parser::choice(vec![
            &Parser::match_('C'),
            &Parser::match_('D'),
        ]));
    let mut parser_runner = parser.compile(&'$');
    let _ = parser_runner.advance(Token('A'));
    let _ = parser_runner.advance(Token('B'));
    let _ = parser_runner.advance(Token('C'));
    let _ = parser_runner.advance(Token('$'));
    let _ = parser_runner.advance(Token('$'));
    if parser_runner.is_finished() {
        println!("{:?}", parser_runner.get_result());
    } else {
        println!("more input required.");
    }
}

#[test]
fn test_parser_many0() {
    #[derive(Debug)]
    struct Token(char);
    impl TokenClass for Token {
        type Result = char;
        fn token_class(&self) -> Self::Result {
            return self.0;
        }
    }
    let parser: Parser<String, Token, char, _> =
        Parser::match_('A').seq2(
            &Parser::match_(',')
                .seq_right(&Parser::match_('A'))
                .many0()
        );
    let mut parser_runner = parser.compile(&'$');
    let _ = parser_runner.advance(Token('A'));
    let _ = parser_runner.advance(Token(','));
    let _ = parser_runner.advance(Token('A'));
    let _ = parser_runner.advance(Token('$'));
    let _ = parser_runner.advance(Token('$'));
    if parser_runner.is_finished() {
        println!("{:?}", parser_runner.get_result());
    } else {
        println!("more input required.");
    }
}
