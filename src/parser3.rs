use super::TokenStream;
use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
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

pub struct Parser<Err, T, A> {
    phantom_err: PhantomData<Err>,
    phantom_a: PhantomData<A>,
    base: Rc<ParserBase<T>>,
}

impl<Err, T, A> Parser<Err, T, A> {
    fn wrap_base(base: ParserBase<T>) -> Parser<Err, T, A> {
        Parser {
            phantom_err: PhantomData,
            phantom_a: PhantomData,
            base: Rc::new(base),
        }
    }

    pub fn seq2<B>(&self, parser2: &Parser<Err, T, B>) -> Parser<Err, T, (A, B)> {
        Parser::wrap_base(ParserBase::Seq {
            parsers: vec![Rc::clone(&self.base), Rc::clone(&parser2.base)],
        })
    }

    pub fn choice(parsers: Vec<Parser<Err, T, A>>) -> Parser<Err, T, A> {
        Parser::wrap_base(ParserBase::Choice {
            parsers: parsers
                .iter()
                .map(|parser| Rc::clone(&parser.base))
                .collect(),
        })
    }
}

impl<Err, T> Parser<Err, T, T> {
    pub fn match_(t: T) -> Parser<Err, T, T> {
        Parser::wrap_base(ParserBase::Match { sym: t })
    }
}

impl<Err, T> Parser<Err, T, ()> {
    pub fn empty() -> Parser<Err, T, ()> {
        Parser::wrap_base(ParserBase::Seq {
            parsers: Vec::new(),
        })
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
    fn gen_name(&mut self, parser: &ParserBase<S>) -> (RuleOrToken<S>, bool)
    where
        S: Clone + PartialEq + Eq + std::hash::Hash,
    {
        match parser {
            ParserBase::Match { sym } => {
                return (RuleOrToken::Token(S::clone(sym)), false);
            }
            ParserBase::Seq { parsers } => {
                if let Some(id) = self.grammar_to_name.get(parser) {
                    return (RuleOrToken::Rule(*id), false);
                }
                let id = self.next_id;
                self.next_id += 1;
                self.grammar_to_name.insert(ParserBase::clone(parser), id);
                return (RuleOrToken::Rule(id), true);
            }
            ParserBase::Choice { parsers } => {
                if let Some(id) = self.grammar_to_name.get(parser) {
                    return (RuleOrToken::Rule(*id), false);
                }
                let id = self.next_id;
                self.next_id += 1;
                self.grammar_to_name.insert(ParserBase::clone(parser), id);
                return (RuleOrToken::Rule(id), true);
            },
            ParserBase::AndThenEffect { parser, effect } => {
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum RuleOrToken<S> {
    Rule(usize),
    Token(S),
}

enum ParserBase<S> {
    Match { sym: S },
    Seq { parsers: Vec<Rc<ParserBase<S>>> },
    Choice { parsers: Vec<Rc<ParserBase<S>>> },
    AndThenEffect { parser: Rc<ParserBase<S>>, effect: Rc<RefCell<dyn FnMut(&mut Vec<Box<dyn Any>>)>> }
}

impl<S: PartialEq> PartialEq for ParserBase<S> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Match { sym: l_sym }, Self::Match { sym: r_sym }) => *l_sym == *r_sym,
            (Self::Seq { parsers: l_parsers }, Self::Seq { parsers: r_parsers }) => *l_parsers == *r_parsers,
            (Self::Choice { parsers: l_parsers }, Self::Choice { parsers: r_parsers }) => *l_parsers == *r_parsers,
            (Self::AndThenEffect { parser: l_parser, effect: l_effect }, Self::AndThenEffect { parser: r_parser, effect: r_effect }) => {
                let l_ptr: *const RefCell<dyn FnMut(&mut Vec<Box<dyn Any>>)> = &**l_effect;
                let r_ptr: *const RefCell<dyn FnMut(&mut Vec<Box<dyn Any>>)> = &**r_effect;
                *l_parser == *r_parser && l_ptr == r_ptr
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
            },
            Self::Seq { parsers } => {
                state.write_usize(1);
                parsers.hash(state);
            },
            Self::Choice { parsers } => {
                state.write_usize(2);
                parsers.hash(state);
            },
            Self::AndThenEffect { parser, effect, } => {
                state.write_usize(3);
                parser.hash(state);
                let ptr: *const RefCell<dyn FnMut(&mut Vec<Box<dyn Any>>)> = &**effect;
                ptr.hash(state);
            }
        }
        core::mem::discriminant(self).hash(state);
    }
}

impl<S> ParserBase<S> {
    pub fn optimise(&mut self)
    where
        S: Clone,
    {
        fn optimise_<S: Clone>(a: ParserBase<S>) -> ParserBase<S> {
            match a {
                ParserBase::Match { sym: _ } => {
                    return a;
                }
                ParserBase::Seq { parsers } => {
                    let parsers2: Vec<ParserBase<S>> = parsers
                        .iter()
                        .map(|parser| optimise_(ParserBase::clone(&**parser)))
                        .collect();
                    let mut parsers3 = Vec::new();
                    for parser in parsers2 {
                        match parser {
                            ParserBase::Seq { parsers: parsers4 } => {
                                for parser2 in parsers4 {
                                    parsers3.push(parser2);
                                }
                            }
                            _ => {
                                parsers3.push(Rc::new(parser));
                            }
                        }
                    }
                    return ParserBase::Seq { parsers: parsers3 };
                }
                ParserBase::Choice { parsers } => {
                    let parsers2: Vec<ParserBase<S>> = parsers
                        .iter()
                        .map(|parser| optimise_(ParserBase::clone(&**parser)))
                        .collect();
                    let mut parsers3 = Vec::new();
                    for parser in parsers2 {
                        match parser {
                            ParserBase::Choice { parsers: parsers4 } => {
                                for parser2 in parsers4 {
                                    parsers3.push(parser2);
                                }
                            }
                            _ => {
                                parsers3.push(Rc::new(parser));
                            }
                        }
                    }
                    return ParserBase::Choice { parsers: parsers3 };
                },
                ParserBase::AndThenEffect { parser: _, effect: _, } => {
                    return a;
                }
            }
        }
        let mut tmp: ParserBase<S> = ParserBase::Seq { parsers: vec![] };
        std::mem::swap(&mut tmp, self);
        tmp = optimise_(tmp);
        *self = tmp;
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
            }
        }
    }
}

impl<S> ParserBase<S> {
    fn generate_grammar(&self) -> Vec<crate::lr_parser::Rule<RuleOrToken<S>>>
    where
        S: Clone + PartialEq + Eq + std::hash::Hash,
    {
        let mut name_gen = GrammarNameGen::new();
        let mut rules = Vec::new();
        let gap = crate::lr_parser::Rule::new(None, Vec::new(), None);
        rules.push(gap);
        self.generate_grammar_(&mut name_gen, &mut rules);
        rules[0] = crate::lr_parser::Rule::new(None, vec![name_gen.gen_name(self).0], None);
        rules
    }

    fn generate_grammar_(
        &self,
        name_gen: &mut GrammarNameGen<S>,
        rules_out: &mut Vec<crate::lr_parser::Rule<RuleOrToken<S>>>,
    ) where
        S: Clone + PartialEq + Eq + std::hash::Hash,
    {
        match self {
            ParserBase::Match { sym: _ } => {}
            ParserBase::Seq { parsers } => {
                let (name, is_new) = name_gen.gen_name(self);
                if !is_new {
                    return;
                }
                let gap_idx = rules_out.len();
                let gap = crate::lr_parser::Rule::new(None, Vec::new(), None);
                rules_out.push(gap);
                let mut parts = Vec::new();
                for parser in parsers {
                    parser.generate_grammar_(name_gen, rules_out);
                    let (part, _) = name_gen.gen_name(parser);
                    parts.push(part);
                }
                let num_parsers = parsers.len();
                let rule = crate::lr_parser::Rule::new(
                    Some(name),
                    parts,
                    Some(Rc::new(RefCell::new(move |value_stack: &mut Vec<Box<dyn Any>>| {
                        let mut result = Vec::new();
                        for _i in 0..num_parsers {
                            result.push(value_stack.pop().unwrap());
                        }
                        result.reverse();
                        value_stack.push(Box::new(result) as Box<dyn Any>);
                    })))
                );
                rules_out[gap_idx] = rule;
            }
            ParserBase::Choice { parsers } => {
                let (name, is_new) = name_gen.gen_name(self);
                if !is_new {
                    return;
                }
                for parser in parsers {
                    parser.generate_grammar_(name_gen, rules_out);
                    let (choice_name, _) = name_gen.gen_name(parser);
                    let rule = crate::lr_parser::Rule::new(
                        Some(RuleOrToken::clone(&name)),
                        vec![choice_name],
                        None,
                    );
                    rules_out.push(rule);
                }
            }
            ParserBase::AndThenEffect { parser, effect, } => {
                let (name, is_new) = name_gen.gen_name(self);
                if !is_new {
                    return;
                }
                let gap_idx = rules_out.len();
                let gap = crate::lr_parser::Rule::new(None, Vec::new(), None);
                rules_out.push(gap);
                parser.generate_grammar_(name_gen, rules_out);
                let (inner_name, _) = name_gen.gen_name(parser);
                rules_out[gap_idx] = crate::lr_parser::Rule::new(Some(name), vec![inner_name], Some(Rc::clone(effect)));
            }
        }
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
    let rules = combinator.generate_grammar();
    println!("Rules:");
    println!("{:?}", rules);
    println!();
    let table_generator = crate::lr_parser::LrParserTableGenerator::new(
        crate::lr_parser::Grammar(rules),
        crate::lr_parser::Lexemes(vec![
            RuleOrToken::Token('A'),
            RuleOrToken::Token('B'),
            RuleOrToken::Token('C'),
            RuleOrToken::Token('D'),
        ]),
    );
    let table = table_generator.generate_table();
    println!("Parser Table:");
    println!("{:?}", table);
}
