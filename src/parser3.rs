use super::TokenStream;
use std::cell::RefCell;
use std::collections::HashMap;
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

struct GrammarNameGen<S> {
    next_id: usize,
    grammar_to_name: HashMap<ParserBase2<S>,usize>,
}

impl<S> GrammarNameGen<S> {
    fn new() -> GrammarNameGen<S> {
        GrammarNameGen {
            next_id: 0,
            grammar_to_name: HashMap::new()
        }
    }
}

impl<S> GrammarNameGen<S> {
    fn gen_name(&mut self, parser: &ParserBase2<S>) -> (RuleOrToken<S>,bool) where S: Clone+PartialEq+Eq+std::hash::Hash {
        match parser {
            ParserBase2::Match { sym, } => {
                return (RuleOrToken::Token(S::clone(sym)), false);
            }
            ParserBase2::Seq { parsers, } => {
                if let Some(id) = self.grammar_to_name.get(parser) {
                    return (RuleOrToken::Rule(*id), false);
                }
                let id = self.next_id;
                self.next_id += 1;
                self.grammar_to_name.insert(ParserBase2::clone(parser), id);
                return (RuleOrToken::Rule(id), true);
            }
            ParserBase2::Choice { parsers, } => {
                if let Some(id) = self.grammar_to_name.get(parser) {
                    return (RuleOrToken::Rule(*id), false);
                }
                let id = self.next_id;
                self.next_id += 1;
                self.grammar_to_name.insert(ParserBase2::clone(parser), id);
                return (RuleOrToken::Rule(id), true);
            }
        }
    }
}

#[derive(Clone, Debug)]
enum RuleOrToken<S> {
    Rule(usize),
    Token(S),
}

#[derive(PartialEq,Eq,Hash)]
enum ParserBase2<S> {
    Match { sym: S, },
    Seq { parsers: Vec<Rc<ParserBase2<S>>>, },
    Choice { parsers: Vec<Rc<ParserBase2<S>>>, },
}

impl<S> ParserBase2<S> {
    pub fn optimise(&mut self) where S: Clone {
        fn optimise_<S: Clone>(a: ParserBase2<S>) -> ParserBase2<S> {
            match a {
                ParserBase2::Match { sym: _ } => {
                    return a;
                },
                ParserBase2::Seq { parsers } => {
                    let parsers2: Vec<ParserBase2<S>> = parsers.iter().map(|parser| optimise_(ParserBase2::clone(&**parser))).collect();
                    let mut parsers3 = Vec::new();
                    for parser in parsers2 {
                        match parser {
                            ParserBase2::Seq { parsers: parsers4 } => {
                                for parser2 in parsers4 {
                                    parsers3.push(parser2);
                                }
                            },
                            _ => {
                                parsers3.push(Rc::new(parser));
                            }
                        }
                    }
                    return ParserBase2::Seq { parsers: parsers3 };
                },
                ParserBase2::Choice { parsers } => {
                    let parsers2: Vec<ParserBase2<S>> = parsers.iter().map(|parser| optimise_(ParserBase2::clone(&**parser))).collect();
                    let mut parsers3 = Vec::new();
                    for parser in parsers2 {
                        match parser {
                            ParserBase2::Choice { parsers: parsers4 } => {
                                for parser2 in parsers4 {
                                    parsers3.push(parser2);
                                }
                            },
                            _ => {
                                parsers3.push(Rc::new(parser));
                            }
                        }
                    }
                    return ParserBase2::Choice { parsers: parsers3 };
                }
            }
        }
        let mut tmp: ParserBase2<S> = ParserBase2::Seq { parsers: vec![], };
        std::mem::swap(&mut tmp, self);
        tmp = optimise_(tmp);
        *self = tmp;
    }
}

impl<S: Clone> Clone for ParserBase2<S> {
    fn clone(&self) -> Self {
        match self {
            ParserBase2::Match { sym, } => {
                ParserBase2::Match { sym: S::clone(sym), }
            }
            ParserBase2::Seq { parsers, } => {
                ParserBase2::Seq { parsers: parsers.iter().map(Rc::clone).collect(), }
            }
            ParserBase2::Choice { parsers } => {
                ParserBase2::Choice { parsers: parsers.iter().map(Rc::clone).collect(), }
            }
        }
    }
}

impl<S> ParserBase2<S> {
    fn generate_grammar(&self) -> Vec<crate::lr_parser::Rule<RuleOrToken<S>>> where S: Clone+PartialEq+Eq+std::hash::Hash {
        let mut name_gen = GrammarNameGen::new();
        let mut rules = Vec::new();
        self.generate_grammar_(&mut name_gen, &mut rules);
        rules.push(crate::lr_parser::Rule::new(None, vec![name_gen.gen_name(self).0]));
        rules.reverse();
        rules
    }

    fn generate_grammar_(&self, name_gen: &mut GrammarNameGen<S>, rules_out: &mut Vec<crate::lr_parser::Rule<RuleOrToken<S>>>) where S: Clone+PartialEq+Eq+std::hash::Hash {
        match self {
            ParserBase2::Match { sym, } => {
            }
            ParserBase2::Seq { parsers, } => {
                let (name, is_new) = name_gen.gen_name(self);
                if !is_new {
                    return;
                }
                let mut parts = Vec::new();
                for parser in parsers {
                    parser.generate_grammar_(name_gen, rules_out);
                    let (part, _) = name_gen.gen_name(parser);
                    parts.push(part);
                }
                let rule = crate::lr_parser::Rule::new(Some(name), parts);
                rules_out.push(rule);
            }
            ParserBase2::Choice { parsers } => {
                let (name, is_new) = name_gen.gen_name(self);
                if !is_new {
                    return;
                }
                for parser in parsers {
                    parser.generate_grammar_(name_gen, rules_out);
                    let (choice_name, _) = name_gen.gen_name(parser);
                    let rule = crate::lr_parser::Rule::new(Some(RuleOrToken::clone(&name)), vec![choice_name]);
                    rules_out.push(rule);
                }
            }
        }
    }
}

#[test]
fn test_combinator_to_grammar() {
    let combinator =
        ParserBase2::Seq {
            parsers: vec![
                Rc::new(ParserBase2::Match { sym: 'A' }),
                Rc::new(ParserBase2::Match { sym: 'B' }),
                Rc::new(ParserBase2::Choice {
                    parsers: vec![
                        Rc::new(ParserBase2::Match { sym: 'C' }),
                        Rc::new(ParserBase2::Match { sym: 'D' }),
                    ]
                })
            ]
        };
    let rules = combinator.generate_grammar();
    println!("{:?}", rules);
}
