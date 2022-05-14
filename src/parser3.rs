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
            ParserBase2::SeqParser { parser1, parser2, } => {
                if let Some(id) = self.grammar_to_name.get(parser) {
                    return (RuleOrToken::Rule(*id), false);
                }
                let id = self.next_id;
                self.next_id += 1;
                return (RuleOrToken::Rule(id), true);
            }
            ParserBase2::MatchParser { sym, } => {
                return (RuleOrToken::Token(S::clone(sym)), false);
            }
        }
    }
}

#[derive(Debug)]
enum RuleOrToken<S> {
    Rule(usize),
    Token(S),
}

#[derive(PartialEq,Eq,Hash)]
enum ParserBase2<S> {
    SeqParser { parser1: Rc<ParserBase2<S>>, parser2: Rc<ParserBase2<S>>, },
    MatchParser { sym: S, },
}

impl<S: Clone> Clone for ParserBase2<S> {
    fn clone(&self) -> Self {
        match self {
            ParserBase2::SeqParser { parser1, parser2, } => {
                ParserBase2::SeqParser { parser1: Rc::clone(parser1), parser2: Rc::clone(parser2), }
            }
            ParserBase2::MatchParser { sym, } => {
                ParserBase2::MatchParser { sym: S::clone(sym), }
            }
        }
    }
}

impl<S> ParserBase2<S> {
    fn generate_grammar(&self, name_gen: &mut GrammarNameGen<S>, rules_out: &mut Vec<crate::lr_parser::Rule<RuleOrToken<S>>>) where S: Clone+PartialEq+Eq+std::hash::Hash {
        match self {
            ParserBase2::SeqParser { parser1, parser2, } => {
                let (name, is_new) = name_gen.gen_name(self);
                if !is_new {
                    return;
                }
                parser1.generate_grammar(name_gen, rules_out);
                parser2.generate_grammar(name_gen, rules_out);
                let (part1, _) = name_gen.gen_name(parser1);
                let (part2, _) = name_gen.gen_name(parser2);
                let rule = crate::lr_parser::Rule::new(Some(name), vec![part1, part2]);
                rules_out.push(rule);
            }
            ParserBase2::MatchParser { sym, } => {
            }
        }
    }
}

#[test]
fn test_combinator_to_grammar() {
    let combinator =
        ParserBase2::SeqParser {
            parser1: Rc::new(ParserBase2::MatchParser { sym: 'A' }),
            parser2: Rc::new(ParserBase2::MatchParser { sym: 'B' }),
        };
    let mut rules = Vec::new();
    combinator.generate_grammar(&mut GrammarNameGen::new(), &mut rules);
    println!("{:?}", rules);
}
