use std::any::Any;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub struct Rule<S> {
    name_op: Option<S>,
    parts: Vec<S>,
    effect_op: Option<Rc<RefCell<dyn FnMut(&mut Vec<Box<dyn Any>>)>>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Item<S> {
    rule: usize,
    index: usize,
    lookahead: S,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ItemSet<S>(Vec<Item<S>>);

impl<S> ItemSet<S> {
    pub fn new() -> ItemSet<S> {
        ItemSet(Vec::new())
    }

    pub fn insert(&mut self, item: Item<S>) -> bool where S: PartialEq + PartialOrd + Ord {
        for row in &self.0 {
            if *row == item {
                return false;
            }
        }
        for i in 0..self.0.len() {
            let row = &self.0[i];
            if item < *row {
                self.0.insert(i, item);
                return true;
            }
        }
        self.0.push(item);
        return true;
    }
}

pub type Grammar<S> = Vec<Rule<S>>;

pub type Lexemes<S> = Vec<S>;

pub struct Reduce<S> {
    lookahead: S,
    rule_name_op: Option<S>,
    effect_op: Option<Rc<RefCell<dyn FnMut(&mut Vec<Box<dyn Any>>)>>>,
}

impl<S: std::fmt::Debug> std::fmt::Debug for Reduce<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f
            .debug_struct("Reduce")
            .field("lookahead", &self.lookahead)
            .field("rule_name_op", &self.rule_name_op)
            .finish()
    }
}

#[derive(Debug)]
pub struct State<S> {
    shifts: HashMap<S,usize>,
    reduces: Vec<Reduce<S>>,
}

impl<S> State<S> {
    fn new() -> State<S> {
        State {
            shifts: HashMap::new(),
            reduces: Vec::new(),
        }
    }
}

type Table<S> = Vec<State<S>>;

pub struct GrammarRefPrefixAndItemRef<'a, S> {
    grammar_ref: &'a Grammar<S>,
    prefix: &'a String,
    item: &'a Item<S>,
}

impl<'a, S: std::fmt::Display> std::fmt::Display for GrammarRefPrefixAndItemRef<'a, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let rule: &Rule<S> = &self.grammar_ref[self.item.rule];
        write!(f, "{}", self.prefix)?;
        if let Some(name) = &rule.name_op {
            write!(f, "{}", name)?;
        } else {
            write!(f, "T")?;
        }
        write!(f, " \u{2192}")?;
        let mut at_index: usize = 0;
        for part in &rule.parts {
            if at_index == self.item.index {
                write!(f, " \u{2218}")?;
            }
            write!(f, " {}", part)?;
            at_index += 1;
        }
        if at_index == self.item.index {
            write!(f, " \u{2218}")?;
        }
        write!(f, ", {}", self.item.lookahead)?;
        writeln!(f, "")?;
        Ok(())
    }
}

impl<S: std::fmt::Display> std::fmt::Display for Rule<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = &self.name_op {
            write!(f, "{}", name)?;
        } else {
            write!(f, "T")?;
        }
        write!(f, " \u{2192}")?;
        for part in &self.parts {
            write!(f, " {}", *part)?;
        }
        return Ok(());
    }
}

pub struct GrammarRefPrefixAndItemSetRef<'a, S> {
    grammar_ref: &'a Grammar<S>,
    prefix: String,
    item_set_ref: &'a ItemSet<S>,
}

impl<'a, S: std::fmt::Display> std::fmt::Display for GrammarRefPrefixAndItemSetRef<'a, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let first_prefix = format!("{}: ", self.prefix);
        let other_prefixes: String = (0..first_prefix.len()).map(|_| ' ').collect();
        let mut is_first = true;
        for item in &self.item_set_ref.0 {
            write!(
                f,
                "{}",
                GrammarRefPrefixAndItemRef {
                    grammar_ref: self.grammar_ref,
                    prefix: if is_first {
                        &first_prefix
                    } else {
                        &other_prefixes
                    },
                    item: item,
                }
            )?;
            is_first = false;
        }
        Ok(())
    }
}

fn find_lexemes<S>(grammar: &Grammar<S>) -> Vec<S> where S: Clone + PartialEq + PartialOrd + Ord {
    let mut result = Vec::new();
    for rule in grammar {
        for part in &rule.parts {
            if !result.contains(part) {
                let mut found = false;
                for rule2 in grammar {
                    if let Some(name) = rule2.name_op.as_ref() {
                        if *part == *name {
                            found = true;
                            break;
                        }
                    }
                }
                if !found {
                    result.push(S::clone(part));
                }
            }
        }
    }
    result.sort();
    result
}

fn make_empty_set<S>(grammar: &Grammar<S>, lexemes: &Vec<S>) -> HashSet<S>
where
    S: Clone + PartialEq + Eq + std::hash::Hash
{
    let mut result = HashSet::new();
    let mut again;
    loop {
        again = false;
        for rule in grammar {
            if let Some(name) = &rule.name_op {
                if !result.contains(name) {
                    let match_ = rule.parts.iter().all(|s| result.contains(s));
                    if match_ {
                        result.insert(S::clone(name));
                        again = true;
                    }
                }
            }
        }
        if !again {
            break;
        }
    }
    result
}

fn first<S>(cache: &mut HashMap<Option<S>,HashSet<S>>, grammar: &Grammar<S>, lexemes: &Vec<S>, empty: &HashSet<S>, sym_op: &Option<S>) -> HashSet<S> where S: Clone + PartialEq + Eq + std::hash::Hash {
    if let Some(r) = cache.get(sym_op) {
        return HashSet::clone(r);
    }
    let mut result = HashSet::new();
    for rule in grammar {
        if rule.name_op == *sym_op {
            for part in &rule.parts {
                if sym_op.as_ref().map_or(false, |sym| *part == *sym) {
                    break;
                }
                if lexemes.contains(part) {
                    result.insert(S::clone(part));
                    break;
                }
                let tmp = first(cache, grammar, lexemes, empty, &Some(S::clone(part)));
                for x in tmp {
                    result.insert(x);
                }
                if !empty.contains(part) {
                    break;
                }
            }
        }
    }
    cache.insert(Option::clone(sym_op), HashSet::clone(&result));
    result
}

fn make_first_table<S>(grammar: &Grammar<S>, lexemes: &Vec<S>, empty: &HashSet<S>) -> HashMap<Option<S>,HashSet<S>> where S: Clone + PartialEq + Eq + std::hash::Hash {
    let mut result: HashMap<Option<S>,HashSet<S>> = HashMap::new();
    for rule in grammar {
        let sym_op = &rule.name_op;
        if !result.contains_key(sym_op) {
            first(&mut result, grammar, lexemes, empty, sym_op);
        }
    }
    return result;
}

fn make_follow_table<S>(grammar: &Grammar<S>, lexemes: &Vec<S>, first: &HashMap<Option<S>,HashSet<S>>) -> HashMap<Option<S>,HashSet<S>> where S: Clone + PartialEq + Eq + std::hash::Hash {
    let mut result: HashMap<Option<S>,HashSet<S>> = HashMap::new();
    fn add_to_result<S>(result: &mut HashMap<Option<S>,HashSet<S>>, key: Option<S>, item: S) -> bool
    where
        S: Clone + PartialEq + Eq + std::hash::Hash,
    {
        if let Some(tmp) = result.get_mut(&key) {
            return tmp.insert(item);
        } else {
            let mut tmp = HashSet::new();
            tmp.insert(item);
            result.insert(key, tmp);
            return true;
        }
    }
    // FIXME: HACK
    add_to_result(&mut result, None, S::clone(&grammar[0].parts[grammar[0].parts.len()-1]));
    //
    for rule in grammar {
        for k in 0..rule.parts.len()-1 {
            let part_k = &rule.parts[k];
            if lexemes.contains(part_k) {
                continue;
            }
            let part_k_plus_1 = &rule.parts[k + 1];
            if lexemes.contains(part_k_plus_1) {
                add_to_result(&mut result, Some(S::clone(part_k)), S::clone(part_k_plus_1));
            } else {
                if let Some(tmp) = first.get(&Some(S::clone(part_k_plus_1))) {
                    for tmp2 in tmp {
                        add_to_result(&mut result, Some(S::clone(part_k)), S::clone(tmp2));
                    }
                }
            }
        }
    }
    loop {
        let mut again = false;
        for rule in grammar {
            if !rule.parts.is_empty() {
                let last_part = &rule.parts[rule.parts.len()-1];
                if lexemes.contains(last_part) {
                    continue;
                }
                let mut tmp2: Vec<S> = Vec::new();
                if let Some(tmp) = result.get(&rule.name_op) {
                    for tmp3 in tmp {
                        tmp2.push(S::clone(tmp3));
                    }
                }
                for tmp3 in tmp2 {
                    again |= add_to_result(&mut result, Some(S::clone(last_part)), tmp3);
                }
            }
        }
        if !again {
            break;
        }
    }
    result
}

fn closure<S>(grammar: &Grammar<S>, lexemes: &Lexemes<S>, first: &HashMap<Option<S>,HashSet<S>>, follow: &HashMap<Option<S>,HashSet<S>>, item: &Item<S>) -> ItemSet<S>
where
    S: Clone + PartialEq + Eq + std::hash::Hash + PartialOrd + Ord,
{
    let mut result = ItemSet::new();
    let mut stack = Vec::new();
    stack.push(Item::clone(item));
    while let Some(item) = stack.pop() {
        let changed = result.insert(Item::clone(&item));
        if !changed {
            continue;
        }
        let rule = &grammar[item.rule];
        if item.index < rule.parts.len() {
            let at_part = &rule.parts[item.index];
            if lexemes.contains(at_part) {
                // do nothing
            } else if item.index < rule.parts.len() - 1 {
                let at_next_part = &rule.parts[item.index + 1];
                if lexemes.contains(at_next_part) {
                    let lookahead = at_next_part;
                    let mut rule_idx: usize = 0;
                    for rule in grammar {
                        if rule.name_op.as_ref().map_or(false, |name| *name == *at_part) {
                            stack.push(Item {
                                rule: rule_idx,
                                index: 0,
                                lookahead: S::clone(lookahead),
                            });
                        }
                        rule_idx += 1;
                    }
                } else {
                    let lookaheads = first.get(&Some(S::clone(at_next_part))).unwrap();
                    let mut rule_idx: usize = 0;
                    for rule in grammar {
                        if rule.name_op.as_ref().map_or(false, |name| *name == *at_part) {
                            for lookahead in lookaheads {
                                stack.push(Item {
                                    rule: rule_idx,
                                    index: 0,
                                    lookahead: S::clone(lookahead),
                                });
                            }
                        }
                        rule_idx += 1;
                    }
                }
            } else {
                let lookaheads = follow.get(&rule.name_op).unwrap();
                if lexemes.contains(at_part) {
                    for lookahead in lookaheads {
                        result.insert(Item {
                            rule: item.rule,
                            index: item.index,
                            lookahead: S::clone(lookahead),
                        });
                    }
                } else {
                    let mut rule_idx: usize = 0;
                    for rule in grammar {
                        if rule.name_op.as_ref().map_or(false, |name| *name == *at_part) {
                            for lookahead in lookaheads {
                                stack.push(Item {
                                    rule: rule_idx,
                                    index: 0,
                                    lookahead: S::clone(lookahead),
                                });
                            }
                        }
                        rule_idx += 1;
                    }
                }
            }
        }
    }
    result
}

fn edges<S>(grammar: &Grammar<S>, item_set: &ItemSet<S>) -> HashSet<S>
where
    S: Clone + PartialEq + Eq + std::hash::Hash
{
    let mut result = HashSet::new();
    for item in &item_set.0 {
        let rule = &grammar[item.rule];
        if item.index < rule.parts.len() {
            result.insert(S::clone(&rule.parts[item.index]));
        }
    }
    result
}

fn goto<S>(grammar: &Grammar<S>, lexemes: &Lexemes<S>, first: &HashMap<Option<S>,HashSet<S>>, follow: &HashMap<Option<S>,HashSet<S>>, item_set: &ItemSet<S>, sym: &S) -> ItemSet<S>
where
    S: Clone + PartialEq + std::hash::Hash + PartialOrd + Ord,
{
    let mut result = ItemSet::new();
    for item in &item_set.0 {
        let rule = &grammar[item.rule];
        if item.index < rule.parts.len() {
            if rule.parts[item.index] == *sym {
                let item = Item {
                    rule: item.rule,
                    index: item.index + 1,
                    lookahead: S::clone(&item.lookahead),
                };
                for item in closure(grammar, lexemes, first, follow, &item).0 {
                    result.insert(item);
                }
            }
        }
    }
    result
}

fn make_table_<S>(grammar: &Grammar<S>, lexemes: &Lexemes<S>, first: &HashMap<Option<S>,HashSet<S>>, follow: &HashMap<Option<S>,HashSet<S>>, start: &Item<S>) -> Table<S>
where
    S: Clone + PartialEq + Eq + std::hash::Hash + PartialOrd + Ord + std::fmt::Display,
{
    let mut states: HashMap<ItemSet<S>,usize> = HashMap::new();
    let mut stack = Vec::new();
    {
        let item_set_0 = closure(grammar, lexemes, first, follow, start);
        stack.push(item_set_0);
    }
    while let Some(item_set) = stack.pop() {
        if states.contains_key(&item_set) {
            continue;
        }
        let state = states.len();
        states.insert(ItemSet::clone(&item_set), state);
        print!("{}", GrammarRefPrefixAndItemSetRef {
            grammar_ref: &grammar,
            prefix: format!("State {}", state),
            item_set_ref: &item_set,
        });
        let edges = edges(grammar, &item_set);
        for edge in edges {
            let next_item_set = goto(grammar, lexemes, first, follow, &item_set, &edge);
            stack.push(next_item_set);
        }
    }
    let mut table: HashMap<usize,State<S>> = HashMap::new();
    for item_set in states.keys() {
        let state_idx = states.get(item_set).unwrap();
        let edges = edges(grammar, item_set);
        let mut state = State::new();
        for edge in edges {
            let next_item_set = goto(grammar, lexemes, first, follow, item_set, &edge);
            let next_state_idx = states.get(&next_item_set).unwrap();
            state.shifts.insert(edge, *next_state_idx);
        }
        for item in &item_set.0 {
            let rule = &grammar[item.rule];
            if item.index == rule.parts.len() {
                state.reduces.push(Reduce {
                    lookahead: S::clone(&item.lookahead),
                    rule_name_op: Option::clone(&rule.name_op),
                    effect_op: rule.effect_op.as_ref().map(Rc::clone),
                });
            }
        }
        table.insert(*state_idx, state);
    }
    let mut result = Vec::new();
    for i in 0..table.len() {
        let mut tmp = State::new();
        std::mem::swap(&mut tmp, table.get_mut(&i).unwrap());
        result.push(tmp);
    }
    result
}

pub fn make_table<S>(grammar: &Grammar<S>, eof_sym: S) -> Table<S>
where
    S: Clone + PartialEq + Eq + std::hash::Hash + PartialOrd + Ord + std::fmt::Display,
{
    let lexemes = find_lexemes(&grammar);
    let empty = make_empty_set(&grammar, &lexemes);
    let first = make_first_table(&grammar, &lexemes, &empty);
    let follow = make_follow_table(&grammar, &lexemes, &first);
    let mut start_op: Option<Item<S>> = None;
    for rule_idx in 0..grammar.len() {
        let rule = &grammar[rule_idx];
        if rule.name_op.is_none() {
            start_op = Some(Item {
                rule: rule_idx,
                index: 0,
                lookahead: S::clone(&eof_sym)
            });
            break;
        }
    }
    make_table_(grammar, &lexemes, &first, &follow, &start_op.expect("start grammar rule not found."))
}

#[test]
fn test_lr1_parser() {
    let grammar = vec![
        Rule {
            name_op: None,
            parts: vec!["Add", "$"],
            effect_op: None,
        },
        Rule {
            name_op: Some("Add"),
            parts: vec!["Add", "+", "Factor"],
            effect_op: None,
        },
        Rule {
            name_op: Some("Add"),
            parts: vec!["Factor"],
            effect_op: None,
        },
        Rule {
            name_op: Some("Factor"),
            parts: vec!["Factor", "*", "Term"],
            effect_op: None,
        },
        Rule {
            name_op: Some("Factor"),
            parts: vec!["Term"],
            effect_op: None,
        },
        Rule {
            name_op: Some("Term"),
            parts: vec!["(", "Add", ")"],
            effect_op: None,
        },
        Rule {
            name_op: Some("Term"),
            parts: vec!["name"],
            effect_op: None,
        },
        Rule {
            name_op: Some("Term"),
            parts: vec!["int"],
            effect_op: None,
        }
    ];
    let lexemes = find_lexemes(&grammar);
    let empty = make_empty_set(&grammar, &lexemes);
    let first = make_first_table(&grammar, &lexemes, &empty);
    let follow = make_follow_table(&grammar, &lexemes, &first);
    println!("Rules:");
    for rule in &grammar {
        println!("  {}", rule);
    }
    println!("Empty Set: {:?}", empty);
    println!("First: {:#?}", first);
    println!("Follow: {:#?}", follow);
    let table = make_table_(&grammar, &lexemes, &first, &follow, &Item { rule: 0, index: 0, lookahead: "$", });
    println!("Table:");
    for i in 0..table.len() {
        println!("  {}: {:?}", i, table[i]);
    }
}
