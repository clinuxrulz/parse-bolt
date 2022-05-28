use std::any::Any;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

// Reference: https://serokell.io/blog/how-to-implement-lr1-parser

pub struct Rule<S> {
    name_op: Option<S>,
    parts: Vec<S>,
    effect_op: Option<Rc<RefCell<dyn FnMut(&mut Vec<Box<dyn Any>>)>>>,
}

impl<S> Rule<S> {
    pub fn new(name_op: Option<S>, parts: Vec<S>, effect_op: Option<Rc<RefCell<dyn FnMut(&mut Vec<Box<dyn Any>>)>>>) -> Rule<S> {
        Rule {
            name_op,
            parts,
            effect_op,
        }
    }
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
    consume_count: usize,
    effect_op: Option<Rc<RefCell<dyn FnMut(&mut Vec<Box<dyn Any>>)>>>,
}

impl<S: Clone> Clone for Reduce<S> {
    fn clone(&self) -> Self {
        Reduce {
            lookahead: S::clone(&self.lookahead),
            rule_name_op: Option::clone(&self.rule_name_op),
            consume_count: self.consume_count,
            effect_op: self.effect_op.as_ref().map(Rc::clone),
        }
    }
}

impl<S: std::fmt::Debug> std::fmt::Debug for Reduce<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f
            .debug_struct("Reduce")
            .field("lookahead", &self.lookahead)
            .field("rule_name_op", &self.rule_name_op)
            .field("consume_count", &self.consume_count)
            .finish_non_exhaustive()
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

fn make_first_table<S>(grammar: &Grammar<S>, lexemes: &Vec<S>, empty: &HashSet<S>) -> HashMap<Option<S>,HashSet<S>> where S: Clone + PartialEq + Eq + std::hash::Hash {
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
    loop {
        let mut again = false;
        for rule in grammar {
            for k in 0..rule.parts.len() {
                let part = &rule.parts[k];
                if lexemes.contains(part) {
                    again |= add_to_result(&mut result, Option::clone(&rule.name_op), S::clone(part));
                    break;
                } else {
                    let tmp = result.get(&Some(S::clone(part))).map(HashSet::clone);
                    if let Some(tmp2) = tmp {
                        for tmp3 in tmp2 {
                            again |= add_to_result(&mut result, Option::clone(&rule.name_op), tmp3);
                        }
                    }
                }
                if !empty.contains(part) {
                    break;
                }
            }
        }
        if !again {
            break;
        }
    }
    result
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
        if !rule.parts.is_empty() {
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

fn closure<S>(grammar: &Grammar<S>, lexemes: &Lexemes<S>, empty: &HashSet<S>, first: &HashMap<Option<S>,HashSet<S>>, follow: &HashMap<Option<S>,HashSet<S>>, item: &Item<S>) -> ItemSet<S>
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
            if empty.contains(&rule.parts[item.index]) {
                stack.push(Item {
                    rule: item.rule,
                    index: item.index + 1,
                    lookahead: S::clone(&item.lookahead),
                });
            }
        }
        for k in item.index..rule.parts.len()+1 {
            if k < rule.parts.len() {
                let at_part = &rule.parts[k];
                if lexemes.contains(at_part) {
                    break;
                } else if k < rule.parts.len() - 1 {
                    let at_next_part = &rule.parts[k + 1];
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
                        let lookaheads_op = first.get(&Some(S::clone(at_next_part)));
                        if let Some(lookaheads) = lookaheads_op {
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
                if !empty.contains(&rule.parts[k]) {
                    break;
                }
            }
        }
    }
    result
}

fn edges<S>(grammar: &Grammar<S>, empty_set: &HashSet<S>, item_set: &ItemSet<S>) -> HashSet<S>
where
    S: Clone + PartialEq + Eq + std::hash::Hash
{
    let mut result = HashSet::new();
    for item in &item_set.0 {
        let rule = &grammar[item.rule];
        if item.index < rule.parts.len() {
            let part = &rule.parts[item.index];
            result.insert(S::clone(part));
        }
    }
    result
}

fn goto<S>(grammar: &Grammar<S>, lexemes: &Lexemes<S>, empty: &HashSet<S>, first: &HashMap<Option<S>,HashSet<S>>, follow: &HashMap<Option<S>,HashSet<S>>, item_set: &ItemSet<S>, sym: &S) -> ItemSet<S>
where
    S: Clone + PartialEq + std::hash::Hash + PartialOrd + Ord,
{
    let mut result = ItemSet::new();
    for item in &item_set.0 {
        let rule = &grammar[item.rule];
        if item.index < rule.parts.len() {
            let part = &rule.parts[item.index];
            if *part == *sym {
                let item = Item {
                    rule: item.rule,
                    index: item.index + 1,
                    lookahead: S::clone(&item.lookahead),
                };
                for item in closure(grammar, lexemes, empty, first, follow, &item).0 {
                    result.insert(item);
                }
            }
        }
    }
    result
}

fn make_table_<S>(grammar: &Grammar<S>, lexemes: &Lexemes<S>, empty: &HashSet<S>, first: &HashMap<Option<S>,HashSet<S>>, follow: &HashMap<Option<S>,HashSet<S>>, start: &Item<S>) -> Table<S>
where
    S: Clone + PartialEq + Eq + std::hash::Hash + PartialOrd + Ord + std::fmt::Display + std::fmt::Debug,
{
    let mut states: HashMap<ItemSet<S>,usize> = HashMap::new();
    let mut stack = Vec::new();
    {
        let item_set_0 = closure(grammar, lexemes, empty, first, follow, start);
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
        let edges = edges(grammar, empty, &item_set);
        for edge in edges {
            let next_item_set = goto(grammar, lexemes, empty, first, follow, &item_set, &edge);
            stack.push(next_item_set);
        }
    }
    let mut table: HashMap<usize,State<S>> = HashMap::new();
    for item_set in states.keys() {
        let state_idx = states.get(item_set).unwrap();
        let edges = edges(grammar, empty, item_set);
        let mut state = State::new();
        for edge in edges {
            let next_item_set = goto(grammar, lexemes, empty, first, follow, item_set, &edge);
            let next_state_idx = states.get(&next_item_set).unwrap();
            state.shifts.insert(edge, *next_state_idx);
        }
        for item in &item_set.0 {
            let rule = &grammar[item.rule];
            if item.index == rule.parts.len() {
                state.reduces.push(Reduce {
                    lookahead: S::clone(&item.lookahead),
                    rule_name_op: Option::clone(&rule.name_op),
                    consume_count: rule.parts.len(),
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
    // debugging
    {
        let mut state_idx = 0;
        for state in &result {
            println!("  {}: {:?}", state_idx, state);
            state_idx += 1;
        }
    }
    //
    result
}

pub fn make_table<S>(grammar: &Grammar<S>, eof_sym: &S) -> Table<S>
where
    S: Clone + PartialEq + Eq + std::hash::Hash + PartialOrd + Ord + std::fmt::Display + std::fmt::Debug,
{
    let lexemes = find_lexemes(&grammar);
    let empty = make_empty_set(&grammar, &lexemes);
    println!("empty: {:?}", empty);
    let first = make_first_table(&grammar, &lexemes, &empty);
    println!("first: {:?}", first);
    let follow = make_follow_table(&grammar, &lexemes, &first);
    println!("follow: {:?}", follow);
    let mut start_op: Option<Item<S>> = None;
    for rule_idx in 0..grammar.len() {
        let rule = &grammar[rule_idx];
        if rule.name_op.is_none() {
            start_op = Some(Item {
                rule: rule_idx,
                index: 0,
                lookahead: S::clone(eof_sym)
            });
            break;
        }
    }
    make_table_(grammar, &lexemes, &empty, &first, &follow, &start_op.expect("start grammar rule not found."))
}

#[derive(Debug)]
pub struct Lr1Parser<S> {
    table: Table<S>,
    control_stack_top: usize,
    control_stack: Vec<usize>,
    value_stack: Vec<Box<dyn Any>>,
    finished: bool,
}

impl<S> Lr1Parser<S> {
    pub fn new(table: Table<S>) -> Lr1Parser<S> {
        Lr1Parser {
            table,
            control_stack_top: 0,
            control_stack: Vec::new(),
            value_stack: Vec::new(),
            finished: false,
        }
    }

    pub fn from_grammar(grammar: &Grammar<S>, eof_sym: &S) -> Lr1Parser<S>
    where
        S: Clone + PartialEq + Eq + std::hash::Hash + PartialOrd + Ord + std::fmt::Display + std::fmt::Debug,
    {
        Lr1Parser::new(make_table(grammar, eof_sym))
    }

    fn push_control(&mut self, control: usize) {
        self.control_stack.push(self.control_stack_top);
        self.control_stack_top = control;
    }

    fn pop_control(&mut self) -> usize {
        let result = self.control_stack_top;
        if let Some(x) = self.control_stack.pop() {
            self.control_stack_top = x;
        }
        result
    }

    pub fn get_value_stack_mut(&mut self) -> &mut Vec<Box<dyn Any>> {
        &mut self.value_stack
    }

    pub fn is_finished(&self) -> bool {
        return self.finished;
    }

    pub fn advance(&mut self, sym: &S, value_op: Option<Box<dyn Any>>) -> Result<bool, String>
    where
        S: Clone + PartialEq + Eq + std::hash::Hash + PartialOrd + Ord + std::fmt::Display,
    {
        loop {
            // Debugging
            print!("control:");
            for c in &self.control_stack {
                print!(" {}", c);
            }
            println!(" {}", self.control_stack_top);
            //
            let state = &self.table[self.control_stack_top];
            let shift_op = state.shifts.get(sym).map(|x| *x);
            let reduces: Vec<Reduce<S>> = state.reduces.iter().map(Reduce::clone).filter(|reduce| reduce.lookahead == *sym).collect();
            if shift_op.is_some() && !reduces.is_empty() {
                return Err("Shift/Reduce error.".to_owned());
            } else if reduces.len() > 1 {
                return Err("Reduce/Reduce error.".to_owned());
            } else if shift_op.is_none() && reduces.is_empty() {
                let mut expected: HashSet<S> = HashSet::new();
                for shift_key in state.shifts.keys() {
                    expected.insert(S::clone(shift_key));
                }
                for reduce in &state.reduces {
                    expected.insert(S::clone(&reduce.lookahead));
                }
                let mut expected: Vec<S> = expected.drain().collect();
                expected.sort();
                let mut msg = String::new();
                msg += "Expected one of ";
                let mut first = true;
                for sym2 in expected {
                    if first {
                        first = false;
                    } else {
                        msg += ", ";
                    }
                    msg += &format!("{}", sym2);
                }
                return Err(msg);
            }
            if let Some(shift) = shift_op {
                self.push_control(shift);
                if let Some(value) = value_op {
                    self.value_stack.push(value);
                }
                break;
            } else if reduces.len() == 1 {
                let reduce = &reduces[0];
                for _i in 0..reduce.consume_count {
                    self.pop_control();
                }
                if let Some(effect) = &reduce.effect_op {
                    effect.borrow_mut()(&mut self.value_stack);
                }
                if reduce.rule_name_op.is_none() {
                    self.finished = true;
                    break;
                }
                let state = &self.table[self.control_stack_top];
                let control = *state.shifts.get(reduce.rule_name_op.as_ref().unwrap()).unwrap();
                self.push_control(control);
            }
        }
        Ok(self.is_finished())
    }
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
            effect_op: Some(Rc::new(RefCell::new(|value_stack: &mut Vec<Box<dyn Any>>| {
                let rhs: i32 = *value_stack.pop().unwrap().downcast().ok().unwrap();
                let lhs: i32 = *value_stack.pop().unwrap().downcast().ok().unwrap();
                value_stack.push(Box::new(lhs + rhs) as Box<dyn Any>);
            }))),
        },
        Rule {
            name_op: Some("Add"),
            parts: vec!["Factor"],
            effect_op: None,
        },
        Rule {
            name_op: Some("Factor"),
            parts: vec!["Factor", "*", "Term"],
            effect_op: Some(Rc::new(RefCell::new(|value_stack: &mut Vec<Box<dyn Any>>| {
                let rhs: i32 = *value_stack.pop().unwrap().downcast().ok().unwrap();
                let lhs: i32 = *value_stack.pop().unwrap().downcast().ok().unwrap();
                value_stack.push(Box::new(lhs * rhs) as Box<dyn Any>);
            }))),
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
    let table = make_table_(&grammar, &lexemes, &empty, &first, &follow, &Item { rule: 0, index: 0, lookahead: "$", });
    println!("Table:");
    for i in 0..table.len() {
        println!("  {}: {:?}", i, table[i]);
    }
    let mut parser = Lr1Parser::new(table);
    let _ = parser.advance(&"int", Some(Box::new(3 as i32)));
    let _ = parser.advance(&"+", None);
    let _ = parser.advance(&"int", Some(Box::new(5 as i32)));
    let _ = parser.advance(&"*", None);
    let _ = parser.advance(&"int", Some(Box::new(3 as i32)));
    let _ = parser.advance(&"$", None);
    let x: i32 = *parser.get_value_stack_mut().pop().unwrap().downcast().ok().unwrap();
    println!("result {}", x);
}

#[test]
fn test_lr1_parser_many1() {
    let grammar = vec![
        Rule {
            name_op: None,
            parts: vec!["Start", "$"],
            effect_op: None,
        },
        Rule {
            name_op: Some("Start"),
            parts: vec!["A"],
            effect_op: None,
        },
        Rule {
            name_op: Some("Start"),
            parts: vec!["Start", ",", "A"],
            effect_op: Some(Rc::new(RefCell::new(|value_stack: &mut Vec<Box<dyn Any>>| {
                let rhs: String = *value_stack.pop().unwrap().downcast().ok().unwrap();
                let lhs: String = *value_stack.pop().unwrap().downcast().ok().unwrap();
                value_stack.push(Box::new(format!("({},{})", lhs, rhs)) as Box<dyn Any>);
            }))),
        },
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
    let table = make_table_(&grammar, &lexemes, &empty, &first, &follow, &Item { rule: 0, index: 0, lookahead: "$", });
    println!("Table:");
    for i in 0..table.len() {
        println!("  {}: {:?}", i, table[i]);
    }
    let mut parser = Lr1Parser::new(table);
    let _ = parser.advance(&"A", Some(Box::new("1".to_owned())));
    let _ = parser.advance(&",", None);
    let _ = parser.advance(&"A", Some(Box::new("2".to_owned())));
    let _ = parser.advance(&",", None);
    let _ = parser.advance(&"A", Some(Box::new("3".to_owned())));
    let _ = parser.advance(&",", None);
    let _ = parser.advance(&"A", Some(Box::new("4".to_owned())));
    let _ = parser.advance(&"$", None);
    let _ = parser.advance(&"$", None);
    let x: String = *parser.get_value_stack_mut().pop().unwrap().downcast().ok().unwrap();
    println!("result {:?}", x);
}
