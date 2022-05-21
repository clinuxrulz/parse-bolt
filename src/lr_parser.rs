use std::any::Any;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::rc::Rc;

// https://boxbase.org/entries/2019/oct/14/lr1-parsing-tables/

pub struct LrParserTableGenerator<S> {
    grammar: Grammar<S>,
    lexemes: Lexemes<S>,
    first_table: HashMap<Option<S>,HashSet<Option<S>>>,
}

impl<S> LrParserTableGenerator<S> {
    pub fn new(grammar: Grammar<S>, lexemes: Lexemes<S>) -> LrParserTableGenerator<S>
    where S: Clone + PartialEq + Eq + Hash + std::fmt::Debug
    {
        let mut r = LrParserTableGenerator {
            grammar,
            lexemes,
            first_table: HashMap::new(),
        };
        r.generate_first_table();
        r
    }
}

pub struct Grammar<S>(pub Vec<Rule<S>>);

pub struct Lexemes<S>(pub Vec<S>);

pub struct Rule<S> {
    name_op: Option<S>,
    parts: Vec<S>,
    effect_op: Option<Rc<RefCell<dyn FnMut(&mut Vec<Box<dyn Any>>)>>>,
}

impl<S: std::fmt::Debug> std::fmt::Debug for Rule<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct BasicEffectInto<'a>(&'a Rc<RefCell<dyn FnMut(&mut Vec<Box<dyn Any>>)>>);
        impl<'a> std::fmt::Debug for BasicEffectInto<'a> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct("Effect").finish_non_exhaustive()
            }
        }
        f.debug_struct("Rule")
            .field("name_op", &self.name_op)
            .field("parts", &self.parts)
            .field("effect_op", &self.effect_op.as_ref().map(BasicEffectInto))
            .finish()
    }
}

impl<S> Rule<S> {
    pub fn new(
        name_op: Option<S>,
        parts: Vec<S>,
        effect_op: Option<Rc<RefCell<dyn FnMut(&mut Vec<Box<dyn Any>>)>>>,
    ) -> Rule<S> {
        Rule {
            name_op,
            parts,
            effect_op,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Item {
    rule: usize,
    index: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ItemSet {
    items: Vec<Item>,
}

pub struct LrParserTableState<S> {
    shifts: HashMap<S, usize>,
    reduce_op: Option<(
        usize,
        Option<S>,
        Option<Rc<RefCell<dyn FnMut(&mut Vec<Box<dyn Any>>)>>>,
    )>,
}

impl<S: std::fmt::Debug> std::fmt::Debug for LrParserTableState<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct BasicEffectInto<'a>(&'a Rc<RefCell<dyn FnMut(&mut Vec<Box<dyn Any>>)>>);
        impl<'a> std::fmt::Debug for BasicEffectInto<'a> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct("Effect").finish_non_exhaustive()
            }
        }
        f.debug_struct("LrParserTableState")
            .field("shifts", &self.shifts)
            .field(
                "reduce_op",
                &self
                    .reduce_op
                    .as_ref()
                    .map(|(a, b, c)| (a, b, c.as_ref().map(BasicEffectInto))),
            )
            .finish()
    }
}

#[derive(Debug)]
pub struct LrParserTable<S> {
    states: Vec<LrParserTableState<S>>,
}

#[derive(Debug)]
pub struct AstNode<S> {
    value: Option<S>,
    children: Vec<AstNode<S>>,
}

impl<S: std::fmt::Display> std::fmt::Display for AstNode<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn draw_node<S: std::fmt::Display>(
            mut prefix: String,
            node: &AstNode<S>,
            f: &mut std::fmt::Formatter<'_>,
        ) -> std::fmt::Result {
            if let Some(sym) = &node.value {
                writeln!(f, "{}{}", prefix, sym)?;
            } else {
                writeln!(f, "{}Root", prefix)?;
            }
            prefix += "  ";
            for child in &node.children {
                draw_node(prefix.clone(), child, f)?;
            }
            Ok(())
        }
        draw_node("".to_owned(), self, f)?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct LrParser<S> {
    table: LrParserTable<S>,
    stack: Vec<usize>,
    forest: Vec<AstNode<S>>,
    value_stack: Vec<Box<dyn Any>>,
}

impl<S: std::fmt::Debug> LrParserTableGenerator<S> {
    pub fn after_dot(&self, item: &Item) -> Option<S>
    where
        S: Clone,
    {
        let rule = &self.grammar.0[item.rule];
        rule.parts.get(item.index).map(S::clone)
    }

    pub fn empty(&self) -> HashSet<S> where S: Clone + PartialEq + Eq + Hash {
        let mut result = HashSet::new();
        let mut again;
        loop {
            again = false;
            for rule in &self.grammar.0 {
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

    pub fn first(&self, sym_op: Option<S>) -> HashSet<Option<S>>
    where
        S: Clone + PartialEq + Eq + Hash
    {
        let mut result = HashSet::new();
        let mut stack = Vec::new();
        let mut visited = HashSet::new();
        stack.push(sym_op);
        while let Some(sym_op) = stack.pop() {
            if visited.contains(&sym_op) {
                continue;
            }
            visited.insert(Option::clone(&sym_op));
            for rule in &self.grammar.0 {
                if rule.name_op == sym_op {
                    if rule.parts.is_empty() {
                        result.insert(None);
                    } else {
                        let part_0 = &rule.parts[0];
                        if self.lexemes.0.contains(part_0) {
                            result.insert(Some(S::clone(part_0)));
                        } else {
                            stack.push(Some(S::clone(part_0)));
                        }
                    }
                }
            }
        }
        result
    }

    pub fn generate_first_table(&mut self)
    where
        S: Clone + PartialEq + Eq + Hash
    {
        for rule in &self.grammar.0 {
            if !self.first_table.contains_key(&rule.name_op) {
                self.first_table.insert(Option::clone(&rule.name_op), self.first(Option::clone(&rule.name_op)));
            }
        }
    }

    pub fn follow(&self, item_set: &ItemSet, sym: S) -> ItemSet
    where
        S: Clone + PartialEq + Eq + Hash,
    {
        let empty = self.empty();
        let mut result: HashSet<Item> = HashSet::new();
        let mut stack: Vec<Item> = Vec::new();
        for item in &item_set.items {
            let rule = &self.grammar.0[item.rule];
            if item.index < rule.parts.len() {
                if sym == rule.parts[item.index] {
                    let next_item = Item {
                        rule: item.rule,
                        index: item.index + 1,
                    };
                    result.insert(next_item);
                    stack.push(next_item);
                }
            }
        }
        let mut visited: HashSet<Item> = HashSet::new();
        while let Some(item) = stack.pop() {
            let rule = &self.grammar.0[item.rule];
            if visited.contains(&item) {
                continue;
            }
            visited.insert(item);
            if item.index < rule.parts.len() {
                let part0 = &rule.parts[item.index];
                if self.lexemes.0.contains(part0) {
                    result.insert(item);
                } else {
                    let mut rule_index: usize = 0;
                    for rule in &self.grammar.0 {
                        if let Some(rule_name) = rule.name_op.as_ref() {
                            if *rule_name == *part0 {
                                let mut reached_end = true;
                                for k in 0..rule.parts.len() {
                                    result.insert(Item {
                                        rule: rule_index,
                                        index: k,
                                    });
                                    stack.push(Item {
                                        rule: rule_index,
                                        index: k,
                                    });
                                    if !empty.contains(&rule.parts[k]) {
                                        reached_end = false;
                                        break;
                                    }
                                }
                                if reached_end {
                                    result.insert(Item { rule: rule_index, index: rule.parts.len() });
                                }
                            }
                        }
                        rule_index += 1;
                    }
                }
            } else {
                result.insert(item);
            }
        }
        let mut items = result.drain().collect::<Vec<Item>>();
        items.sort();
        return ItemSet { items };
    }

    pub fn create_state_0_item_set(&self) -> ItemSet
    where
        S: Clone + PartialEq + Eq + Hash,
    {
        let empty = self.empty();
        let mut result: HashSet<Item> = HashSet::new();
        let mut stack: Vec<Option<S>> = Vec::new();
        stack.push(None);
        while let Some(sym_op) = stack.pop() {
            let mut rule_index: usize = 0;
            for rule in &self.grammar.0 {
                if rule.name_op == sym_op {
                    let changed = result.insert(Item {
                        rule: rule_index,
                        index: 0,
                    });
                    if changed && !rule.parts.is_empty() {
                        let mut reached_end = true;
                        for k in 0..rule.parts.len() {
                            let part_k = &rule.parts[k];
                            result.insert(Item { rule: rule_index, index: k, });
                            if !self.lexemes.0.contains(part_k) {
                                stack.push(Some(part_k.clone()));
                            }
                            if !empty.contains(part_k) {
                                reached_end = false;
                                break;
                            }
                        }
                        if reached_end {
                            result.insert(Item { rule: rule_index, index: rule.parts.len() });
                        }
                    }
                }
                rule_index += 1;
            }
        }
        let mut items: Vec<Item> = result.drain().collect();
        items.sort();
        return ItemSet { items: items };
    }

    pub fn edges(&self, item_set: &ItemSet) -> HashSet<Option<S>>
    where
        S: Clone + PartialEq + Eq + Hash,
    {
        let mut result: HashSet<Option<S>> = HashSet::new();
        for item in &item_set.items {
            let rule = &self.grammar.0[item.rule];
            result.insert(rule.parts.get(item.index).map(S::clone));
        }
        return result;
    }

    pub fn generate_table(&self) -> LrParserTable<S>
    where
        S: Clone + PartialEq + Eq + Hash,
    {
        let mut stack: Vec<ItemSet> = Vec::new();
        let mut states: HashMap<usize, Option<LrParserTableState<S>>> = HashMap::new();
        let mut state_index_map: HashMap<ItemSet, usize> = HashMap::new();
        {
            let state_0 = self.create_state_0_item_set();
            stack.push(state_0);
        }
        while let Some(item_set) = stack.pop() {
            let state_index;
            if let Some(state_index_2) = state_index_map.get(&item_set) {
                state_index = *state_index_2;
            } else {
                state_index = state_index_map.len();
                state_index_map.insert(item_set.clone(), state_index);
            }
            if states.contains_key(&state_index) {
                continue;
            }
            let edges = self.edges(&item_set);
            let mut reduce_op: Option<(
                usize,
                Option<S>,
                Option<Rc<RefCell<dyn FnMut(&mut Vec<Box<dyn Any>>)>>>,
            )> = None;
            for item in &item_set.items {
                let rule = &self.grammar.0[item.rule];
                if item.index == rule.parts.len() {
                    reduce_op = Some((
                        rule.parts.len(),
                        rule.name_op.clone(),
                        rule.effect_op.as_ref().map(Rc::clone),
                    ));
                }
            }
            let mut shifts: HashMap<S, usize> = HashMap::new();
            for sym_op in edges {
                if let Some(sym) = sym_op {
                    let next_item_set = self.follow(&item_set, sym.clone());
                    let next_state_index;
                    if let Some(next_state_index2) = state_index_map.get(&next_item_set) {
                        next_state_index = *next_state_index2;
                    } else {
                        next_state_index = state_index_map.len();
                        state_index_map.insert(next_item_set.clone(), next_state_index);
                    }
                    shifts.insert(sym, next_state_index);
                    stack.push(next_item_set);
                }
            }
            let lr_parse_table_state = LrParserTableState { shifts, reduce_op };
            states.insert(state_index, Some(lr_parse_table_state));
        }
        let mut states2 = Vec::new();
        for i in 0..states.len() {
            let mut tmp = None;
            std::mem::swap(&mut tmp, &mut states.get_mut(&i).unwrap());
            states2.push(tmp.unwrap());
        }
        LrParserTable { states: states2 }
    }
}

impl<S> LrParser<S> {
    pub fn new(table: LrParserTable<S>) -> LrParser<S> {
        LrParser {
            table,
            stack: vec![0],
            forest: Vec::new(),
            value_stack: Vec::new(),
        }
    }

    pub fn get_value_stack_mut(&mut self) -> &mut Vec<Box<dyn Any>> {
        &mut self.value_stack
    }

    pub fn is_finished(&self) -> bool {
        return self.stack.is_empty() && !self.value_stack.is_empty();
    }

    pub fn advance(
        &mut self,
        sym_op: Option<S>,
        mut value_op: Option<Box<dyn Any>>,
    ) -> Result<bool, String>
    where
        S: Clone + PartialEq + Eq + Hash,
    {
        let mut state;
        let mut state_idx = self.stack[self.stack.len() - 1];
        if let Some(sym) = sym_op {
            loop {
                state = &self.table.states[state_idx];
                let mut again = true;
                if let Some(shift) = state.shifts.get(&sym) {
                    self.stack.push(*shift);
                    state_idx = *shift;
                    state = &self.table.states[state_idx];
                    self.forest.push(AstNode {
                        value: Some(sym.clone()),
                        children: Vec::new(),
                    });
                    let mut tmp = None;
                    std::mem::swap(&mut tmp, &mut value_op);
                    if let Some(value) = tmp {
                        self.value_stack.push(value);
                    }
                    again = false;
                }
                if !again {
                    break;
                }
                if let Some((consume, rule_name_op, effect_op)) = &state.reduce_op {
                    let mut leaves: Vec<AstNode<S>> = Vec::new();
                    for _i in 0..*consume {
                        self.stack.pop();
                        leaves.push(self.forest.pop().unwrap());
                    }
                    leaves.reverse();
                    self.forest.push(AstNode {
                        value: Option::<S>::clone(rule_name_op),
                        children: leaves,
                    });
                    for effect in effect_op {
                        effect.borrow_mut()(&mut self.value_stack);
                    }
                    state_idx = self.stack[self.stack.len() - 1];
                    state = &self.table.states[state_idx];
                    self.stack
                        .push(*state.shifts.get(rule_name_op.as_ref().unwrap()).unwrap());
                    state_idx = self.stack[self.stack.len() - 1];
                }
            }
        } else {
            state = &self.table.states[state_idx];
            while let Some((consume, rule_name_op, effect_op)) = &state.reduce_op {
                let mut leaves: Vec<AstNode<S>> = Vec::new();
                for _i in 0..*consume {
                    self.stack.pop();
                    leaves.push(self.forest.pop().unwrap());
                }
                leaves.reverse();
                self.forest.push(AstNode {
                    value: Option::<S>::clone(rule_name_op),
                    children: leaves,
                });
                for effect in effect_op {
                    effect.borrow_mut()(&mut self.value_stack);
                }
                if rule_name_op.is_none() {
                    // Finished
                    self.stack.pop();
                    return Ok(true);
                }
                state_idx = self.stack[self.stack.len() - 1];
                state = &self.table.states[state_idx];
                self.stack
                    .push(*state.shifts.get(rule_name_op.as_ref().unwrap()).unwrap());
                state_idx = self.stack[self.stack.len() - 1];
                state = &self.table.states[state_idx];
            }
        }
        return Ok(false);
    }
}

pub struct GrammarRefPrefixAndItemRef<'a, S> {
    grammar_ref: &'a Grammar<S>,
    prefix: &'a String,
    item: &'a Item,
}

pub struct GrammarRefPrefixAndItemSetRef<'a, S> {
    grammar_ref: &'a Grammar<S>,
    prefix: String,
    item_set: &'a ItemSet,
}

impl<S: std::fmt::Display> std::fmt::Display for Grammar<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Grammar {{")?;
        for rule in &self.0 {
            writeln!(f, "  {}", rule)?;
        }
        writeln!(f, "}}")?;
        return Ok(());
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

impl<'a, S: std::fmt::Display> std::fmt::Display for GrammarRefPrefixAndItemRef<'a, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let rule: &Rule<S> = &self.grammar_ref.0[self.item.rule];
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
        writeln!(f, "")?;
        Ok(())
    }
}

impl<'a, S: std::fmt::Display> std::fmt::Display for GrammarRefPrefixAndItemSetRef<'a, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let first_prefix = format!("{}: ", self.prefix);
        let other_prefixes: String = (0..first_prefix.len()).map(|_| ' ').collect();
        let mut is_first = true;
        for item in &self.item_set.items {
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

#[test]
fn test_lr_parser() {
    let grammar = Grammar(vec![
        Rule {
            name_op: None,
            parts: vec!["program"],
            effect_op: None,
        },
        Rule {
            name_op: Some("program"),
            parts: vec![],
            effect_op: None,
        },
        Rule {
            name_op: Some("program"),
            parts: vec!["program", "declaration"],
            effect_op: None,
        },
        Rule {
            name_op: Some("declaration"),
            parts: vec!["varDecl"],
            effect_op: None,
        },
        Rule {
            name_op: Some("declaration"),
            parts: vec!["constDecl"],
            effect_op: None,
        },
        Rule {
            name_op: Some("declaration"),
            parts: vec!["statement"],
            effect_op: None,
        },
    ]);
    let lexemes = Lexemes(vec!["varDecl", "constDecl", "statement"]);
    let lr_parser_tg = LrParserTableGenerator::new(grammar, lexemes);
    println!("---");
    println!("create state 0 item set:");
    let state0_item_set = lr_parser_tg.create_state_0_item_set();
    print!(
        "{}",
        GrammarRefPrefixAndItemSetRef {
            grammar_ref: &lr_parser_tg.grammar,
            prefix: "0".to_owned(),
            item_set: &state0_item_set,
        }
    );
    println!("---");
    println!("following \"program\" from state 0 item set to make next item set state.");
    let state1_item_set = lr_parser_tg.follow(&state0_item_set, "program");
    print!(
        "{}",
        GrammarRefPrefixAndItemSetRef {
            grammar_ref: &lr_parser_tg.grammar,
            prefix: "1".to_owned(),
            item_set: &state1_item_set,
        }
    );
    println!("---");
    println!("generate lr parse table:");
    let lr_parser_table = lr_parser_tg.generate_table();
    println!("{:#?}", lr_parser_table);
    println!("---");
    println!("testing lr parser:");
    let mut lr_parser = LrParser::new(lr_parser_table);
    println!("{:?}", lr_parser);
    println!("__advance statement");
    let _ = lr_parser.advance(Some("statement"), None);
    println!("{:?}", lr_parser);
    println!("__advance varDecl");
    let _ = lr_parser.advance(Some("varDecl"), None);
    println!("{:?}", lr_parser);
    println!("__advance eof");
    let _ = lr_parser.advance(None, None);
    println!("{:?}", lr_parser);
    println!("");
    println!("result:");
    println!("{}", lr_parser.forest[0]);
}

#[test]
fn test_star() {
    let grammar = Grammar(vec![
        Rule {
            name_op: None,
            parts: vec!["A"],
            effect_op: None,
        },
        Rule {
            name_op: Some("A"),
            parts: vec![],
            effect_op: Some(Rc::new(RefCell::new(|stack: &mut Vec<Box<dyn Any>>| {
                stack.push(Box::new(Vec::<usize>::new()))
            }))),
        },
        Rule {
            name_op: Some("A"),
            parts: vec!["A", "a"],
            effect_op: Some(Rc::new(RefCell::new(|stack: &mut Vec<Box<dyn Any>>| {
                let elem: usize = *stack.pop().unwrap().downcast().ok().unwrap();
                let mut list: Box<Vec<usize>> = stack.pop().unwrap().downcast().ok().unwrap();
                list.push(elem);
                stack.push(list);
            }))),
        },
    ]);
    let lexemes = Lexemes(vec!["a"]);
    let lr_parser_tg = LrParserTableGenerator::new(grammar, lexemes);
    let lr_parser_table = lr_parser_tg.generate_table();
    let mut lr_parser = LrParser::new(lr_parser_table);
    let _ = lr_parser.advance(Some("a"), Some(Box::new(1 as usize) as Box<dyn Any>));
    let _ = lr_parser.advance(Some("a"), Some(Box::new(2 as usize) as Box<dyn Any>));
    let _ = lr_parser.advance(Some("a"), Some(Box::new(3 as usize) as Box<dyn Any>));
    let _ = lr_parser.advance(None, None);
    println!("{:?}", lr_parser.value_stack.pop().unwrap().downcast::<Vec<usize>>().ok().unwrap());
}
