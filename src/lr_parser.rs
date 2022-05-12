use std::collections::{HashMap, HashSet};
use std::hash::Hash;

// https://boxbase.org/entries/2019/oct/14/lr1-parsing-tables/

pub struct LrParserTableGenerator<S> {
    grammar: Grammar<S>,
    lexemes: Lexemes<S>,
}

pub struct Grammar<S>(Vec<Rule<S>>);

pub struct Lexemes<S>(Vec<S>);

pub struct Rule<S> {
    name_op: Option<S>,
    parts: Vec<S>,
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

#[derive(Debug)]
pub struct LrParserTableState<S> {
    shifts: HashMap<S, usize>,
    reduce_op: Option<(usize, Option<S>)>,
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

#[derive(Debug)]
pub struct LrParser<S> {
    table: LrParserTable<S>,
    state: usize,
    stack: Vec<usize>,
    output: Vec<AstNode<S>>,
}

impl<S: std::fmt::Debug> LrParserTableGenerator<S> {
    pub fn after_dot(&self, item: &Item) -> Option<S>
    where
        S: Clone,
    {
        let rule = &self.grammar.0[item.rule];
        rule.parts.get(item.index).map(S::clone)
    }

    pub fn follow(&self, item_set: &ItemSet, sym: S) -> ItemSet
    where
        S: PartialEq + Eq + Hash,
    {
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
                                stack.push(Item {
                                    rule: rule_index,
                                    index: 0,
                                })
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
                        let part_0 = &rule.parts[0];
                        if !self.lexemes.0.contains(part_0) {
                            stack.push(Some(part_0.clone()));
                            break;
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
            let mut reduce_op: Option<(usize, Option<S>)> = None;
            for item in &item_set.items {
                let rule = &self.grammar.0[item.rule];
                if item.index == rule.parts.len() {
                    reduce_op = Some((rule.parts.len(), rule.name_op.clone()));
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
            let lr_parse_table_state = LrParserTableState {
                shifts,
                reduce_op,
            };
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
            state: 0,
            stack: Vec::new(),
            output: Vec::new(),
        }
    }

    pub fn advance(&mut self, sym_op: Option<S>) -> Result<bool,String> where S: Clone + PartialEq + Eq + Hash {
        let state = &self.table.states[self.state];
        if let Some(sym) = sym_op {
            let shift_op = state.shifts.get(&sym);
            if shift_op.is_none() {
                return Err("syntax error".to_owned());
            }
            let shift = shift_op.unwrap();
            self.stack.push(self.state);
            self.state = *shift;
            self.output.push(AstNode { value: Some(sym), children: Vec::new(), });
        } else {
            // handle eof
            if let Some((consume, rule_name_op)) = &state.reduce_op {
                let mut leaves: Vec<AstNode<S>> = Vec::new();
                for _i in 0..*consume {
                    self.stack.pop();
                    leaves.push(self.output.pop().unwrap());
                }
                leaves.reverse();
                self.output.push(
                    AstNode {
                        value: Option::<S>::clone(rule_name_op),
                        children: leaves,
                    }
                );
                return Ok(rule_name_op.is_none());
            } else {
                return Err("syntax error".to_owned());
            }
        }
        Err("unreachable".to_owned())
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
        },
        Rule {
            name_op: Some("program"),
            parts: vec![],
        },
        Rule {
            name_op: Some("program"),
            parts: vec!["program", "declaration"],
        },
        Rule {
            name_op: Some("declaration"),
            parts: vec!["varDecl"],
        },
        Rule {
            name_op: Some("declaration"),
            parts: vec!["constDecl"],
        },
        Rule {
            name_op: Some("declaration"),
            parts: vec!["statement"],
        },
    ]);
    let lexemes = Lexemes(vec!["varDecl", "constDecl", "statement"]);
    let lr_parser_tg = LrParserTableGenerator { grammar, lexemes };
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
    let lr_parser = LrParser::new(lr_parser_table);
}
