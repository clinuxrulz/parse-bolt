use std::collections::{HashSet, HashMap};
use std::hash::Hash;

// https://boxbase.org/entries/2019/oct/14/lr1-parsing-tables/

pub struct LrParser<S> {
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

impl<S> LrParser<S> {
    pub fn after_dot(&self, item: &Item) -> Option<S> where S: Clone {
        let rule = &self.grammar.0[item.rule];
        rule.parts.get(item.index).map(S::clone)
    }

    pub fn first(&self, sym_op: Option<S>) -> HashSet<S> where S: Clone + PartialEq + Eq + Hash {
        let mut result: HashSet<S> = HashSet::new();
        for rule in &self.grammar.0 {
            if rule.name_op == sym_op {
                if !rule.parts.is_empty() {
                    let part0 = &rule.parts[0];
                    if self.lexemes.0.contains(part0) {
                        result.insert(part0.clone());
                    }
                }
            }
        }
        return result;
    }

    pub fn follow(&self, item_set: &ItemSet, sym: S) -> ItemSet where S: PartialEq + Eq + Hash {
        let mut result: HashSet<Item> = HashSet::new();
        let mut stack: Vec<Item> = Vec::new();
        for item in &item_set.items {
            let rule = &self.grammar.0[item.rule];
            if item.index < rule.parts.len() {
                if sym == rule.parts[item.index] {
                    let next_item = Item { rule: item.rule, index: item.index + 1, };
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
                                stack.push(Item { rule: rule_index, index: 0, })
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
        return ItemSet {
            items,
        };
    }

    pub fn create_state_0_item_set(&self) -> ItemSet where S: Clone + PartialEq {
        let mut result: HashSet<Item> = HashSet::new();
        let mut stack: Vec<Option<S>> = Vec::new();
        stack.push(None);
        while let Some(sym_op) = stack.pop() {
            let mut rule_index: usize = 0;
            for rule in &self.grammar.0 {
                if rule.name_op == sym_op {
                    let changed = result.insert(Item { rule: rule_index, index: 0 });
                    if changed && !rule.parts.is_empty() {
                        let part0 = &rule.parts[0];
                        if !self.lexemes.0.contains(part0) {
                            stack.push(Some(part0.clone()));
                        }
                    }
                }
                rule_index += 1;
            }
        }
        let mut items: Vec<Item> = result.drain().collect();
        items.sort();
        return ItemSet { items: items, }
    }

    pub fn edges(&self, item_set: &ItemSet) -> HashSet<Option<S>> where S: Clone + PartialEq + Eq + Hash {
        let mut result: HashSet<Option<S>> = HashSet::new();
        for item in &item_set.items {
            let rule = &self.grammar.0[item.rule];
            result.insert(rule.parts.get(item.index).map(S::clone));
        }
        return result;
    }

    pub fn predict(&self, mut items: Vec<Item>) -> ItemSet where S: Clone + PartialEq {
        let mut prediction: HashSet<Item> = HashSet::new();
        for item in &items {
            prediction.insert(*item);
        }
        let mut p = prediction.len();
        while let Some(item) = items.pop() {
            let sym_op = self.after_dot(&item);
            let mut index: usize = 0;
            for rule in &self.grammar.0 {
                if sym_op.is_some() && sym_op == rule.name_op {
                    prediction.insert(Item { rule: index, index: 0, });
                    if p < prediction.len() {
                        p = prediction.len();
                        items.push(Item { rule: index, index: 0, });
                    }
                }
                index += 1;
            }
        }
        let mut prediction: Vec<Item> = prediction.drain().collect();
        prediction.sort_by(|a, b| {
            if a.rule == b.rule {
                return a.index.cmp(&b.index);
            } else {
                return a.rule.cmp(&b.rule);
            }
        });
        return ItemSet {
            items: prediction,
        };
    }

    pub fn partition(&self, items: &ItemSet) -> Vec<(Option<S>,ItemSet)> where S: Clone + PartialEq + Eq + Hash, {
        let mut groups: HashMap<Option<S>,Vec<Item>> = HashMap::new();
        for item in &items.items {
            let mut item = *item;
            let sym_op = self.after_dot(&item);
            if sym_op.is_some() {
                item = Item { rule: item.rule, index: item.index + 1 };
            }
            if let Some(items) = groups.get_mut(&sym_op) {
                items.push(item);
            } else {
                groups.insert(sym_op, vec![item]);
            }
        }
        let mut result: Vec<(Option<S>,ItemSet)> = Vec::new();
        for group in groups {
            let mut items: HashSet<Item> = group.1.iter().map(Item::clone).collect();
            result.push((group.0, ItemSet { items: items.drain().collect() }));
        }
        result
    }

    pub fn compute_all_itemsets(&self) -> (Vec<ItemSet>,Vec<ItemSet>,Vec<Vec<usize>>) where S: Clone + std::fmt::Debug + PartialEq + Eq + Hash + std::fmt::Display {
        let mut item_sets: Vec<ItemSet> = Vec::new();
        item_sets.push(ItemSet { items: vec![Item { rule: 0, index: 0, }] });
        let mut item_sets_index: HashMap<ItemSet,usize> = HashMap::new();
        {
            let mut index: usize = 0;
            for item_set in &item_sets {
                item_sets_index.insert(item_set.clone(), index);
                index += 1;
            }
        }
        let mut vectors: Vec<Vec<usize>> = Vec::new();
        let mut full_item_sets: Vec<ItemSet> = Vec::new();
        let mut shifts: Vec<HashMap<S,usize>> = Vec::new();
        let mut reductions: Vec<ItemSet> = Vec::new();
        let mut k: usize = 0;
        while k < item_sets.len() {
            let item_set = &item_sets[k];
            vectors.push(item_set.items.iter().map(|i| i.rule).collect());
            let pset = self.predict(item_set.items.clone());
            full_item_sets.push(pset.clone());
            print!("{}", GrammarRefPrefixAndItemSetRef {
                grammar_ref: &self.grammar,
                prefix: format!("{}", k),
                item_set: &pset
            });
            let mut k_shifts: HashMap<S,usize> = HashMap::new();
            let mut k_reductions: HashSet<Item> = HashSet::new();
            for (sym_op, items) in self.partition(&pset) {
                if let Some(sym) = sym_op {
                    let j;
                    if let Some(j2) = item_sets_index.get(&items) {
                        j = *j2;
                    } else {
                        j = item_sets.len();
                        item_sets_index.insert(items.clone(), j);
                        item_sets.push(items);
                    }
                    k_shifts.insert(sym, j);
                } else {
                    for item in items.items {
                        k_reductions.insert(item);
                    }
                }
            }
            shifts.push(k_shifts);
            reductions.push(ItemSet { items: k_reductions.drain().collect() });
            k += 1;
        }
        println!("{:?}", shifts);
        println!("{:?}", reductions);
        return (item_sets, full_item_sets,vectors);
    }

    pub fn empty_symbols(&self) -> HashSet<Option<S>> where S: Clone + PartialEq + Eq + Hash {
        let mut symbols: HashSet<Option<S>> = HashSet::new();
        for rule in &self.grammar.0 {
            if rule.parts.is_empty() {
                symbols.insert(rule.name_op.clone());
            }
        }
        let mut m: usize = 0;
        let mut n = symbols.len();
        while m < n {
            for rule in &self.grammar.0 {
                if rule.parts.iter().all(|part| symbols.contains(&Some(part.clone()))) {
                    symbols.insert(rule.name_op.clone());
                }
            }
            m = n;
            n = symbols.len();
        }
        symbols
    }

    pub fn first_lexemes(&self) -> HashMap<Option<S>,HashSet<Option<S>>> where S: Clone + PartialEq + Eq + Hash {
        let empty = self.empty_symbols();
        let mut symbols: HashMap<Option<S>,HashSet<Option<S>>> = HashMap::new();
        let mut routes: HashSet<(Option<S>,Option<S>)> = HashSet::new();
        for sym in &self.lexemes.0 {
            let mut tmp: HashSet<Option<S>> = HashSet::new();
            tmp.insert(Some((*sym).clone()));
            symbols.insert(Some((*sym).clone()), tmp);
        }
        for rule in &self.grammar.0 {
            if !symbols.contains_key(&rule.name_op) {
                symbols.insert(rule.name_op.clone(), HashSet::new());
            }
        }
        for rule in &self.grammar.0 {
            for rule_n in &rule.parts {
                routes.insert((rule.name_op.clone(), Some(rule_n.clone())));
                if !empty.contains(&Some(rule_n.clone())) {
                    break;
                }
            }
        }
        let mut rep = true;
        while rep {
            rep = false;
            for (lhs, rhs0) in &routes {
                let n = symbols.get(lhs).map(HashSet::len).unwrap_or(0);
                let to_add = symbols.get(rhs0).unwrap().clone();
                if let Some(xs) = symbols.get_mut(lhs) {
                    for symbol in to_add {
                        xs.insert(symbol);
                    }
                } else {
                    symbols.insert(lhs.clone(), to_add);
                }
                rep |= n < symbols.get(lhs).map(HashSet::len).unwrap_or(0);
            }
        }
        return symbols;
    }

    pub fn follow_lexemes(&self, seed_set: &HashSet<(usize,usize)>, full_item_set: &ItemSet) -> (HashMap<Option<S>, HashSet<Option<S>>>, HashMap<Option<S>, HashSet<(usize, usize)>>) where S: Clone + PartialEq + Eq + Hash {
        let empty = self.empty_symbols();
        let first = self.first_lexemes();
        let mut symbols: HashMap<Option<S>,HashSet<Option<S>>> = HashMap::new();
        let mut seeds: HashMap<Option<S>,HashSet<(usize,usize)>> = HashMap::new();
        let mut routes: HashSet<(Option<S>,Option<S>)> = HashSet::new();
        for item in &full_item_set.items {
            let sym0 = self.after_dot(item);
            if !symbols.contains_key(&sym0) {
                symbols.insert(sym0.clone(), HashSet::new());
                seeds.insert(sym0, HashSet::new());
            }
        }
        let mut rhs0: Option<S> = None;
        for Item { rule, index } in &full_item_set.items {
            let Rule { name_op: lhs, parts: rhs } = &self.grammar.0[*rule];
            if *index < rhs.len() {
                rhs0 = Some(rhs[*index].clone());
                let mut k = *index + 1;
                let symbols_rhs0 = symbols.get_mut(&rhs0).unwrap();
                while k < rhs.len() {
                    let tmp_op = first.get(&Some(rhs[k].clone()));
                    if let Some(tmp) = tmp_op {
                        for tmp2 in tmp {
                            symbols_rhs0.insert(tmp2.clone());
                        }
                        if !empty.contains(&Some(rhs[k].clone())) {
                            break;
                        }
                    }
                    k += 1;
                }
                if k == rhs.len() {
                    if seed_set.contains(&(*rule, *index)) {
                        seeds.get_mut(&rhs0).unwrap().insert((*rule, *index));
                    } else {
                        routes.insert((lhs.clone(), rhs0.clone()));
                    }
                }
            }
        }
        let mut rep = true;
        while rep {
            rep = false;
            for (lhs, _sym) in &routes {
                let mut n = symbols.get(lhs).map(|x| x.len()).unwrap_or(0);
                {
                    let to_add = symbols.get(&rhs0).unwrap().clone();
                    let target = symbols.get_mut(&lhs).unwrap();
                    for tmp in to_add {
                        target.insert(tmp);
                    }
                }
                rep |= n < symbols.get(lhs).map(|x| x.len()).unwrap_or(0);
                n = seeds.get(lhs).map(|x| x.len()).unwrap_or(0);
                {
                    let to_add = seeds.get(&rhs0).unwrap().clone();
                    let target = seeds.get_mut(&lhs).unwrap();
                    for tmp in to_add {
                        target.insert(tmp);
                    }
                }
                rep |= n < seeds.get(lhs).map(|x| x.len()).unwrap_or(0);
            }
        }
        return (symbols, seeds);
    }

    pub fn build_decision_table(k: usize, args: Vec<usize>, vectors: Vec<Vec<usize>>) {
        let mut fin_index: HashMap<Vec<usize>,usize> = HashMap::new();
        let mut fin_vectors: Vec<Vec<usize>> = Vec::new();
        let mut fin_tabs: Vec<HashMap<Option<S>,Vec<usize>>> = Vec::new();
        let mut conflicts: HashMap<(usize, Option<S>),Vec<usize>> = HashMap::new();
        let mut tab_index = fin_vectors.len();
        {
            let mut tmp: Vec<usize> = Vec::with_capacity(args.len() + 1);
            tmp.push(k);
            for arg in &args {
                tmp.push(*arg);
            }
            fin_index.insert(tmp, tab_index);
        }
        let mut tab: HashMap<Option<S>,Vec<usize>> = HashMap::new();
        fin_tabs.push(tab);
        assert_eq!(vectors[k].len(), args.len());
        let mut seed_lookahead: HashMap<Vec<usize>,usize> = HashMap::new();
        todo!();
    }
}

pub struct GrammarRefPrefixAndItemRef<'a,S> {
    grammar_ref: &'a Grammar<S>,
    prefix: &'a String,
    item: &'a Item,
}

pub struct GrammarRefPrefixAndItemSetRef<'a,S> {
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

impl<'a, S: std::fmt::Display> std::fmt::Display for GrammarRefPrefixAndItemRef<'a,S> {
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
                    prefix: if is_first { &first_prefix } else { &other_prefixes },
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
        Rule { name_op: None, parts: vec!["program"], },
        Rule { name_op: Some("program"), parts: vec![], },
        Rule { name_op: Some("program"), parts: vec!["program", "declaration"], },
        Rule { name_op: Some("declaration"), parts: vec!["varDecl"] },
        Rule { name_op: Some("declaration"), parts: vec!["constDecl"] },
        Rule { name_op: Some("declaration"), parts: vec!["statement"] },
    ]);
    let lexemes = Lexemes(vec![
        "varDecl",
        "constDecl",
        "statement",
    ]);
    let lr_parser = LrParser {
        grammar,
        lexemes,
    };
    let prediction = lr_parser.predict(vec![Item { rule: 0, index: 0 }]);
    println!("---");
    println!("{}", GrammarRefPrefixAndItemSetRef {
        grammar_ref: &lr_parser.grammar,
        prefix: "0: ".to_owned(),
        item_set: &prediction,
    });
    println!("---");
    let partition = lr_parser.partition(&prediction);
    println!("parition:");
    for (sym_op, item_set) in partition {
        print!("{}", GrammarRefPrefixAndItemSetRef {
            grammar_ref: &lr_parser.grammar,
            prefix: sym_op.unwrap_or("None").to_owned(),
            item_set: &item_set,
        });
    }
    println!("---");
    let (item_sets, full_item_sets, vectors) = lr_parser.compute_all_itemsets();
    println!("---");
    println!("create state 0 item set:");
    let state0_item_set = lr_parser.create_state_0_item_set();
    print!("{}", GrammarRefPrefixAndItemSetRef {
        grammar_ref: &lr_parser.grammar,
        prefix: "0".to_owned(),
        item_set: &state0_item_set,
    });
    println!("---");
    println!("following \"program\" from state 0 item set to make next item set state.");
    let state1_item_set = lr_parser.follow(
        &state0_item_set,
        "program",
    );
    print!("{}",
        GrammarRefPrefixAndItemSetRef {
            grammar_ref: &lr_parser.grammar,
            prefix: "1".to_owned(),
            item_set: &state1_item_set,
        }
    );
    println!("---");
    let first = lr_parser.first_lexemes();
    println!("{:?}", first);
    println!("---");
    let mut follow_sym = Vec::new();
    let mut follow_seeds = Vec::new();
    for i in 0..item_sets.len() {
        let (syms, seeds) = lr_parser.follow_lexemes(&item_sets[i].items.iter().map(|x| (x.rule, x.index)).collect(), &full_item_sets[i]);
        println!("{}", i);
        println!("{:?}", syms);
        println!("{:?}", seeds);
        follow_sym.push(syms);
        follow_seeds.push(seeds);
    }
}
