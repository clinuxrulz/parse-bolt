use std::collections::HashSet;

// https://boxbase.org/entries/2019/oct/14/lr1-parsing-tables/

pub struct LrParser<S> {
    grammar: Grammar<S>,
    lexemes: Lexemes<S>,
}

impl<S> LrParser<S> {
    pub fn after_dot(&self, rule: usize, index: usize) -> Option<S> where S: Clone {
        let rule = &self.grammar.0[rule];
        if index < rule.parts.len() {
            Some(rule.parts[index].clone())
        } else {
            None
        }
    }

    pub fn predict(&self, mut items: Vec<Item>) -> ItemSet where S: Clone + PartialEq {
        let mut prediction: HashSet<Item> = HashSet::new();
        for item in &items {
            prediction.insert(*item);
        }
        let mut p = prediction.len();
        while let Some(item) = items.pop() {
            let sym_op = self.after_dot(item.rule, item.index);
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
}

pub struct Grammar<S>(Vec<Rule<S>>);

pub struct Lexemes<S>(Vec<S>);

pub struct Rule<S> {
    name_op: Option<S>,
    parts: Vec<S>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Item {
    rule: usize,
    index: usize,
}

#[derive(Clone)]
pub struct ItemSet {
    items: Vec<Item>,
}

pub struct GrammarRefPrefixAndItemRef<'a,S> {
    grammar_ref: &'a Grammar<S>,
    prefix: &'a String,
    item: &'a Item,
}

pub struct GrammarRefIndexAndItemSetRef<'a,S> {
    grammar_ref: &'a Grammar<S>,
    index: usize,
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

impl<'a, S: std::fmt::Display> std::fmt::Display for GrammarRefIndexAndItemSetRef<'a, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let first_prefix = format!("{}: ", self.index);
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
    println!("{}", GrammarRefIndexAndItemSetRef {
        grammar_ref: &lr_parser.grammar,
        index: 0,
        item_set: &prediction
    });
}
