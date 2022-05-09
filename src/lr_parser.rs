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

#[derive(Clone, Copy)]
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
