pub struct TokenStream<T> {
    tokens: Vec<T>,
    pos: usize,
}

impl<T> TokenStream<T> {
    pub fn read(&mut self) -> Option<T>
    where
        T: Clone,
    {
        if self.pos >= self.tokens.len() {
            return None;
        }
        let t = self.tokens[self.pos].clone();
        self.pos += 1;
        return Some(t);
    }

    pub fn save(&mut self) -> usize {
        self.pos
    }

    pub fn restore(&mut self, at: usize) {
        self.pos = at;
    }
}

impl TokenStream<char> {
    pub fn from_str(str: &str) -> TokenStream<char> {
        TokenStream {
            tokens: str.chars().collect(),
            pos: 0,
        }
    }
}

impl<T> TokenStream<T> {
    pub fn from_vec(tokens: Vec<T>) -> TokenStream<T> {
        TokenStream { tokens, pos: 0 }
    }
}
