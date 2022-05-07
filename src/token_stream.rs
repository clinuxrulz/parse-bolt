#[derive(Clone,Copy,Debug)]
pub struct Pos {
    pub offset: usize,
    pub line: usize,
    pub col: usize,
}

pub struct TokenStream<T> {
    tokens: Vec<T>,
    pos: Pos,
    t_to_char_op: Option<fn(&T)->char>,
}

impl<T> TokenStream<T> {
    pub fn read(&mut self) -> Option<T>
    where
        T: Clone,
    {
        if self.pos.offset >= self.tokens.len() {
            return None;
        }
        let t = self.tokens[self.pos.offset].clone();
        if let Some(t_to_char) = self.t_to_char_op {
            let t = t_to_char(&t);
            if t == '\n' || (self.pos.offset > 0) && t_to_char(&self.tokens[self.pos.offset - 1]) == '\r' {
                self.pos.line += 1;
                self.pos.col = 1;
            } else {
                self.pos.col += 1;
            }
        }
        self.pos.offset += 1;
        return Some(t);
    }

    pub fn save(&mut self) -> Pos {
        self.pos
    }

    pub fn restore(&mut self, at: Pos) {
        self.pos = at;
    }
}

impl TokenStream<char> {
    pub fn from_str(str: &str) -> TokenStream<char> {
        TokenStream {
            tokens: str.chars().collect(),
            pos: Pos {
                offset: 0,
                line: 1,
                col: 1,
            },
            t_to_char_op: Some(|t| *t),
        }
    }
}

impl<T> TokenStream<T> {
    pub fn from_vec(tokens: Vec<T>) -> TokenStream<T> {
        TokenStream {
            tokens,
            pos: Pos {
                offset: 0,
                line: 1,
                col: 1,
            },
            t_to_char_op: None,
        }
    }
}
