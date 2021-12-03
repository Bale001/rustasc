/// A `SourceReader` is an iterator that keeps track of the current position, and also allows peeking.
#[derive(Clone)]
pub struct SourceReader<'a> {
    src: &'a str,
    index: usize,
}

impl<'a> Iterator for SourceReader<'a> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        let a = self.src.chars().nth(self.index);
        self.index += 1;
        a
    }
}

impl<'a> SourceReader<'a> {
    pub fn new(src: &'a str) -> Self {
        Self { src, index: 0 }
    }
    /// Peek multiple characters from the current index
    pub fn multipeek(&mut self, amnt: usize) -> Option<&'a str> {
        self.src.get(self.index..).and_then(|s| s.get(..amnt))
    }

    /// Peek the next character from the current index.
    pub fn peek(&mut self) -> Option<char> {
        self.src.chars().nth(self.index)
    }

    /// Peeks the next character from the current index, and checks if it is equal to the `expect` argument.
    pub fn peek_eq(&mut self, expect: char) -> bool {
        self.peek().map(|c| c == expect).unwrap_or(false)
    }

    pub fn next_eq(&mut self, expect: char) -> bool {
        if self.peek_eq(expect) {
            self.index += 1;
            true
        } else {
            false
        }
    }

    pub fn peek_until(&mut self, predicate: fn(&char) -> bool) {
        while let Some(c) = self.peek() {
            if predicate(&c) {
                return;
            }
            self.index += 1;
        }
    }

    pub fn next_match<T: Copy>(&mut self, expects: &[(char, T)], default: T) -> T {
        for (expect, ret) in expects {
            if self.peek_eq(*expect) {
                self.index += 1;
                return *ret;
            }
        }
        default
    }

    pub fn next_match_multi<T: Copy>(&mut self, expects: &[(&'a str, T)], default: T) -> T {
        for (expect, ret) in expects {
            if self.multipeek(expect.len()) == Some(expect) {
                self.index += expect.len();
                return *ret;
            }
        }
        default
    }

    pub fn read_until_peek(&mut self, predicate: fn(&char) -> bool) -> &'a str {
        let original = self.index;

        self.peek_until(predicate);
        self.src.get(original..self.index).unwrap()
    }

    pub fn advance(&mut self, amnt: usize) {
        self.index += amnt;
    }

    pub fn advance_back(&mut self, amnt: usize) {
        self.index -= amnt;
    }

    pub fn read_until(&mut self, predicate: fn(&char) -> bool) -> &'a str {
        let original = self.index;

        self.find(predicate);
        self.src.get(original..self.index - 1).unwrap()
    }
}
