use core::fmt;

#[derive(Default)]
pub struct Generator {
    text: String,
    line: String,
    indent: usize,
}

impl Generator {
    pub fn push(&mut self, char: char) {
        self.line.push(char);
    }

    pub fn push_str(&mut self, str: &str) {
        self.line.push_str(str);
    }

    pub fn newline(&mut self) {
        for _ in 0..self.indent {
            self.text.push(' ');
        }
        self.text.push_str(&self.line);
        self.line.clear();
    }

    fn inc(&mut self) {
        self.indent += 1;
    }

    fn dec(&mut self) {
        self.indent -= 1;
    }

    pub fn scope<T>(&mut self, inner: impl FnOnce(&mut Self) -> T) -> T {
        self.inc();
        let result = inner(self);
        self.dec();
        result
    }

    pub fn generate(mut self) -> String {
        if !self.line.is_empty() {
            self.newline();
        }
        self.text
    }
}
