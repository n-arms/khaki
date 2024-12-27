use std::io;

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
        self.text.push('\n');
        self.line.clear();
    }

    pub fn inc(&mut self) {
        self.indent += 1;
    }

    pub fn dec(&mut self) {
        self.indent -= 1;
    }

    pub fn scope<T>(&mut self, inner: impl FnOnce(&mut Self) -> T) -> T {
        self.inc();
        let result = inner(self);
        self.dec();
        result
    }

    pub fn comma_list<T>(
        &mut self,
        list: impl IntoIterator<Item = T>,
        mut func: impl FnMut(&mut Self, T),
    ) {
        self.scope(|mut gen| {
            let mut first = true;
            for elem in list {
                if first {
                    first = false;
                } else {
                    gen.push_str(", ");
                }
                func(&mut gen, elem);
            }
        });
    }

    pub fn generate(mut self) -> String {
        if !self.line.is_empty() {
            self.newline();
        }
        self.text
    }
}

impl io::Write for Generator {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        for byte in buf {
            self.push(*byte as char);
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[derive(Default)]
pub struct Env {
    pub preamble: Generator,
    pub main: Generator,
    names: usize,
}

impl Env {
    pub fn fresh_name(&mut self, prefix: &str) -> String {
        let name = self.names;
        self.names += 1;
        format!("{prefix}_{name}")
    }

    pub fn generate(self) -> String {
        let mut text = String::from("//=== preamble ===\n");
        text.push_str(&self.preamble.generate());
        text.push_str("//=== main ===\n");
        text.push_str(&self.main.generate());
        text
    }
}
