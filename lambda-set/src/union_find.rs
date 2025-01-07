#[derive(Debug)]
pub struct UnionFind {
    elems: Vec<usize>,
}

impl UnionFind {
    pub fn new() -> Self {
        Self { elems: Vec::new() }
    }

    pub fn token(&mut self) -> usize {
        let token = self.elems.len();
        self.elems.push(token);
        token
    }

    pub fn merge(&mut self, first: usize, second: usize) {
        println!("merging sets {first} and {second}");
        let first_root = self.root(first);
        self.elems[first_root] = self.root(second);
    }

    pub fn root(&mut self, token: usize) -> usize {
        if self.elems[token] == token {
            token
        } else {
            let root = self.root(self.elems[token]);
            self.elems[token] = root;
            root
        }
    }
}
