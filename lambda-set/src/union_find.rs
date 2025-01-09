#[derive(Debug)]
pub struct UnionFind {
    elems: Vec<usize>,
}

impl UnionFind {
    pub fn new() -> Self {
        Self { elems: vec![0] }
    }

    pub fn token(&mut self) -> usize {
        let token = self.elems.len();
        self.elems.push(token);
        token
    }

    pub fn merge(&mut self, first: usize, second: usize) {
        if first == 0 || second == 0 {
            panic!("unifying {first} and {second}");
        }
        println!("merging sets {first} and {second}");
        let first_root = self.root(first);
        self.elems[first_root] = self.root(second);
    }

    pub fn root(&mut self, token: usize) -> usize {
        if token == 0 {
            panic!("getting root of 0");
        }
        if self.elems[token] == token {
            token
        } else {
            let root = self.root(self.elems[token]);
            self.elems[token] = root;
            root
        }
    }
}
