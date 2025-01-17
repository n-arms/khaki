pub struct UnionFind<T> {
    elems: Vec<T>,
}

impl<T> UnionFind<T> {
    pub fn new() -> Self {
        Self { elems: Vec::new() }
    }
}

impl<T: From<usize>> UnionFind<T> {
    pub fn new_with(elems: usize) -> Self {
        Self {
            elems: (0..elems).map(From::from).collect(),
        }
    }
}

impl<T: Into<usize> + From<usize> + Clone + Eq> UnionFind<T> {
    pub fn merge(&mut self, first: T, second: T) {
        let index = self.root(first).into();
        self.elems[index] = self.root(second);
    }

    pub fn root(&mut self, elem: T) -> T {
        if self.elems[elem.clone().into()] == elem.clone() {
            elem
        } else {
            let root = self.root(self.elems[elem.clone().into()].clone());
            self.elems[elem.clone().into()] = root.clone();
            root
        }
    }
}
