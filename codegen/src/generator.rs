pub struct Program {
    defs: Vec<Definition>,
    functions: Vec<Function>,
}

pub enum Kind {
    Enum,
    Union,
    Struct,
}

pub struct Definition {
    kind: Kind,
    name: String,
    fields: Vec<String>,
}

pub struct Function {
    result: String,
    name: String,
    arguments: Vec<(String, String)>,
    body: Option<Block>,
}

pub struct Block {
    statements: Vec<Statement>,
}

pub enum Statement {
    Line(String),
    Block(Block),
}

impl Program {
    pub fn definition(&mut self, kind: Kind, name: String, fields: Vec<String>) {
        self.defs.push(Definition { kind, name, fields });
    }

    pub fn function(&mut self, function: Function) {
        self.functions.push(function);
    }

    pub fn generate(self) -> String {
        for def in self.defs {
            def.generate();
        }

        for func in self.functions {
            func.generate();
        }
    }
}

impl Definition {
    pub fn generate(self) -> String {
        let mut output = String::from(self.kind.keyword());
        output.push(' ');
        output.push_str(&self.name);
        output.push_str(" {\n");
        for field in self.fields {
            output.push_str(&ind(1));
            output.push_str(&field);
            output.push_str(self.kind.delimeter());
            output.push('\n')
        }
        output.push_str("}\n");
        output
    }
}

impl Function {
    pub fn forward(result: String, name: String, arguments: Vec<(String, String)>) -> Self {
        Self {
            result,
            name,
            arguments,
            body: None,
        }
    }

    pub fn function(
        result: String,
        name: String,
        arguments: Vec<(String, String)>,
        body: Block,
    ) -> Self {
        Self {
            result,
            name,
            arguments,
            body: Some(body),
        }
    }

    pub fn generate(self) -> String {
        let mut output = self.result;
        output.push(' ');
        output.push_str(&self.name);
        output.push('(');
        commas_with(self.arguments, |(typ, name)| typ + " " + &name);
        output.push(')');
        if let Some(body) = self.body {
            output.push(' ');
            body.generate(1);
        } else {
            output.push_str(";\n");
        }
        output
    }
}

impl Kind {
    pub fn keyword(&self) -> &'static str {
        match self {
            Kind::Enum => "enum",
            Kind::Union => "union",
            Kind::Struct => "struct",
        }
    }

    pub fn delimeter(&self) -> &'static str {
        match self {
            Kind::Enum => ",",
            Kind::Union | Kind::Struct => ";",
        }
    }
}

impl Block {
    pub fn line(&mut self, string: String) {
        self.statements.push(Statement::Line(string));
    }

    pub fn block(&mut self, block: Block) {
        self.statements.push(Statement::Block(block));
    }

    pub fn generate(self, mut indent: usize) -> String {
        let mut output = String::from("{\n");
        indent += 1;
        for stmt in self.statements {
            output.push_str(&ind(indent));
            match stmt {
                Statement::Line(line) => {
                    output.push_str(&line);
                    output.push(';');
                }
                Statement::Block(block) => {
                    block.generate(indent + 1);
                }
            }
            output.push_str(";\n");
        }
        output.push('}');
        output
    }
}

impl From<String> for Statement {
    fn from(value: String) -> Self {
        Self::Line(value)
    }
}

pub fn ind(indent: usize) -> String {
    let mut output = String::new();
    for _ in 0..indent * 2 {
        output.push(' ');
    }
    output
}

pub fn commas<S: AsRef<str>>(iter: impl IntoIterator<Item = S>) -> String {
    let mut output = String::new();
    let mut first = true;

    for elem in iter.into_iter() {
        if first {
            first = false;
        } else {
            output.push_str(", ");
        }
        output.push_str(elem.as_ref());
    }

    output
}

pub fn commas_with<S: AsRef<str>, I>(
    iter: impl IntoIterator<Item = I>,
    mapper: impl FnMut(I) -> S,
) -> String {
    commas(iter.into_iter().map(mapper))
}
