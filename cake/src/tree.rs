use {
    std::{
        fmt,
        fmt::{
            Display,
            Formatter,
        },
    },
};

#[macro_export]
macro_rules! tree {
    ($root:expr) => {
        match $root {
            SyntaxChild::Node(node) => SyntaxTree::new(node),
            _ => panic!("cannot set leaf as syntax tree root"),
        }
    };
}

#[macro_export]
macro_rules! node {
    ($name:expr => $children:expr) => {
        SyntaxChild::node($name.to_string(), $children)
    };
}

#[macro_export]
macro_rules! leaf {
    ($value:expr) => {
        SyntaxChild::leaf($value.to_string())
    };
}

pub trait ToNestedString {
    fn to_nested_string(&self, nest: usize) -> String;
}

#[derive(Clone, Debug, PartialEq)]
pub struct SyntaxTree {
    pub root: SyntaxNode,
}

impl SyntaxTree {
    pub fn new(root: SyntaxNode) -> SyntaxTree {
        SyntaxTree {
            root: root,
        }
    }
}

impl Display for SyntaxTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.root.to_nested_string(0))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum SyntaxChild {
    Node(SyntaxNode),
    Leaf(SyntaxLeaf),
}

impl SyntaxChild {
    pub fn node(name: String, children: Vec<SyntaxChild>) -> SyntaxChild {
        SyntaxChild::Node(SyntaxNode::new(name, children))
    }

    pub fn leaf(value: String) -> SyntaxChild {
        SyntaxChild::Leaf(SyntaxLeaf::new(value))
    }

    pub fn join_children(&self) -> String {
        let mut s = String::new();

        match self {
            SyntaxChild::Node(node) => for each_child in &node.children {
                s += &each_child.join_children()
            },
            SyntaxChild::Leaf(leaf) => s += &leaf.value,
        }

        s
    }
}

impl ToNestedString for SyntaxChild {
    fn to_nested_string(&self, nest: usize) -> String {
        match self {
            SyntaxChild::Node(v) => v.to_nested_string(nest),
            SyntaxChild::Leaf(v) => v.to_nested_string(nest),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SyntaxNode {
    pub name: String,
    pub children: Vec<SyntaxChild>,
}

impl SyntaxNode {
    pub fn new(name: String, children: Vec<SyntaxChild>) -> SyntaxNode {
        SyntaxNode {
            name: name,
            children: children,
        }
    }

    pub fn child_at(&self, i: usize) -> &SyntaxChild {
        match self.children.get(i) {
            Some(v) => v,
            None => panic!("Child index is invalid."),
        }
    }

    // todo: add node_at()
}

impl ToNestedString for SyntaxNode {
    fn to_nested_string(&self, nest: usize) -> String {
        format!("{}| {}{}", "  ".repeat(nest), self.name, self.children.iter().map(|v| format!("\n{}", v.to_nested_string(nest + 1))).collect::<Vec<String>>().join(""))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SyntaxLeaf {
    pub value: String,
    // todo: add `replaced_from`
}

impl SyntaxLeaf {
    pub fn new(value: String) -> SyntaxLeaf {
        SyntaxLeaf {
            value: value,
        }
    }
}

impl ToNestedString for SyntaxLeaf {
    fn to_nested_string(&self, nest: usize) -> String {
        format!("{}|- \"{}\"", "  ".repeat(nest), self.value)
    }
}
