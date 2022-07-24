use {
    crate::*,
    regex::Regex,
    unicode_segmentation::UnicodeSegmentation,
};

trait GraphemeCount {
    fn count(&self) -> usize;

    fn slice(&self, skip: usize, take: usize) -> String;
}

impl GraphemeCount for String {
    fn count(&self) -> usize {
        self.graphemes(true).count()
    }

    fn slice(&self, skip: usize, take: usize) -> String {
        self.chars().skip(skip).take(take).collect::<String>()
    }
}

impl GraphemeCount for str {
    fn count(&self) -> usize {
        self.graphemes(true).count()
    }

    fn slice(&self, skip: usize, take: usize) -> String {
        self.chars().skip(skip).take(take).collect::<String>()
    }
}

pub type ParserResult<'a, T> = Result<Option<T>, ParserError>;

#[derive(Clone, Debug)]
pub enum ParserError {
    RuleNotExists { id: RuleId },
    RecursionExceededLimit,
}

pub struct Parser<'a> {
    cake: &'a Cake,
    max_recursion: usize,
    recursion_count: usize,
    input: &'a str,
    index: usize,
}

impl<'a> Parser<'a> {
    pub fn parse(cake: &'a Cake, input: &'a str, max_recursion: usize) -> ParserResult<'a, SyntaxTree> {
        let mut parser = Parser {
            cake: cake,
            max_recursion: max_recursion,
            recursion_count: 0,
            input: input,
            index: 0,
        };

        parser.input = input;

        match parser.rule(&parser.cake.start_rule_id) {
            Ok(option) => match option {
                Some(child) => if parser.index == parser.input.count() {
                    Ok(Some(SyntaxTree::new(child)))
                } else {
                    Ok(None)
                },
                None => Ok(None)
            },
            Err(e) => Err(e),
        }
    }

    fn lookahead(&mut self, elem: &Rc<Element>) -> ParserResult<Vec<SyntaxChild>> {
        if self.recursion_count >= self.max_recursion {
            return Err(ParserError::RecursionExceededLimit);
        }

        self.recursion_count += 1;

        let result = match elem.lookahead_kind {
            LookaheadKind::None => self.times(elem),
            LookaheadKind::Positive => self.lookahead_(elem, true),
            LookaheadKind::Negative => self.lookahead_(elem, false),
        };

        self.recursion_count -= 1;
        result
    }

    fn lookahead_(&mut self, elem: &Rc<Element>, is_positive: bool) -> ParserResult<Vec<SyntaxChild>> {
        let tmp_index = self.index;
        let result = self.times(elem);

        match result {
            Ok(option) => {
                self.index = tmp_index;

                let has_succeeded = if is_positive {
                    option.is_some()
                } else {
                    option.is_none()
                };

                if has_succeeded {
                    Ok(Some(Vec::new()))
                } else {
                    Ok(None)
                }
            },
            Err(e) => Err(e),
        }
    }

    fn times(&mut self, elem: &Rc<Element>) -> ParserResult<Vec<SyntaxChild>> {
        if elem.loop_range.is_default() {
            self.expr(&elem.kind)
        } else {
            let tmp_index = self.index;
            let mut children = Vec::new();
            let mut count = 0;

            loop {
                let mut new_children = match self.expr(&elem.kind) {
                    Ok(option) => match option {
                        Some(new_children) => new_children,
                        _ => break,
                    },
                    Err(e) => return Err(e),
                };

                match elem.loop_range.max {
                    Maxable::Max => {
                        children.append(&mut new_children);
                        count += 1;
                    },
                    Maxable::Specified(max) => {
                        if count <= max {
                            children.append(&mut new_children);
                            count += 1;

                            if count == max {
                                break;
                            }
                        } else {
                            break;
                        }
                    },
                }
            }

            if count >= elem.loop_range.min {
                Ok(Some(children))
            } else {
                self.index = tmp_index;
                Ok(None)
            }
        }
    }

    fn expr(&mut self, elem_kind: &ElementKind) -> ParserResult<Vec<SyntaxChild>> {
        match elem_kind {
            ElementKind::Choice(elems) => self.choice(elems),
            ElementKind::Sequence(elems) => self.seq(elems),
            ElementKind::Rule(rule) => match self.rule(rule) {
                Ok(option) => match option {
                    Some(new_child) => Ok(Some(vec![SyntaxChild::Node(new_child)])),
                    None => Ok(None),
                },
                Err(e) => Err(e),
            },
            ElementKind::String(s) => self.str(&s),
            ElementKind::Regex(regex) => self.regex(&regex),
            ElementKind::Wildcard => self.wildcard(),
            ElementKind::Skip => self.skip(),
        }
    }

    fn choice(&mut self, subelems: &Vec<Rc<Element>>) -> ParserResult<Vec<SyntaxChild>> {
        let tmp_index = self.index;

        for each_elem in subelems {
            match self.lookahead(each_elem) {
                Ok(option) => match option {
                    Some(children) => return Ok(Some(children)),
                    None => (),
                },
                Err(e) => return Err(e),
            }
        }

        self.index = tmp_index;
        Ok(None)
    }

    fn seq(&mut self, subelems: &Vec<Rc<Element>>) -> ParserResult<Vec<SyntaxChild>> {
        let tmp_index = self.index;
        let mut children = Vec::new();

        for each_elem in subelems {
            match self.lookahead(each_elem) {
                Ok(mut option) => match &mut option {
                    Some(new_children) => children.append(new_children),
                    None => {
                        self.index = tmp_index;
                        return Ok(None);
                    },
                },
                Err(e) => return Err(e),
            }
        }

        Ok(Some(children))
    }

    fn rule(&mut self, id: &RuleId) -> ParserResult<SyntaxNode> {
        let elem = match self.cake.rule_map.get(id) {
            Some(v) => v,
            None => return Err(ParserError::RuleNotExists {
                id: id.clone(),
            }),
        };

        match self.lookahead(elem) {
            Ok(option) => match option {
                Some(new_children) => Ok(Some(SyntaxNode::new(id.to_string(), new_children))),
                None => Ok(None),
            },
            Err(e) => Err(e),
        }
    }

    fn str(&mut self, s: &str) -> ParserResult<Vec<SyntaxChild>> {
        if self.input.count() >= self.index + s.count() && self.input.slice(self.index, s.count()) == *s {
            self.index += s.count();
            Ok(Some(vec![SyntaxChild::leaf(s.to_string())]))
        } else {
            Ok(None)
        }
    }

    fn regex(&mut self, regex: &Regex) -> ParserResult<Vec<SyntaxChild>> {
        match regex.find(&self.input.slice(self.index, self.input.count() - self.index)) {
            Some(regex_match) => {
                if regex_match.start() != 0 {
                    Ok(None)
                } else {
                    let match_s = regex_match.as_str().to_string();
                    self.index += match_s.count();
                    Ok(Some(vec![SyntaxChild::leaf(match_s)]))
                }
            },
            None => Ok(None),
        }
    }

    fn wildcard(&mut self) -> ParserResult<Vec<SyntaxChild>> {
        if self.input.count() >= self.index + 1 {
            let s = self.input.slice(self.index, 1);
            self.index += 1;
            Ok(Some(vec![SyntaxChild::leaf(s)]))
        } else {
            Ok(None)
        }
    }

    fn skip(&mut self) -> ParserResult<Vec<SyntaxChild>> {
        Ok(Some(Vec::new()))
    }
}

pub trait ToNestedString {
    fn to_nested_string(&self, nest: usize) -> String;
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum SyntaxChild {
    Node(SyntaxNode),
    Leaf(SyntaxLeaf),
}

impl SyntaxChild {
    pub fn leaf(value: String) -> SyntaxChild {
        SyntaxChild::Leaf(SyntaxLeaf::new(value))
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

#[derive(Clone, Debug)]
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
}

impl ToNestedString for SyntaxNode {
    fn to_nested_string(&self, nest: usize) -> String {
        format!("{}| {}{}", "  ".repeat(nest), self.name, self.children.iter().map(|v| format!("\n{}", v.to_nested_string(nest + 1))).collect::<Vec<String>>().join(""))
    }
}

#[derive(Clone, Debug)]
pub struct SyntaxLeaf {
    pub value: String,
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
        format!("{}|- {}", "  ".repeat(nest), self.value.replace("\n", "\\n").replace("\t", "\\t"))
    }
}
