use {
    crate::*
};

pub type ParserResult<'a> = Result<Option<Vec<SyntaxChild>>, ParserError>;

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
    pub fn parse(cake: &'a Cake, input: &'a str, max_recursion: usize) -> ParserResult<'a> {
        let mut parser = Parser {
            cake: cake,
            max_recursion: max_recursion,
            recursion_count: 0,
            input: input,
            index: 0,
        };

        parser.input = input;
        // fix
        let start_elem = parser.cake.rule_map.get(&RuleId("Main::main".to_string())).unwrap().clone();

        match parser.lookahead(&start_elem) {
            Ok(option) => {
                if parser.index == parser.input.len() {
                    Ok(option)
                } else {
                    Ok(None)
                }
            },
            Err(e) => Err(e),
        }
    }

    fn lookahead(&mut self, elem: &Rc<Element>) -> ParserResult {
        if self.recursion_count >= self.max_recursion {
            return Err(ParserError::RecursionExceededLimit);
        }

        self.recursion_count += 1;

        let result = match elem.lookahead_kind {
            LookaheadKind::None => self.times(elem),
            _ => unimplemented!(),
        };

        self.recursion_count -= 1;
        result
    }

    fn times(&mut self, elem: &Rc<Element>) -> ParserResult {
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

    fn expr(&mut self, elem_kind: &ElementKind) -> ParserResult {
        match elem_kind {
            ElementKind::Choice(elems) => self.choice(elems),
            ElementKind::Sequence(elems) => self.seq(elems),
            ElementKind::Rule(rule) => self.rule(rule),
            ElementKind::String(s) => self.str(&s),
            ElementKind::Wildcard => self.wildcard(),
            _ => unimplemented!(),
        }
    }

    fn choice(&mut self, subelems: &Vec<Rc<Element>>) -> ParserResult {
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

    fn seq(&mut self, subelems: &Vec<Rc<Element>>) -> ParserResult {
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

    fn rule(&mut self, id: &RuleId) -> ParserResult {
        let elem = match self.cake.rule_map.get(id) {
            Some(v) => v,
            None => return Err(ParserError::RuleNotExists {
                id: id.clone(),
            }),
        };

        self.lookahead(elem)
    }

    fn str(&mut self, s: &str) -> ParserResult {
        if self.input.len() >= self.index + s.len() && self.input[self.index..self.index + s.len()] == *s {
            self.index += s.len();
            Ok(Some(vec![SyntaxChild::leaf(s.to_string())]))
        } else {
            Ok(None)
        }
    }

    fn wildcard(&mut self) -> ParserResult {
        if self.input.len() >= self.index + 1 {
            let s = self.input[self.index..self.index + 1].to_string();
            self.index += 1;
            Ok(Some(vec![SyntaxChild::leaf(s)]))
        } else {
            Ok(None)
        }
    }
}

#[derive(Clone, Debug)]
pub struct SyntaxTree {
    pub root: SyntaxNode,
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

#[derive(Clone, Debug)]
pub struct SyntaxNode {
    pub children: Vec<SyntaxChild>,
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
