use {
    crate::{
        *,
        tree::*,
    },
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

pub type ParserResult = Result<SyntaxTree, ParserError>;
pub type OptionalParserResult<'a, T> = Result<Option<T>, ParserError>;

#[derive(Clone, Debug, PartialEq)]
pub enum ParserError {
    ExpectedEndOfInput,
    RuleNotExists { id: RuleId },
    RecursionExceededLimit,
    UnexpectedEndOfInput,
}

pub struct Parser<'a> {
    cake: &'a Cake,
    max_recursion: usize,
    recursion_count: usize,
    input: &'a str,
    index: usize,
}

impl<'a> Parser<'a> {
    pub fn parse(cake: &'a Cake, input: &'a str, max_recursion: usize) -> ParserResult {
        Parser::parse_rule(cake, &cake.start_rule_id, input, max_recursion)
    }

    pub fn parse_rule(cake: &'a Cake, rule_id: &RuleId, input: &'a str, max_recursion: usize) -> ParserResult {
        let mut parser = Parser {
            cake: cake,
            max_recursion: max_recursion,
            recursion_count: 0,
            input: input,
            index: 0,
        };

        parser.input = input;

        match parser.rule(rule_id, false) {
            Ok(option) => match option {
                Some(mut children) => if parser.index == parser.input.count() {
                    if let Some(first_child) = children.pop() {
                        if let SyntaxChild::Node(first_node) = first_child {
                            // When input matched successfully:
                            Ok(SyntaxTree::new(first_node))
                        } else {
                            unreachable!();
                        }
                    } else {
                        unreachable!();
                    }
                } else {
                    // When didn't reach end of input due to unmatch of input:
                    Err(ParserError::ExpectedEndOfInput)
                }
                // When reached end of input while parsing:
                None => Err(ParserError::UnexpectedEndOfInput)
            },
            Err(e) => Err(e),
        }
    }

    fn lookahead(&mut self, elem: &Rc<Element>) -> OptionalParserResult<Vec<SyntaxChild>> {
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

    fn lookahead_(&mut self, elem: &Rc<Element>, is_positive: bool) -> OptionalParserResult<Vec<SyntaxChild>> {
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

    fn times(&mut self, elem: &Rc<Element>) -> OptionalParserResult<Vec<SyntaxChild>> {
        if elem.loop_range.is_default() {
            self.expr(elem)
        } else {
            let tmp_index = self.index;
            let mut children = Vec::new();
            let mut count = 0;

            loop {
                let mut new_children = match self.expr(elem) {
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

    fn expr(&mut self, elem: &Rc<Element>) -> OptionalParserResult<Vec<SyntaxChild>> {
        let result = match &elem.kind {
            ElementKind::Element(elem) => self.lookahead(elem),
            ElementKind::Choice(elems) => self.choice(elems),
            ElementKind::Sequence(elems) => self.seq(elems),
            ElementKind::Rule(rule, expands) => self.rule(rule, *expands),
            ElementKind::String(s) => self.str(&s),
            ElementKind::Regex(regex) => self.regex(&regex),
            ElementKind::Wildcard => self.wildcard(),
            ElementKind::Skip => self.skip(),
            ElementKind::SeparatorSkip => unimplemented!(),
        };

        let result = match &elem.reflection {
            Reflection::Reflected => match result {
                Ok(option) => match option {
                    Some(children) => Ok(Some(Parser::proc_children_on_succeed(children, elem))),
                    None => Ok(None),
                },
                Err(e) => Err(e),
            },
            Reflection::ReflectedWithName(name) => match result {
                Ok(option) => match option {
                    Some(new_children) => Ok(Some(Parser::proc_children_on_succeed(vec![SyntaxChild::node(name.to_string(), new_children)], elem))),
                    None => Ok(None),
                },
                Err(e) => Err(e),
            },
            Reflection::Hidden => match result {
                Ok(option) => match option {
                    Some(_) => Ok(Some(Vec::new())),
                    None => Ok(None),
                },
                Err(e) => Err(e),
            },
        };

        match elem.callback {
            Some(callback) => match result {
                Ok(option) => Ok(callback(option)),
                Err(e) => Err(e),
            },
            None => result,
        }
    }

    fn choice(&mut self, subelems: &Vec<Rc<Element>>) -> OptionalParserResult<Vec<SyntaxChild>> {
        let tmp_index = self.index;

        for each_elem in subelems {
            match self.lookahead(each_elem) {
                Ok(option) => match option {
                    Some(children) => return Ok(Some(children)),
                    None => self.index = tmp_index,
                },
                Err(e) => return Err(e),
            }
        }

        Ok(None)
    }

    fn seq(&mut self, subelems: &Vec<Rc<Element>>) -> OptionalParserResult<Vec<SyntaxChild>> {
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

    fn rule(&mut self, id: &RuleId, expands: bool) -> OptionalParserResult<Vec<SyntaxChild>> {
        let elem = match self.cake.rule_map.get(id) {
            Some(v) => v,
            None => return Err(ParserError::RuleNotExists {
                id: id.clone(),
            }),
        };

        match self.lookahead(elem) {
            Ok(option) => match option {
                Some(new_children) => if expands {
                    Ok(Some(new_children))
                } else {
                    Ok(Some(vec![SyntaxChild::node(id.to_string(), new_children)]))
                },
                None => Ok(None),
            },
            Err(e) => Err(e),
        }
    }

    fn str(&mut self, s: &str) -> OptionalParserResult<Vec<SyntaxChild>> {
        if self.input.count() >= self.index + s.count() && self.input.slice(self.index, s.count()) == *s {
            self.index += s.count();
            Ok(Some(vec![SyntaxChild::leaf(s.to_string())]))
        } else {
            Ok(None)
        }
    }

    fn regex(&mut self, regex: &Regex) -> OptionalParserResult<Vec<SyntaxChild>> {
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

    fn wildcard(&mut self) -> OptionalParserResult<Vec<SyntaxChild>> {
        if self.input.count() >= self.index + 1 {
            let s = self.input.slice(self.index, 1);
            self.index += 1;
            Ok(Some(vec![SyntaxChild::leaf(s)]))
        } else {
            Ok(None)
        }
    }

    fn skip(&mut self) -> OptionalParserResult<Vec<SyntaxChild>> {
        Ok(Some(Vec::new()))
    }

    fn proc_children_on_succeed(children: Vec<SyntaxChild>, elem: &Element) -> Vec<SyntaxChild> {
        let children = if let LeafValueReplacement::ReplaceWith(value) = &elem.replacement {
            vec![SyntaxChild::leaf(value.clone())]
        } else {
            children
        };

        let children = if elem.join_children {
            let mut s = String::new();

            for each_child in children {
                s += &each_child.join_children();
            }

            vec![SyntaxChild::leaf(s)]
        } else {
            children
        };

        children
    }
}
