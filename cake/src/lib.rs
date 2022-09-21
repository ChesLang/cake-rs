pub mod parser;
pub mod tree;
#[cfg(test)]
mod tests;

use {
    crate::{
        parser::{
            Parser,
            ParserResult,
        },
        tree::*,
    },
    regex::Regex,
    std::{
        collections::HashMap,
        fmt,
        fmt::{
            Debug,
            Display,
            Formatter,
        },
        ops::{
            Add,
            BitOr,
            Not,
        },
        rc::Rc,
    },
};

#[macro_export]
macro_rules! new_mod {
    ($($rule_name:ident,)*) => {
        Self {
            $(
                $rule_name: RuleId::generate_id(),
            )*
        }
    };
}

#[macro_export]
macro_rules! add_rules {
    ($($rule_name:ident $([$separator:expr])? := $rule_elem:expr;)*) => {
        {
            Self {
                $(
                    $rule_name: $rule_elem $(.separate($separator, true))?,
                )*
            }
        }
    };
}

// grouping feature
#[macro_export]
macro_rules! g {
    ($elem:expr) => {
        Element::new(ElementKind::Element(Rc::new($elem)))
    };
}

pub fn str(s: &str) -> Element {
    if s == "" {
        panic!("Use skip() instead of empty string.");
    }

    Element::new(ElementKind::String(s.to_string()))
}

pub fn regex(pat: &str) -> Element {
    if pat == "" {
        panic!("Use skip() instead of regex(\"\").");
    }

    if pat == "." {
        panic!("Use wildcard() instead of regex(\".\").");
    }

    let regex = match Regex::new(pat) {
        Ok(v) => v,
        Err(_) => panic!("Regex pattern is invalid."),
    };

    Element::new(ElementKind::Regex(regex))
}

pub fn wildcard() -> Element {
    Element::new(ElementKind::Wildcard)
}

pub fn skip() -> Element {
    Element::new(ElementKind::Skip)
}

pub fn sep_skip() -> Element {
    Element::new(ElementKind::SeparatorSkip)
}

pub struct Cake {
    rule_map: HashMap<RuleId, Rc<Element>>,
    start_rule_id: RuleId,
}

impl Cake {
    pub fn new(start_rule_id: RuleId) -> Cake {
        Cake {
            rule_map: HashMap::new(),
            start_rule_id: start_rule_id,
        }
    }

    pub fn add_module<T: ModuleAssist>(&mut self, module: T) {
        let rules: Vec<Rule> = module.into_rule_vec().into();

        for each_rule in rules {
            let id = each_rule.id.clone();

            if self.rule_map.contains_key(&id) {
                panic!("Rule ID `{}` is already declared.", id);
            }

            self.rule_map.insert(id, each_rule.element);
        }
    }

    pub fn parse<'a>(&self, input: &'a str, max_recursion: usize) -> ParserResult {
        Parser::parse(self, input, max_recursion)
    }
}

impl Debug for Cake {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for Cake {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.rule_map.iter().map(|(id, rule)| format!("{} := {};", id, rule)).collect::<Vec<String>>().join("\n"))
    }
}

pub trait Module: ModuleAssist {
    fn new() -> Self;
}

pub trait ModuleAssist {
    fn into_rule_vec(self) -> RuleVec;
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct RuleId(pub String);

impl Debug for RuleId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for RuleId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone)]
pub struct Rule {
    pub id: RuleId,
    pub element: Rc<Element>,
}

impl Rule {
    pub fn new(id: RuleId) -> Rule {
        Rule {
            id: id,
            element: Rc::new(Element::new(ElementKind::Skip)),
        }
    }

    pub fn from(id: RuleId, elem: Element) -> Rule {
        Rule {
            id: id,
            element: Rc::new(elem),
        }
    }

    pub fn detect_left_recursion(self) -> Rule {
        if self.element.has_left_recursion(&self.id) {
            panic!("Left recursion detected at rule definition of `{}`.", self.id);
        }

        self
    }
}

impl Debug for Rule {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for Rule {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} := {};", self.id, self.element)
    }
}

#[derive(Clone)]
pub struct RuleVec(pub Vec<Rule>);

impl Debug for RuleVec {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for RuleVec {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.iter().map(|r| r.to_string()).collect::<Vec<String>>().join("\n"))
    }
}

impl Into<Vec<Rule>> for RuleVec {
    fn into(self) -> Vec<Rule> {
        self.0
    }
}

pub type ElementParserOption = Option<Vec<SyntaxChild>>;
pub type ElementCallback = fn(result: ElementParserOption) -> ElementParserOption;

#[derive(Clone)]
pub struct Element {
    pub kind: ElementKind,
    pub reflection: Reflection,
    pub replacement: LeafValueReplacement,
    pub join_children: bool,
    pub lookahead_kind: LookaheadKind,
    pub loop_range: LoopRange,
    has_loop_range_min_set: bool,
    has_loop_range_max_set: bool,
    pub callback: Option<ElementCallback>,
}

impl Element {
    pub fn new(kind: ElementKind) -> Element {
        Element {
            kind: kind,
            reflection: Reflection::default(),
            replacement: LeafValueReplacement::default(),
            join_children: false,
            lookahead_kind: LookaheadKind::None,
            loop_range: LoopRange::default(),
            has_loop_range_min_set: false,
            has_loop_range_max_set: false,
            callback: None,
        }
    }

    fn has_left_recursion(&self, rule_id: &RuleId) -> bool {
        match &self.kind {
            ElementKind::Element(elem) => elem.has_left_recursion(rule_id),
            ElementKind::Choice(elems) | ElementKind::Sequence(elems) => match elems.get(0) {
                Some(first_elem) => first_elem.has_left_recursion(rule_id),
                None => false,
            },
            ElementKind::Rule(id, _) => *rule_id == *id,
            _ => false,
        }
    }

    pub fn group(self) -> Element {
        Element::new(ElementKind::Element(Rc::new(self)))
    }

    pub fn enclose(self, encloser: Element) -> Element {
        let encloser_ptr = Rc::new(encloser);

        let elems = vec![
            encloser_ptr.clone(),
            Rc::new(self),
            encloser_ptr,
        ];

        Element::new(ElementKind::Sequence(elems))
    }

    pub fn join(mut self) -> Element {
        if self.join_children {
            panic!("Child leave joining is already set.");
        }

        self.join_children = true;
        self
    }

    pub fn separate(self, separator: Element, recurse: bool) -> Element {
        self.separate_(&Rc::new(separator), recurse)
    }

    fn separate_(&self, separator: &Rc<Element>, recurse: bool) -> Element {
        match &self.kind {
            ElementKind::Sequence(elems) => {
                let mut new_elems = vec![separator.clone()];

                for each_elem in elems {
                    if let ElementKind::SeparatorSkip = each_elem.kind {
                        new_elems.pop();
                    }

                    if recurse && matches!(&each_elem.kind, ElementKind::Sequence(_)) {
                        new_elems.push(Rc::new(each_elem.separate_(separator, true)));
                    } else {
                        new_elems.push(each_elem.clone());
                    };

                    if !matches!(&each_elem.kind, ElementKind::Skip | ElementKind::SeparatorSkip) {
                        new_elems.push(separator.clone());
                    }
                }

                Element::new(ElementKind::Sequence(new_elems))
            },
            _ => panic!("Cannot separate elements other than sequences."),
        }
    }

    pub fn expand(self) -> Element {
        if let ElementKind::Rule(rule, expands) = self.kind {
            if expands {
                panic!("Rule is already set to expanded.");
            }

            Element::new(ElementKind::Rule(rule, true))
        } else {
            panic!("Cannot expand elements other than rules.");
        }
    }

    pub fn name(mut self, name: &str) -> Element {
        if self.reflection != Reflection::default() {
            panic!("Reflection is already set.");
        }

        self.reflection = Reflection::ReflectedWithName(name.to_string());
        self
    }

    pub fn replace(mut self, to: &str) -> Element {
        if self.replacement != LeafValueReplacement::default() {
            panic!("Replacement value is already set.");
        }

        if self.reflection == Reflection::Hidden {
            panic!("Cannot replace value of hidden element.");
        }

        self.replacement = LeafValueReplacement::ReplaceWith(to.to_string());
        self
    }

    pub fn run(mut self, callback: ElementCallback) -> Element {
        self.callback = Some(callback);
        self
    }

    fn hide_(mut self) -> Element {
        // todo: 先読みとの併用を禁止?

        if self.reflection != Reflection::default() {
            panic!("Reflection is already set.");
        }

        if let LeafValueReplacement::ReplaceWith(_) = self.replacement {
            panic!("Cannot hide element which value replaced.");
        }

        self.reflection = Reflection::Hidden;
        self
    }

    fn set_lookahead_kind(mut self, kind: LookaheadKind) -> Element {
        if self.lookahead_kind != LookaheadKind::None {
            panic!("Lookahead kind is already set.");
        }

        self.lookahead_kind = kind;
        self
    }

    pub fn pos(self) -> Element {
        self.set_lookahead_kind(LookaheadKind::Positive)
    }

    pub fn neg(self) -> Element {
        self.set_lookahead_kind(LookaheadKind::Negative)
    }

    pub fn times(self, times: usize) -> Element {
        if self.has_loop_range_min_set || self.has_loop_range_max_set {
            let reason = if self.has_loop_range_min_set && self.has_loop_range_max_set {
                "cannot use times() multiple times."
            } else if self.has_loop_range_min_set {
                "cannot use times() with min()."
            } else {
                "cannot use times() with max()."
            };

            panic!("Loop range is already set; {}", reason);
        }

        if times == 0 {
            panic!("Detected 0 times of loop.");
        }

        if times == 1 {
            panic!("times(1) is the default.");
        }

        self.max_(times, times)
    }

    pub fn min(self, min: usize) -> Element {
        if self.has_loop_range_min_set || self.has_loop_range_max_set {
            let reason = if self.has_loop_range_min_set {
                if self.has_loop_range_max_set {
                    "cannot use min() with times()."
                } else {
                    "cannot use min() multiple times."
                }
            } else {
                "use min() before max()."
            };

            panic!("Loop range is already set; {}", reason);
        }

        self.min_(min)
    }

    fn min_(mut self, min: usize) -> Element {
        self.has_loop_range_min_set = true;
        self.loop_range = LoopRange::new(min, Maxable::Max);
        self
    }

    pub fn max(self, max: usize) -> Element {
        if self.has_loop_range_max_set {
            let reason = if self.has_loop_range_min_set {
                "cannot use max() with times()."
            } else {
                "cannot use max() multiple times."
            };

            panic!("Max number is already set; {}", reason);
        }

        if max == 0 {
            panic!("Cannot set 0 times as max number.");
        }

        if self.loop_range.min == 0 {
            panic!("min(0) is not necessary.");
        }

        let min = if self.has_loop_range_min_set {
            self.loop_range.min
        } else {
            0
        };

        if min > max {
            panic!("Cannot set smaller number than min.");
        }

        if min == max {
            panic!("Min and max number are duplicate; use times() instead.");
        }

        self.max_(min, max)
    }

    fn max_(mut self, min: usize, max: usize) -> Element {
        self.has_loop_range_max_set = true;
        self.loop_range = LoopRange::new(min, Maxable::Specified(max));
        self
    }

    // Alias of `e.max(1)`.
    pub fn optional(self) -> Element {
        self.max(1)
    }

    // Alias of `e.min(0)`.
    pub fn zero_or_more(self) -> Element {
        self.min(0)
    }

    // Alias of `e.min(1)`.
    pub fn one_or_more(self) -> Element {
        self.min(1)
    }

    fn to_choice_elements(self) -> Vec<Rc<Element>> {
        if self.is_choice() {
            match self.kind {
                ElementKind::Choice(elems) => elems,
                _ => unreachable!(),
            }
        } else {
            vec![Rc::new(self)]
        }
    }

    fn to_sequence_elements(self) -> Vec<Rc<Element>> {
        if self.is_sequence() {
            match self.kind {
                ElementKind::Sequence(elems) => elems,
                _ => unreachable!(),
            }
        } else {
            vec![Rc::new(self)]
        }
    }

    fn is_choice(&self) -> bool {
        match self.kind {
            ElementKind::Choice(_) => true,
            _ => false,
        }
    }

    fn is_sequence(&self) -> bool {
        match self.kind {
            ElementKind::Sequence(_) => true,
            _ => false,
        }
    }
}

impl Debug for Element {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for Element {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = format!("{}{}{}{}", self.lookahead_kind, self.kind, self.loop_range, self.reflection);

        let s = if self.join_children {
            format!("JOIN{{{}}}", s)
        } else {
            s
        };

        write!(f, "{}", s)
    }
}

impl BitOr for Element {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        let mut first = self.to_choice_elements();
        let mut second = rhs.to_choice_elements();
        first.append(&mut second);
        Element::new(ElementKind::Choice(first))
    }
}

impl Add for Element {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let mut first = self.to_sequence_elements();
        let mut second = rhs.to_sequence_elements();
        first.append(&mut second);
        Element::new(ElementKind::Sequence(first))
    }
}

impl Not for Element {
    type Output = Self;

    fn not(self) -> Self::Output {
        self.hide_()
    }
}

#[derive(Clone)]
pub enum ElementKind {
    // Supports a single covered element. (like `((e):mytag1):mytag2`)
    Element(Rc<Element>),
    Choice(Vec<Rc<Element>>),
    Sequence(Vec<Rc<Element>>),
    Rule(RuleId, bool),
    String(String),
    Regex(Regex),
    Wildcard,
    Skip,
    SeparatorSkip,
}

impl Debug for ElementKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for ElementKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            ElementKind::Element(elem) => format!("({})", elem),
            ElementKind::Choice(elems) => format!("({})", elems.iter().map(|e| e.to_string()).collect::<Vec<String>>().join(" | ")),
            ElementKind::Sequence(elems) => format!("({})", elems.iter().map(|e| e.to_string()).collect::<Vec<String>>().join(" + ")),
            ElementKind::Rule(id, expands) => format!("{}{}", id, if *expands { "#" } else { "" }),
            ElementKind::String(value) => format!("\"{}\"", value),
            ElementKind::Regex(regex) => format!("/{}/", regex.to_string()),
            ElementKind::Wildcard => "_".to_string(),
            ElementKind::Skip => "SKIP".to_string(),
            ElementKind::SeparatorSkip => "SEP_SKIP".to_string(),
        };

        write!(f, "{}", s)
    }
}

#[derive(Clone, PartialEq)]
pub enum Reflection {
    Reflected,
    ReflectedWithName(String),
    Hidden
}

impl Default for Reflection {
    fn default() -> Reflection {
        Reflection::Reflected
    }
}

impl Display for Reflection {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            Reflection::Reflected => ":".to_string(),
            Reflection::ReflectedWithName(name) => format!(":{}", name),
            Reflection::Hidden => String::new(),
        };

        write!(f, "{}", s)
    }
}

#[derive(Clone, PartialEq)]
pub enum LeafValueReplacement {
    ReplaceWith(String),
    NoReplacement,
}

impl Default for LeafValueReplacement {
    fn default() -> LeafValueReplacement {
        LeafValueReplacement::NoReplacement
    }
}

impl Display for LeafValueReplacement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            LeafValueReplacement::ReplaceWith(name) => format!(":{}", name),
            LeafValueReplacement::NoReplacement => String::new(),
        };

        write!(f, "{}", s)
    }
}

#[derive(Clone, PartialEq)]
pub enum LookaheadKind {
    None,
    Positive,
    Negative,
}

impl Debug for LookaheadKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for LookaheadKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            LookaheadKind::None => "",
            LookaheadKind::Positive => "&",
            LookaheadKind::Negative => "!",
        };

        write!(f, "{}", s)
    }
}

#[derive(Clone, PartialEq)]
pub struct LoopRange {
    min: usize,
    max: Maxable<usize>,
}

impl LoopRange {
    pub fn new(min: usize, max: Maxable<usize>) -> LoopRange {
        LoopRange {
            min: min,
            max: max,
        }
    }

    pub fn is_default(&self) -> bool {
        self.min == 1 && self.max == Maxable::Specified(1)
    }
}

impl Debug for LoopRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Default for LoopRange {
    fn default() -> LoopRange {
        LoopRange::new(1, Maxable::Specified(1))
    }
}

impl Display for LoopRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self.max {
            Maxable::Max => format!("{{{}-}}", self.min),
            Maxable::Specified(n) => if self.min == n {
                if self.min != 1 {
                    format!("{{{}}}", self.min)
                } else {
                    String::new()
                }
            } else {
                format!("{{{}-{}}}", self.min, n)
            },
        };

        write!(f, "{}", s)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Maxable<T> {
    Max,
    Specified(T),
}
