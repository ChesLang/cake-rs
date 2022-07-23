pub mod parser;
mod tests;

use {
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
    ($($rule_name:ident := $rule_elem:expr;)*) => {
        {
            Self {
                $(
                    $rule_name: $rule_elem,
                )*
            }
        }
    };
}

pub fn str(s: &str) -> Element {
    if s == "" {
        panic!("Use skip() instead of empty string.");
    }

    Element::new(ElementKind::String(s.to_string()))
}

pub fn regex(pat: &str) -> Element {
    let regex = match Regex::new(pat) {
        Ok(v) => v,
        Err(_) => panic!("Regex pattern is invalid."),
    };

    Element::new(ElementKind::Regex(regex))
}

pub fn skip() -> Element {
    Element::new(ElementKind::Skip)
}

pub struct Cake {
    rule_map: HashMap<RuleId, Rc<Element>>,
}

impl Cake {
    pub fn new() -> Cake {
        Cake {
            rule_map: HashMap::new(),
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
}

impl Debug for Cake {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for Cake {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.rule_map.iter().map(|v| v.1.to_string()).collect::<Vec<String>>().join("\n"))
    }
}

pub trait Module: Debug + ModuleAssist {
    fn new() -> Self;
}

pub trait ModuleAssist: Debug {
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
pub struct RuleVec(Vec<Rule>);

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

#[derive(Clone, PartialEq)]
pub struct Element {
    pub kind: ElementKind,
    pub lookahead_kind: LookaheadKind,
    pub loop_range: LoopRange,
}

impl Element {
    pub fn new(kind: ElementKind) -> Element {
        Element {
            kind: kind,
            lookahead_kind: LookaheadKind::None,
            loop_range: LoopRange::default(),
        }
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
        self.min_to_max(times, times)
    }

    pub fn min(mut self, min: usize) -> Element {
        self.loop_range = LoopRange::validate_new(min, Maxable::Max);
        self
    }

    pub fn min_to_max(mut self, min: usize, max: usize) -> Element {
        self.loop_range = LoopRange::validate_new(min, Maxable::Specified(max));
        self
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
        write!(f, "{}{}{}", self.lookahead_kind, self.kind, self.loop_range)
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

#[derive(Clone, PartialEq)]
pub enum ElementKind {
    Choice(Vec<Rc<Element>>),
    Sequence(Vec<Rc<Element>>),
    Rule(RuleId),
    String(String),
    Regex(Regex),
    Wildcard,
    Skip,
}

impl Debug for ElementKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for ElementKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            ElementKind::Choice(elems) => format!("({})", elems.iter().map(|e| e.to_string()).collect::<Vec<String>>().join(" | ")),
            ElementKind::Sequence(elems) => format!("({})", elems.iter().map(|e| e.to_string()).collect::<Vec<String>>().join(" + ")),
            ElementKind::Rule(id) => id.to_string(),
            ElementKind::String(value) => value.clone(),
            ElementKind::Regex(regex) => format!("/{}/", regex.to_string()),
            ElementKind::Wildcard => "_".to_string(),
            ElementKind::Skip => "SKIP".to_string(),
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
    pub fn validate_new(min: usize, max: Maxable<usize>) -> LoopRange {
        match max {
            Maxable::Specified(n) if min > n => panic!("Max of loop range is bigger than min."),
            _ => (),
        }

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
        LoopRange::validate_new(1, Maxable::Specified(1))
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
