use {
    crate::*,
    cake_derive::RuleContainer,
};

#[test]
fn test() {
    let mut cake = Cake::new();
    cake.add_module(Main::new());
    println!("{:?}", cake);
}

#[derive(Debug, RuleContainer)]
pub struct Main {
    pub number: Element,
    pub add: Element,
}

impl Module for Main {
    fn new() -> Main {
        add_rules!{
            number := char("a-z[") | (str("a").min(1).neg() | str("a")).neg().times(2) + str("a");
            add := Main::add() + char("a-z[") | (str("a").min(1).neg() | str("a")).neg().times(2) + str("a");
        }
    }
}
