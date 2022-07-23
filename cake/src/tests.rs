use {
    crate::*,
    cake_derive::RuleContainer,
};

macro_rules! assert_parse {
    ($value:expr, $is_ok:expr) => {
        match $value {
            Ok(v) => match v {
                Some(_) if !$is_ok => panic!("Assertion panicked."),
                None if $is_ok => panic!("Assertion panicked."),
                _ => (),
            },
            Err(_) => panic!("Parsing failed."),
        }
    };
}

#[test]
fn tests() {
    use {
        crate::parser::*,
    };

    fn parse<'a>(cake: &Cake, input: &str) -> ParserResult<'a> {
        Parser::parse(cake, input, 100)
    }

    let mut cake = Cake::new();
    cake.add_module(Main::new());

    assert_parse!(parse(&cake, "a"), false);
    assert_parse!(parse(&cake, "aa"), false);
    assert_parse!(parse(&cake, "aaa"), false);
    assert_parse!(parse(&cake, "_"), false);
    assert_parse!(parse(&cake, "b"), false);
    assert_parse!(parse(&cake, "ab"), true);
    assert_parse!(parse(&cake, "aba"), false);
    assert_parse!(parse(&cake, "abab"), false);
    assert_parse!(parse(&cake, "ba"), false);
    assert_parse!(parse(&cake, "bb"), false);
    assert_parse!(parse(&cake, ""), false);
}

#[derive(Debug, RuleContainer)]
pub struct Main {
    pub main: Element,
}

impl Module for Main {
    fn new() -> Main {
        add_rules!{
            main := str("a") + str("b");
        }
    }
}
