use {
    crate::*,
    crate::parser::*,
    cake_derive::RuleContainer,
    speculate::speculate,
};

speculate!{
    before {
        let cake = &mut Cake::new(RuleId("Main::main".to_string()));
        cake.add_module(ParsingTest::new());

        let assert_ast = |input: &str, rule_id: &str, expected: ParserResult|
            assert_eq!(Parser::parse_rule(cake, &RuleId(rule_id.to_string()), input, 1024), expected);

        #[allow(unused)]
        let expect_success = |input: &str, rule_id: &str, expected: SyntaxTree|
            assert_ast(input, rule_id, Ok(expected));

        #[allow(unused)]
        let expect_failure = |input: &str, rule_id: &str, expected: ParserError|
            assert_ast(input, rule_id, Err(expected));
    }

    it "parse string" {
        expect_success("s", "ParsingTest::string", tree!{
            node!{
                "ParsingTest::string" => vec![
                    leaf!("s"),
                ]
            }
        });
    }

    it "parse too short string" {
        expect_failure("", "ParsingTest::string", ParserError::UnexpectedEndOfInput);
    }

    it "parse too long string" {
        expect_failure("ss", "ParsingTest::string", ParserError::ExpectedEndOfInput);
    }

    it "parse single skip" {
        expect_failure("ss", "ParsingTest::string", ParserError::ExpectedEndOfInput);
    }

    it "parse skip in choice" {
        expect_success("", "ParsingTest::skip_in_choice", tree!{
            node!{
                "ParsingTest::skip_in_choice" => vec![]
            }
        });
    }

    it "parse skip in sequence" {
        expect_success("..", "ParsingTest::skip_in_sequence", tree!{
            node!{
                "ParsingTest::skip_in_sequence" => vec![
                    leaf!("."),
                    leaf!("."),
                ]
            }
        });
    }
}

#[derive(RuleContainer)]
struct ParsingTest {
    string: Element,
    single_skip: Element,
    skip_in_choice: Element,
    skip_in_sequence: Element,
}

impl Module for ParsingTest {
    fn new() -> ParsingTest {
        add_rules!{
            string := str("s");
            single_skip := skip();
            skip_in_choice := wildcard() | skip();
            skip_in_sequence := wildcard() + skip() + wildcard();
        }
    }
}
