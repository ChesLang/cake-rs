use {
    crate::*,
    cake_derive::RuleContainer,
};

#[test]
fn tests() {
    use {
        crate::parser::*,
    };

    fn parse<'a>(cake: &Cake, input: &str) -> Result<Option<SyntaxTree>, ParserError> {
        cake.parse(input, 1024)
    }

    let mut cake = Cake::new(RuleId("Main::main".to_string()));
    cake.add_module(Main::new());
    cake.add_module(Symbol::new());
    println!("{}", cake);

    macro_rules! assert_parse {
        ($value:expr, $is_ok:expr) => {
            match parse(&cake, $value) {
                Ok(v) => match v {
                    Some(_) if !$is_ok => panic!("Assertion panicked."),
                    None if $is_ok => panic!("Assertion panicked."),
                    _ => (),
                },
                Err(_) => panic!("Parsing failed."),
            }
        };
    }

    match parse(&cake, "b") {
        Ok(v) => match v {
            Some(v) => println!("{}", v),
            None => panic!("panicked."),
        },
        Err(_) => panic!("Parsing failed."),
    }
}

#[derive(RuleContainer)]
pub struct Main {
    main: Element,
    escseq: Element,
}

impl Module for Main {
    fn new() -> Main {
        add_rules!{
            // main := str("a").run(|result| println!("{:?}", result));
            main := (Main::escseq().expand() | wildcard()).zero_or_more();
            escseq := !str("\\") + (str("\\").replace("\\") | str("n").replace("\n") | str("t").replace("\t") /*| skip().run(unknown_escseq)*/);
            escseq := !str("\\") + g!(str("\\").replace("\\") | str("n").replace("\n") | str("t").replace("\t") /*| skip().run(unknown_escseq)*/);
        }
    }
}

#[derive(Debug, RuleContainer)]
pub struct Symbol {
    
}

impl Module for Symbol {
    fn new() -> Symbol {
        add_rules!{
            
        }
    }
}
