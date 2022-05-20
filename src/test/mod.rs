mod backtracking;
mod iteratee_parser2;
mod kotlin;
mod kotlin2;
mod python;

#[cfg(test)]
use super::Parser;

#[test]
fn parse_test_exactly() {
    let parser: Parser<String, char, Vec<char>> = Parser::match_('1').map_to('1').exactly_vec(2);
    let input = "11";
    let r = parser.run_str(input);
    println!("{:?}", r)
}

#[test]
fn parse_test_match_eof_matched() {
    let parser: Parser<String, char, ()> = Parser::eof();
    let input = "";
    let r = parser.run_str(input);
    assert_eq!(r, Ok(()));
}

#[test]
fn parse_test_match_eof_unmatched() {
    let parser: Parser<String, char, ()> = Parser::eof();
    let input = "a";
    let r = parser.run_str(input);
    assert!(match r {
        Err(_) => true,
        _ => false,
    });
}

#[test]
fn parse_test_match_char_matched() {
    let parser: Parser<String, char, char> = Parser::match_('a').map_to('a');
    let input = "a";
    let r = parser.run_str(input);
    assert_eq!(r, Ok('a'));
}

#[test]
fn parse_test_march_char_unmatched() {
    let parser: Parser<String, char, char> = Parser::match_('a');
    let input = "b";
    let r = parser.run_str(input);
    assert!(match r {
        Err(_) => true,
        _ => false,
    });
}

#[test]
fn parse_test_match_string_matched() {
    let parser: Parser<String, char, ()> = Parser::match_string("cat");
    let input = "cat";
    let r = parser.run_str(input);
    assert_eq!(r, Ok(()));
}

#[test]
fn parse_test_match_string_unmatched() {
    let parser: Parser<String, char, ()> = Parser::match_string("cat");
    let input = "dog";
    let r = parser.run_str(input);
    assert!(match r {
        Err(_) => true,
        _ => false,
    });
}

#[test]
fn parse_test_or_else() {
    #[derive(Clone, Debug, PartialEq, Eq)]
    enum Animal {
        Cat,
        Dog,
        Rabbit,
    }
    let parser: Parser<String, char, Animal> = Parser::unordered_choice(vec![
        Parser::match_string("cat").map_to(Animal::Cat),
        Parser::match_string("dog").map_to(Animal::Dog),
        Parser::match_string("rabbit").map_to(Animal::Rabbit),
    ]);
    let input = "dog";
    let r = parser.run_str(input);
    assert_eq!(r, Ok(Animal::Dog));
}

#[test]
fn parse_test_int() {
    let parser = match_int().seq_left(&Parser::eof());
    let input = "42";
    let r = parser.run_str(input);
    assert_eq!(r, Ok(42));
}

#[test]
fn parse_test_seq() {
    #[derive(Clone, Debug, PartialEq, Eq)]
    enum Animal {
        Cat,
        Dog,
    }
    let parser: Parser<String, char, (Animal, Animal)> = Parser::match_string("cat")
        .map_to(Animal::Cat)
        .seq2(&Parser::match_string("dog").map_to(Animal::Dog));
    let input = "catdog";
    let r = parser.run_str(input);
    assert_eq!(r, Ok((Animal::Cat, Animal::Dog)));
}

#[test]
fn parse_test_optional() {
    let parser: Parser<String, char, Option<char>> = Parser::match_('a')
        .map_to('a')
        .optional()
        .seq_left(&Parser::eof());
    {
        let input = "a";
        let r = parser.run_str(input);
        assert_eq!(r, Ok(Some('a')));
    }
    {
        let input = "";
        let r = parser.run_str(input);
        assert_eq!(r, Ok(None as Option<char>));
    }
}

#[test]
fn parse_test_optional_and_seq() {
    #[derive(Clone, Debug, PartialEq, Eq)]
    enum Animal {
        Cat,
        Dog,
    }
    let parser: Parser<String, char, (Option<Animal>, Animal)> = Parser::match_string("cat")
        .map_to(Animal::Cat)
        .optional()
        .seq2(&Parser::match_string("dog").map_to(Animal::Dog));
    {
        let input = "catdog";
        let r = parser.run_str(input);
        assert_eq!(r, Ok((Some(Animal::Cat), Animal::Dog)));
    }
    {
        let input = "dog";
        let r = parser.run_str(input);
        assert_eq!(r, Ok((None, Animal::Dog)));
    }
}

#[test]
fn parse_test_seq_backtrack() {
    let parser = match_digit()
        .seq2(
            &Parser::unordered_choice(vec![
                match_digit().map_to(()),
                Parser::match_('_').map_to(()),
            ])
            .optional(),
        )
        .seq2(&match_digit())
        .seq2(&Parser::eof())
        .return_string();
    let input = "10";
    let r = parser.run_str(input);
    println!("{:?}", r);
}

#[cfg(test)]
fn match_digit() -> Parser<String, char, u8> {
    Parser::unordered_choice(vec![
        Parser::match_('0').map_to(0u8),
        Parser::match_('1').map_to(1u8),
        Parser::match_('2').map_to(2u8),
        Parser::match_('3').map_to(3u8),
        Parser::match_('4').map_to(4u8),
        Parser::match_('5').map_to(5u8),
        Parser::match_('6').map_to(6u8),
        Parser::match_('7').map_to(7u8),
        Parser::match_('8').map_to(8u8),
        Parser::match_('9').map_to(9u8),
    ])
}

#[cfg(test)]
fn match_int() -> Parser<String, char, usize> {
    match_digit().one_or_more_vec().map(|v| {
        let mut s = 0;
        for a in v {
            s *= 10;
            s += a as usize;
        }
        return s;
    })
}
