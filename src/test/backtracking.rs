#[cfg(test)]
use super::super::{choice_lazy, Parser};

#[test]
fn backtracing_parser_test_backtracking() {
    fn parser() -> Parser<String, char, (char, char)> {
        Parser::unordered_choice(vec![
            Parser::match_('a').map_to('a'),
            Parser::match_('b').map_to('b'),
            Parser::empty().map_to(' '),
        ])
        .seq2(&Parser::match_('a').map_to('a'))
        .seq_left(&Parser::eof())
    }
    let parser = parser();
    {
        let input: Vec<char> = "a".chars().collect();
        let result = parser.run(&input, input.len(), 0);
        assert_eq!(result, Ok(((' ', 'a'), 1)));
    }
    {
        let input: Vec<char> = "aa".chars().collect();
        let result = parser.run(&input, input.len(), 0);
        assert_eq!(result, Ok((('a', 'a'), 2)));
    }
    {
        let input: Vec<char> = "ba".chars().collect();
        let result = parser.run(&input, input.len(), 0);
        assert_eq!(result, Ok((('b', 'a'), 2)));
    }
}

/*
#[test]
fn backtracing_parser_test_resume_next_rule() {
    fn parser() -> Parser<String, char, char> {
        or_elses_lazy!(
            Parser::match_('a').map_to('a'),
            Parser::match_('b').map_to('b'),
            Parser::match_('a').seq2(&Parser::match_('b')).map_to('c'),
        )
    }
    let parser = parser();
    let input: Vec<char> = "ab".chars().collect();
    let result = parser.start(&input, input.len(), 0);
    assert!(result.is_ok());
    let result = result.ok().unwrap();
    assert_eq!((result.0, result.1), ('a', 1));
    let resume_op = result.2;
    assert!(resume_op.is_some());
    let mut resume = resume_op.unwrap();
    let result = resume.resume(&input, input.len(), 0, "test error".to_owned());
    assert_eq!(result.map(|x| (x.0, x.1)), Ok(('c', 2)));
}*/

#[test]
fn backtracking_parser_test_zero_or_more() {
    fn parser() -> Parser<String, char, Vec<char>> {
        choice_lazy!(
            Parser::match_('a').map_to('a'),
            Parser::match_('b').map_to('b'),
            Parser::match_('c').map_to('c')
        )
        .zero_or_more_vec()
        .seq_left(&Parser::eof())
    }
    let parser = parser();
    let input: Vec<char> = "abc".chars().collect();
    let result = parser.run(&input, input.len(), 0);
    assert_eq!(result, Ok((vec!['a', 'b', 'c'], 3)));
}

/*
#[test]
fn backtracking_parser_test_enumerate() {
    fn parser() -> Parser<String, char, (u8, u8)> {
        Parser::or_elses(vec![
            Parser::empty().map_to(0),
            Parser::empty().map_to(1),
            Parser::empty().map_to(2),
        ])
        .seq2(&Parser::or_elses(vec![
            Parser::empty().map_to(0),
            Parser::empty().map_to(1),
            Parser::empty().map_to(2),
        ]))
    }
    let parser = parser();
    let input: Vec<char> = "".chars().collect();
    let mut d0 = 0;
    let mut d1 = 0;
    let mut result = parser.start(&input, input.len(), 0);
    loop {
        assert!(result.is_ok());
        let result2 = result.ok().unwrap();
        assert_eq!((result2.0, result2.1), ((d1, d0), 0));
        let resume_op = result2.2;
        let again = resume_op.is_some();
        if !again {
            break;
        }
        let mut resume = resume_op.unwrap();
        result = resume.resume(&input, input.len(), 0, "test error".to_owned());
        d0 += 1;
        if d0 == 3 {
            d0 = 0;
            d1 += 1;
        }
    }
    assert_eq!(d0, 2);
    assert_eq!(d1, 2);
}
*/

#[test]
fn backtracking_parser_test_digits_with_separators() {
    fn parser() -> Parser<String, char, String> {
        choice_lazy!(
            Parser::satisfy(|x| '0' <= *x && *x <= '9')
                .seq2(&Parser::satisfy(|x| '0' <= *x && *x <= '9' || *x == '_').zero_or_more_vec_unordered_choice())
                .seq2(&Parser::satisfy(|x| '0' <= *x && *x <= '9'))
                .map_to(()),
            Parser::satisfy(|x| '0' <= *x && *x <= '9').map_to(()),
        )
        .seq2(&Parser::eof())
        .return_string()
    }
    let parser = parser();
    let input: Vec<char> = "1_000_000".chars().collect();
    let result = parser.run(&input, input.len(), 0);
    assert_eq!(result, Ok(("1_000_000".to_owned(), 9)));
}
