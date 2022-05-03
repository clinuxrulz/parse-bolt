#[cfg(test)]
use super::super::Parser;

#[test]
fn iteratee_parser2_test_digits_with_separators() {
    fn parser() -> Parser<String, char, String> {
        Parser::unordered_choice(vec![
            Parser::satisfy(|x| '0' <= *x && *x <= '9')
                .seq2(&Parser::satisfy(|x| '0' <= *x && *x <= '9' || *x == '_').zero_or_more_vec())
                .seq2(&Parser::satisfy(|x| '0' <= *x && *x <= '9'))
                .map_to(()),
            Parser::satisfy(|x| '0' <= *x && *x <= '9').map_to(()),
        ])
        .seq2(&Parser::eof())
        .return_string()
    }
    let parser = parser();
    let input = "1_000_000";
    let result = parser.run_str(input);
    println!("{:?}", result);
}
