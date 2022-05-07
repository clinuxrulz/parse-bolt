#[cfg(test)]
use crate::kotlin;
#[cfg(test)]
use crate::Parser;

#[test]
fn test_kotlin_import_header() {
    let parser = kotlin::parser::import_header().seq_left(&Parser::eof());
    let input = "import java.lang.String as JString;";
    let r = parser.run_str(input);
    assert!(r.is_ok());
    println!("{:?}", r);
}

#[test]
fn test_kotlin_identifier() {
    let parser = kotlin::parser::identifier().seq_left(&Parser::eof());
    let input = "test1.test2.test3";
    let r = parser.run_str(input);
    assert_eq!(
        r,
        Ok(kotlin::data::Identifier {
            parts: vec!["test1".to_owned(), "test2".to_owned(), "test3".to_owned()]
        })
    );
}

#[test]
fn test_kotlin_function_type_parameters() {
    let parser = kotlin::parser::function_type_parameters().seq2(&Parser::eof());
    let input = "(a: String, b: String)";
    let t = std::time::Instant::now();
    let r = parser.run_str(input);
    println!("took {:?}ms", t.elapsed().as_millis());
    println!("{:?}", r);
    assert!(r.is_ok());
}

#[test]
fn test_kotlin_function_type() {
    let parser = kotlin::parser::function_type().seq2(&Parser::eof());
    let input = "(String, String) -> String";
    let r = parser.run_str(input);
    println!("{:?}", r);
    assert!(r.is_ok());
}

#[test]
fn test_kotlin_additive_expression() {
    let parser = kotlin::parser::additive_expression().seq_left(&Parser::eof());
    let input = "1 - 2 + 3 + 4 - 7 + 1 - 1 + 2 + 2";
    let t = std::time::Instant::now();
    let r = parser.run_str(input);
    println!("took {:?}ms", t.elapsed().as_millis());
    println!("{:?}", r);
    assert!(r.is_ok());
}
