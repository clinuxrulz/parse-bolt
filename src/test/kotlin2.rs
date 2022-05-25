use crate::kotlin::parser2::KotlinParser;
use crate::kotlin::source::Source;
use crate::kotlin::source_cursor::SourceCursor;
use crate::kotlin::token::{Token, KTokenClass};
use crate::kotlin::token_stream::TokenStream;
use crate::parser3::Parser;

fn run_parser<A: std::fmt::Debug + 'static>(parser: &Parser<String, Token, KTokenClass, A>, code: &str) -> Result<A, String> {
    let mut runner = parser.compile(&KTokenClass::EOF);
    println!("runner: {:#?}", runner);
    let source = Source::from_str(code);
    let source_cursor = SourceCursor::new(source);
    let mut token_stream = TokenStream::new(source_cursor);
    loop {
        match token_stream.next() {
            Ok((token, _byte_span)) => {
                let was_eof = token == Token::EOF;
                let _in_done_state = runner.advance(token)?;
                if was_eof {
                    break;
                }
            },
            Err(err) => {
                return Err(format!("{:?}", err));
            }
        }
    }
    if runner.is_finished() {
        return Ok(runner.get_result());
    } else {
        return Err("more tokens expected.".to_owned());
    }
}

#[test]
fn test_simple() {
    let r = run_parser(
        &Parser::match_(KTokenClass::Id)
            .seq2(&Parser::seq_right(&Parser::match_(KTokenClass::Dot), &Parser::match_(KTokenClass::Id))),
            "test1.test2"
        );
    println!("{:?}", r);
}

#[test]
fn test_kotlin2_identifier() {
    let parser = KotlinParser::new();
    let r = run_parser(&parser.identifier, "test1.test2");
    println!("{:?}", r);
}

#[test]
fn test_kotlin2_import_header() {
    let parser = KotlinParser::new();
    let r = run_parser(&parser.import_header, "import java.lang.String as JString;");
    println!("{:?}", r);
}
