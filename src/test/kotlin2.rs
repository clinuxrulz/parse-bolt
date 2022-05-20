use crate::kotlin::parser2::KotlinParser;
use crate::kotlin::source::Source;
use crate::kotlin::source_cursor::SourceCursor;
use crate::kotlin::token::Token;
use crate::kotlin::token_stream::TokenStream;

#[test]
fn test_kotlin2_import_header() {
    let parser = KotlinParser::new();
    let mut runner = parser.import_header.compile();
    let source = Source::from_str("import java.lang.String as JString;");
    let source_cursor = SourceCursor::new(source);
    let mut token_stream = TokenStream::new(source_cursor);
    loop {
        match token_stream.next() {
            Ok((token, byte_span)) => {
                match token {
                    Token::EOF => {
                        let _ = runner.advance(None);
                        break;
                    }
                    _ => {
                        let _ = runner.advance(Some(token));
                    }
                }
            },
            Err(err) => {
                println!("{:?}", err);
                break;
            }
        }
    }
    if runner.is_finished() {
        println!("{:?}", runner.get_result());
    } else {
        println!("more tokens expected.");
    }
}
