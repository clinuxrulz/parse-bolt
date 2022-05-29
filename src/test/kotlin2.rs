use crate::kotlin::parser2::KotlinParser;
use crate::kotlin::source::Source;
use crate::kotlin::source_cursor::SourceCursor;
use crate::kotlin::token::{Token, KTokenClass};
use crate::kotlin::token_stream::TokenStream;
use crate::parser3::{Parser, TokenClass};

fn run_parser<A: std::fmt::Debug + 'static>(parser: &Parser<String, Token, KTokenClass, A>, code: &str) -> Result<A, String> {
    let mut runner = parser.compile(&KTokenClass::EOF);
    if !runner.check_table() {
        //return Err("Problem in parsing table.".to_owned());
    }
    let source = Source::from_str(code);
    let source_cursor = SourceCursor::new(source);
    let mut token_stream = TokenStream::new(source_cursor);
    let start_time = std::time::Instant::now();
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
    // TODO: FIXME: End of file token currently needs to be sent twice. Once for consumed, and next for lookahead for final rule match.
    runner.advance(Token::EOF)?;
    //
    println!("Actual parsing took: {}ms", start_time.elapsed().as_millis());
    if runner.is_finished() {
        return Ok(runner.get_result());
    } else {
        return Err("more tokens expected.".to_owned());
    }
}

fn run_parser_2(grammar: &Vec<crate::lr1_parser::Rule<crate::parser3::RuleOrToken<KTokenClass>>>, code: &str) -> Result<(), String> {
    let mut parser = crate::lr1_parser::Lr1Parser::from_grammar(grammar, &crate::parser3::RuleOrToken::Token(KTokenClass::EOF));
    if !parser.check_table() {
        //return Err("Problem in parsing table.".to_owned());
    }
    let source = Source::from_str(code);
    let source_cursor = SourceCursor::new(source);
    let mut token_stream = TokenStream::new(source_cursor);
    let start_time = std::time::Instant::now();
    loop {
        match token_stream.next() {
            Ok((token, _byte_span)) => {
                let was_eof = token == Token::EOF;
                let _in_done_state = parser.advance(&crate::parser3::RuleOrToken::Token(token.token_class()), None)?;
                if was_eof {
                    break;
                }
            },
            Err(err) => {
                return Err(format!("{:?}", err));
            }
        }
    }
    // TODO: FIXME: End of file token currently needs to be sent twice. Once for consumed, and next for lookahead for final rule match.
    parser.advance(&crate::parser3::RuleOrToken::Token(Token::EOF.token_class()), None)?;
    //
    println!("Actual parsing took: {}ms", start_time.elapsed().as_millis());
    return Ok(());
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

#[test]
fn test_kotlin2_import_header2() {
    let grammar = vec![
        crate::lr1_parser::Rule {
            name_op: None,
            parts: vec![crate::parser3::RuleOrToken::Rule(0), crate::parser3::RuleOrToken::Token(KTokenClass::EOF)],
            effect_op: None,
        },
        crate::lr1_parser::Rule {
            name_op: Some(crate::parser3::RuleOrToken::Rule(0)),
            parts: vec![
                crate::parser3::RuleOrToken::Token(KTokenClass::Import),
                crate::parser3::RuleOrToken::Rule(1),
                crate::parser3::RuleOrToken::Token(KTokenClass::Semicolon),
            ],
            effect_op: None,
        },
        crate::lr1_parser::Rule {
            name_op: Some(crate::parser3::RuleOrToken::Rule(1)),
            parts: vec![
                crate::parser3::RuleOrToken::Rule(2),
                crate::parser3::RuleOrToken::Token(KTokenClass::Dot),
                crate::parser3::RuleOrToken::Token(KTokenClass::Asterisk),
            ],
            effect_op: None,
        },
        crate::lr1_parser::Rule {
            name_op: Some(crate::parser3::RuleOrToken::Rule(1)),
            parts: vec![
                crate::parser3::RuleOrToken::Rule(2),
                crate::parser3::RuleOrToken::Token(KTokenClass::As),
                crate::parser3::RuleOrToken::Token(KTokenClass::Id),
            ],
            effect_op: None,
        },
        crate::lr1_parser::Rule {
            name_op: Some(crate::parser3::RuleOrToken::Rule(1)),
            parts: vec![
                crate::parser3::RuleOrToken::Rule(2),
            ],
            effect_op: None,
        },
        crate::lr1_parser::Rule {
            name_op: Some(crate::parser3::RuleOrToken::Rule(2)),
            parts: vec![
                crate::parser3::RuleOrToken::Rule(2),
                crate::parser3::RuleOrToken::Token(KTokenClass::Dot),
                crate::parser3::RuleOrToken::Token(KTokenClass::Id),
            ],
            effect_op: None,
        },
        crate::lr1_parser::Rule {
            name_op: Some(crate::parser3::RuleOrToken::Rule(2)),
            parts: vec![
                crate::parser3::RuleOrToken::Token(KTokenClass::Id),
            ],
            effect_op: None,
        },
    ];
    let r = run_parser_2(&grammar, "import java.lang.String as JString;");
    if let Err(err) = r {
        println!("{}", err);
    }
}
