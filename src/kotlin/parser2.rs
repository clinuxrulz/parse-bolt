// https://kotlinlang.org/docs/reference/grammar.html
use super::super::parser3::Parser;
use super::data;
use super::token::{KTokenClass, Token};

pub struct KotlinParser {
    pub simple_identifier: Parser<String, Token, KTokenClass, String>,
    pub identifier: Parser<String, Token, KTokenClass, data::Identifier>,
    pub import_alias: Parser<String, Token, KTokenClass, data::ImportAlias>,
    pub import_header: Parser<String, Token, KTokenClass, data::ImportHeader>,
}

impl KotlinParser {
    pub fn new() -> KotlinParser {
        //
        let simple_identifier: Parser<String, Token, KTokenClass, String> =
            Parser::choice(vec![
                &Parser::match_(KTokenClass::Id).map(|x| match x { Token::Id(x2) => x2, _ => unreachable!() }),
                &Parser::match_(KTokenClass::Abstract).map(|_| "abstract".to_owned()),
                &Parser::match_(KTokenClass::Annotation).map(|_| "annotation".to_owned()),
                &Parser::match_(KTokenClass::By).map(|_| "by".to_owned()),
                &Parser::match_(KTokenClass::Catch).map(|_| "catch".to_owned()),
                &Parser::match_(KTokenClass::Companion).map(|_| "companion".to_owned()),
                &Parser::match_(KTokenClass::Constructor).map(|_| "constructor".to_owned()),
                &Parser::match_(KTokenClass::Crossinline).map(|_| "crossinline".to_owned()),
                &Parser::match_(KTokenClass::Data).map(|_| "data".to_owned()),
                &Parser::match_(KTokenClass::Dynamic).map(|_| "dynamic".to_owned()),
                &Parser::match_(KTokenClass::Enum).map(|_| "enum".to_owned()),
                &Parser::match_(KTokenClass::External).map(|_| "external".to_owned()),
                &Parser::match_(KTokenClass::Final).map(|_| "final".to_owned()),
                &Parser::match_(KTokenClass::Finally).map(|_| "finally".to_owned()),
                &Parser::match_(KTokenClass::Get).map(|_| "get".to_owned()),
                &Parser::match_(KTokenClass::Import).map(|_| "import".to_owned()),
                &Parser::match_(KTokenClass::Infix).map(|_| "infix".to_owned()),
                &Parser::match_(KTokenClass::Init).map(|_| "init".to_owned()),
                &Parser::match_(KTokenClass::Inline).map(|_| "inline".to_owned()),
                &Parser::match_(KTokenClass::Inner).map(|_| "inner".to_owned()),
                &Parser::match_(KTokenClass::Internal).map(|_| "internal".to_owned()),
                &Parser::match_(KTokenClass::Lateinit).map(|_| "lateinit".to_owned()),
                &Parser::match_(KTokenClass::Noinline).map(|_| "noinline".to_owned()),
                &Parser::match_(KTokenClass::Open).map(|_| "open".to_owned()),
                &Parser::match_(KTokenClass::Operator).map(|_| "operator".to_owned()),
                &Parser::match_(KTokenClass::Out).map(|_| "out".to_owned()),
                &Parser::match_(KTokenClass::Override).map(|_| "override".to_owned()),
                &Parser::match_(KTokenClass::Private).map(|_| "private".to_owned()),
                &Parser::match_(KTokenClass::Protected).map(|_| "protected".to_owned()),
                &Parser::match_(KTokenClass::Public).map(|_| "public".to_owned()),
                &Parser::match_(KTokenClass::Reified).map(|_| "reified".to_owned()),
                &Parser::match_(KTokenClass::Sealed).map(|_| "sealed".to_owned()),
                &Parser::match_(KTokenClass::Tailrec).map(|_| "tailrec".to_owned()),
                &Parser::match_(KTokenClass::Set).map(|_| "set".to_owned()),
                &Parser::match_(KTokenClass::Vararg).map(|_| "vararg".to_owned()),
                &Parser::match_(KTokenClass::Where).map(|_| "where".to_owned()),
                &Parser::match_(KTokenClass::Field).map(|_| "field".to_owned()),
                &Parser::match_(KTokenClass::Property).map(|_| "property".to_owned()),
                &Parser::match_(KTokenClass::Receiver).map(|_| "receiver".to_owned()),
                &Parser::match_(KTokenClass::Param).map(|_| "param".to_owned()),
                &Parser::match_(KTokenClass::Setparam).map(|_| "setparam".to_owned()),
                &Parser::match_(KTokenClass::Delegate).map(|_| "delegate".to_owned()),
                &Parser::match_(KTokenClass::File).map(|_| "file".to_owned()),
                &Parser::match_(KTokenClass::Expect).map(|_| "expect".to_owned()),
                &Parser::match_(KTokenClass::Actual).map(|_| "actual".to_owned()),
                &Parser::match_(KTokenClass::Const).map(|_| "const".to_owned()),
                &Parser::match_(KTokenClass::Suspend).map(|_| "suspend".to_owned()),
                &Parser::match_(KTokenClass::Value).map(|_| "value".to_owned()),
            ]);
        //
        let identifier: Parser<String, Token, KTokenClass, data::Identifier> =
            simple_identifier
                .many1_sep(&Parser::match_(KTokenClass::Dot))
                .map(|parts| data::Identifier { parts });
        //
        let import_alias: Parser<String, Token, KTokenClass, data::ImportAlias> =
            Parser::match_(KTokenClass::As)
                .seq_right(&simple_identifier)
                .map(|simple_identifier| data::ImportAlias { simple_identifier, });
        let import_header: Parser<String, Token, KTokenClass, data::ImportHeader> =
            Parser::match_(KTokenClass::Import)
                .seq_right(&identifier)
                .seq2(
                    &Parser::choice(vec![
                        &Parser::match_(KTokenClass::Dot)
                            .seq2(&Parser::match_(KTokenClass::Asterisk))
                            .map_to(data::ImportHeader2::DotMult),
                        &import_alias.map(data::ImportHeader2::ImportAlias)
                    ])
                    .optional()
                )
                .seq_left(
                    &Parser::match_(KTokenClass::Semicolon).optional()
                )
                .map(|(identifier, import_header_op)| {
                    data::ImportHeader {
                        identifier,
                        import_header_op
                    }
                });
        //
        KotlinParser {
            simple_identifier,
            identifier,
            import_alias,
            import_header,
        }
    }
}
