// https://kotlinlang.org/docs/reference/grammar.html
use super::super::choice_lazy;
use super::super::kotlin;
use super::super::parser3::Parser;
use super::data;
use super::token::{KTokenClass, Token};

pub fn simple_identifier() -> Parser<String, Token, KTokenClass, String> {
    Parser::choice(vec![
        &Parser::match_(KTokenClass::Id).map(|x| match x { Token::Id(x2) => x2, _ => unreachable!() }),
        //&Parser::match_(KTokenClass::)
        //&Parser::match_(KTokenClass::)
    ])
    //todo!();
    /*
    choice_lazy!(
        kotlin::lexer::identifier().map_to(()),
        kotlin::lexer::abstract_(),
        kotlin::lexer::annotation(),
        kotlin::lexer::by(),
        kotlin::lexer::catch(),
        kotlin::lexer::companion(),
        kotlin::lexer::constructor(),
        kotlin::lexer::crossinline(),
        kotlin::lexer::data(),
        kotlin::lexer::dynamic(),
        kotlin::lexer::enum_(),
        kotlin::lexer::external(),
        kotlin::lexer::final_(),
        kotlin::lexer::finally(),
        kotlin::lexer::get(),
        kotlin::lexer::import(),
        kotlin::lexer::infix(),
        kotlin::lexer::init(),
        kotlin::lexer::inline(),
        kotlin::lexer::inner(),
        kotlin::lexer::internal(),
        kotlin::lexer::lateinit(),
        kotlin::lexer::noinline(),
        kotlin::lexer::open(),
        kotlin::lexer::operator(),
        kotlin::lexer::out(),
        kotlin::lexer::override_(),
        kotlin::lexer::private(),
        kotlin::lexer::protected(),
        kotlin::lexer::public(),
        kotlin::lexer::reified(),
        kotlin::lexer::sealed(),
        kotlin::lexer::tailrec(),
        kotlin::lexer::set(),
        kotlin::lexer::vararg(),
        kotlin::lexer::where_(),
        kotlin::lexer::field(),
        kotlin::lexer::property(),
        kotlin::lexer::receiver(),
        kotlin::lexer::param(),
        kotlin::lexer::setparam(),
        kotlin::lexer::delegate(),
        kotlin::lexer::file(),
        kotlin::lexer::expect(),
        kotlin::lexer::actual(),
        kotlin::lexer::const_(),
        kotlin::lexer::suspend(),
        kotlin::lexer::value(),
    )
    .return_string()*/
}
