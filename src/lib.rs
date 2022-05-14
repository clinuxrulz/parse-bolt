#![recursion_limit = "1024"]

pub mod adt;
pub mod kotlin;
pub mod lr_parser;
//mod parser;
mod parser2;
pub mod parser3;
//mod parser_byte_code;
pub mod python;
mod test;
mod token_stream;

pub use self::parser2::Parser;
//pub use self::parser::Parser;
//pub use self::parser::Parser2 as ExperimentalParser;
//pub use self::parser::TokenStream;
pub use self::token_stream::Pos;
pub use self::token_stream::TokenStream;

#[macro_export]
macro_rules! choice_lazy {
    ( $( $x:expr ),* $(,)? ) => {
        {
            let mut parsers = Vec::new();
            $(
                parsers.push(crate::Parser::lazy(|| $x));
            )*
            crate::Parser::choice(parsers)
        }
    };
}

#[macro_export]
macro_rules! unordered_choice_lazy {
    ( $( $x:expr ),* $(,)? ) => {
        {
            let mut parsers = Vec::new();
            $(
                parsers.push(crate::Parser::lazy(|| $x));
            )*
            crate::Parser::unordered_choice(parsers)
        }
    };
}
