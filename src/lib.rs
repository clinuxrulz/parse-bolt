#![recursion_limit = "1024"]

pub mod adt;
pub mod kotlin;
mod parser;
mod parser2;
mod parser_byte_code;
pub mod python;
mod test;

pub use self::parser::Parser;
pub use self::parser::Parser2 as ExperimentalParser;
pub use self::parser::TokenStream;

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
