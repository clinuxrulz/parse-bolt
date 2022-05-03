#![recursion_limit = "1024"]

pub mod adt;
pub mod kotlin;
mod parser;
pub mod python;
mod test;

pub use self::parser::Parser;

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
