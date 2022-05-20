// https://github.com/cout970/kotlin-interpreter-rs

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Id(String),
    Number(Number),
    Char(char),
    StringStart,
    StringEnd,
    StringTemplateStart,
    StringTemplateEnd,
    StringContent(String),
    StringVariable(String),
    // Signs
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    LeftAngleBracket,
    RightAngleBracket,
    LessEquals,
    GreaterEquals,
    At,
    Colon,
    DoubleColon,
    Dollar,
    Dot,
    DoubleDot,
    Comma,
    QuestionMark,
    ExclamationMark,
    DoubleExclamationMark,
    NotEquals,
    NotDoubleEquals,
    RightArrow,
    Elvis,
    Plus,
    DoublePlus,
    Minus,
    DoubleMinus,
    Asterisk,
    Slash,
    Percent,
    Equals,
    DoubleEquals,
    TripleEquals,
    PlusEquals,
    MinusEquals,
    TimesEquals,
    DivEquals,
    ModEquals,
    Ampersand,
    DoubleAmpersand,
    Pipe,
    DoublePipe,
    Underscore,
    // Keywords
    As,
    AsQuestionMark,
    Break,
    Class,
    Continue,
    Do,
    Else,
    False,
    For,
    Fun,
    If,
    In,
    NotIn,
    Is,
    NotIs,
    Interface,
    Null,
    Object,
    Package,
    Return,
    Super,
    This,
    Throw,
    True,
    Try,
    TypeAlias,
    Val,
    Var,
    When,
    While,
    // End of file
    EOF,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Number {
    Double(f64),
    Float(f32),
    Byte(i8),
    Short(i16),
    Int(i32),
    Long(i64),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum KTokenClass {
    Id,
    Number,
    Char,
    StringStart,
    StringEnd,
    StringTemplateStart,
    StringTemplateEnd,
    StringContent,
    StringVariable,
    // Signs
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    LeftAngleBracket,
    RightAngleBracket,
    LessEquals,
    GreaterEquals,
    At,
    Colon,
    DoubleColon,
    Dollar,
    Dot,
    DoubleDot,
    Comma,
    QuestionMark,
    ExclamationMark,
    DoubleExclamationMark,
    NotEquals,
    NotDoubleEquals,
    RightArrow,
    Elvis,
    Plus,
    DoublePlus,
    Minus,
    DoubleMinus,
    Asterisk,
    Slash,
    Percent,
    Equals,
    DoubleEquals,
    TripleEquals,
    PlusEquals,
    MinusEquals,
    TimesEquals,
    DivEquals,
    ModEquals,
    Ampersand,
    DoubleAmpersand,
    Pipe,
    DoublePipe,
    Underscore,
    // Keywords
    As,
    AsQuestionMark,
    Break,
    Class,
    Continue,
    Do,
    Else,
    False,
    For,
    Fun,
    If,
    In,
    NotIn,
    Is,
    NotIs,
    Interface,
    Null,
    Object,
    Package,
    Return,
    Super,
    This,
    Throw,
    True,
    Try,
    TypeAlias,
    Val,
    Var,
    When,
    While,
    // End of file
    EOF,
}

impl crate::parser3::TokenClass for Token {
    type Result = KTokenClass;

    fn token_class(&self) -> Self::Result {
        match self {
            Token::Id(_) => KTokenClass::Id,
            Token::Number(_) => KTokenClass::Number,
            Token::Char(_) => KTokenClass::Char,
            Token::StringStart => KTokenClass::StringStart,
            Token::StringEnd => KTokenClass::StringEnd,
            Token::StringTemplateStart => KTokenClass::StringTemplateStart,
            Token::StringTemplateEnd => KTokenClass::StringTemplateEnd,
            Token::StringContent(_) => KTokenClass::StringContent,
            Token::StringVariable(_) => KTokenClass::StringVariable,
            Token::Semicolon => KTokenClass::Semicolon,
            Token::LeftParen => KTokenClass::LeftParen,
            Token::RightParen => KTokenClass::RightParen,
            Token::LeftBrace => KTokenClass::LeftBrace,
            Token::RightBrace => KTokenClass::RightBrace,
            Token::LeftBracket => KTokenClass::LeftBracket,
            Token::RightBracket => KTokenClass::RightBracket,
            Token::LeftAngleBracket => KTokenClass::LeftAngleBracket,
            Token::RightAngleBracket => KTokenClass::RightAngleBracket,
            Token::LessEquals => KTokenClass::LessEquals,
            Token::GreaterEquals => KTokenClass::GreaterEquals,
            Token::At => KTokenClass::At,
            Token::Colon => KTokenClass::Colon,
            Token::DoubleColon => KTokenClass::DoubleColon,
            Token::Dollar => KTokenClass::Dollar,
            Token::Dot => KTokenClass::Dot,
            Token::DoubleDot => KTokenClass::DoubleDot,
            Token::Comma => KTokenClass::Comma,
            Token::QuestionMark => KTokenClass::QuestionMark,
            Token::ExclamationMark => KTokenClass::ExclamationMark,
            Token::DoubleExclamationMark => KTokenClass::DoubleExclamationMark,
            Token::NotEquals => KTokenClass::NotEquals,
            Token::NotDoubleEquals => KTokenClass::NotDoubleEquals,
            Token::RightArrow => KTokenClass::RightArrow,
            Token::Elvis => KTokenClass::Elvis,
            Token::Plus => KTokenClass::Plus,
            Token::DoublePlus => KTokenClass::DoublePlus,
            Token::Minus => KTokenClass::Minus,
            Token::DoubleMinus => KTokenClass::DoubleMinus,
            Token::Asterisk => KTokenClass::Asterisk,
            Token::Slash => KTokenClass::Slash,
            Token::Percent => KTokenClass::Percent,
            Token::Equals => KTokenClass::Equals,
            Token::DoubleEquals => KTokenClass::DoubleEquals,
            Token::TripleEquals => KTokenClass::TripleEquals,
            Token::PlusEquals => KTokenClass::PlusEquals,
            Token::MinusEquals => KTokenClass::MinusEquals,
            Token::TimesEquals => KTokenClass::TimesEquals,
            Token::DivEquals => KTokenClass::DivEquals,
            Token::ModEquals => KTokenClass::ModEquals,
            Token::Ampersand => KTokenClass::Ampersand,
            Token::DoubleAmpersand => KTokenClass::DoubleAmpersand,
            Token::Pipe => KTokenClass::Pipe,
            Token::DoublePipe => KTokenClass::DoublePipe,
            Token::Underscore => KTokenClass::Underscore,
            Token::As => KTokenClass::As,
            Token::AsQuestionMark => KTokenClass::AsQuestionMark,
            Token::Break => KTokenClass::Break,
            Token::Class => KTokenClass::Class,
            Token::Continue => KTokenClass::Continue,
            Token::Do => KTokenClass::Do,
            Token::Else => KTokenClass::Else,
            Token::False => KTokenClass::False,
            Token::For => KTokenClass::For,
            Token::Fun => KTokenClass::Fun,
            Token::If => KTokenClass::If,
            Token::In => KTokenClass::In,
            Token::NotIn => KTokenClass::NotIn,
            Token::Is => KTokenClass::Is,
            Token::NotIs => KTokenClass::NotIs,
            Token::Interface => KTokenClass::Interface,
            Token::Null => KTokenClass::Null,
            Token::Object => KTokenClass::Object,
            Token::Package => KTokenClass::Package,
            Token::Return => KTokenClass::Return,
            Token::Super => KTokenClass::Super,
            Token::This => KTokenClass::This,
            Token::Throw => KTokenClass::Throw,
            Token::True => KTokenClass::True,
            Token::Try => KTokenClass::Try,
            Token::TypeAlias => KTokenClass::TypeAlias,
            Token::Val => KTokenClass::Val,
            Token::Var => KTokenClass::Var,
            Token::When => KTokenClass::When,
            Token::While => KTokenClass::While,
            Token::EOF => KTokenClass::EOF,
        }
    }
}
