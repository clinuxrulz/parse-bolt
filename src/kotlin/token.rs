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
    Abstract,
    Actual,
    Annotation,
    As,
    AsQuestionMark,
    Break,
    By,
    Catch,
    Class,
    Companion,
    Constructor,
    Continue,
    Const,
    Crossinline,
    Data,
    Delegate,
    Dynamic,
    Do,
    Else,
    Enum,
    External,
    Expect,
    False,
    Field,
    File,
    Final,
    Finally,
    For,
    Fun,
    Get,
    If,
    Import,
    In,
    Infix,
    Init,
    Inline,
    Inner,
    Internal,
    Lateinit,
    Noinline,
    NotIn,
    Is,
    NotIs,
    Interface,
    Null,
    Object,
    Open,
    Operator,
    Out,
    Override,
    Package,
    Param,
    Private,
    Protected,
    Property,
    Public,
    Receiver,
    Reified,
    Return,
    Sealed,
    Set,
    Setparam,
    Super,
    Suspend,
    Tailrec,
    This,
    Throw,
    True,
    Try,
    TypeAlias,
    Val,
    Value,
    Var,
    Vararg,
    When,
    Where,
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
    Abstract,
    Actual,
    Annotation,
    As,
    AsQuestionMark,
    Break,
    By,
    Catch,
    Class,
    Companion,
    Constructor,
    Continue,
    Const,
    Crossinline,
    Data,
    Delegate,
    Dynamic,
    Do,
    Else,
    Enum,
    External,
    Expect,
    False,
    Field,
    File,
    Final,
    Finally,
    For,
    Fun,
    Get,
    If,
    Import,
    In,
    Infix,
    Init,
    Inline,
    Inner,
    Internal,
    Lateinit,
    Noinline,
    NotIn,
    Is,
    NotIs,
    Interface,
    Null,
    Object,
    Open,
    Operator,
    Out,
    Override,
    Package,
    Param,
    Private,
    Protected,
    Property,
    Public,
    Receiver,
    Reified,
    Return,
    Sealed,
    Set,
    Setparam,
    Super,
    Suspend,
    Tailrec,
    This,
    Throw,
    True,
    Try,
    TypeAlias,
    Val,
    Value,
    Var,
    Vararg,
    When,
    Where,
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
            Token::Abstract => KTokenClass::Abstract,
            Token::Actual => KTokenClass::Actual,
            Token::Annotation => KTokenClass::Annotation,
            Token::As => KTokenClass::As,
            Token::AsQuestionMark => KTokenClass::AsQuestionMark,
            Token::Break => KTokenClass::Break,
            Token::By => KTokenClass::By,
            Token::Catch => KTokenClass::Catch,
            Token::Class => KTokenClass::Class,
            Token::Companion => KTokenClass::Companion,
            Token::Constructor => KTokenClass::Constructor,
            Token::Continue => KTokenClass::Continue,
            Token::Const => KTokenClass::Const,
            Token::Crossinline => KTokenClass::Crossinline,
            Token::Data => KTokenClass::Data,
            Token::Delegate => KTokenClass::Delegate,
            Token::Dynamic => KTokenClass::Dynamic,
            Token::Do => KTokenClass::Do,
            Token::Else => KTokenClass::Else,
            Token::Enum => KTokenClass::Enum,
            Token::External => KTokenClass::External,
            Token::Expect => KTokenClass::Expect,
            Token::False => KTokenClass::False,
            Token::Field => KTokenClass::Field,
            Token::File => KTokenClass::File,
            Token::Final => KTokenClass::Final,
            Token::Finally => KTokenClass::Finally,
            Token::For => KTokenClass::For,
            Token::Fun => KTokenClass::Fun,
            Token::Get => KTokenClass::Get,
            Token::Import => KTokenClass::Import,
            Token::If => KTokenClass::If,
            Token::In => KTokenClass::In,
            Token::Infix => KTokenClass::Infix,
            Token::Init => KTokenClass::Init,
            Token::Inline => KTokenClass::Inline,
            Token::Inner => KTokenClass::Inner,
            Token::Internal => KTokenClass::Internal,
            Token::Lateinit => KTokenClass::Lateinit,
            Token::Noinline => KTokenClass::Noinline,
            Token::NotIn => KTokenClass::NotIn,
            Token::Is => KTokenClass::Is,
            Token::NotIs => KTokenClass::NotIs,
            Token::Interface => KTokenClass::Interface,
            Token::Null => KTokenClass::Null,
            Token::Object => KTokenClass::Object,
            Token::Open => KTokenClass::Open,
            Token::Operator => KTokenClass::Operator,
            Token::Out => KTokenClass::Out,
            Token::Override => KTokenClass::Override,
            Token::Package => KTokenClass::Package,
            Token::Param => KTokenClass::Param,
            Token::Private => KTokenClass::Private,
            Token::Protected => KTokenClass::Protected,
            Token::Property => KTokenClass::Property,
            Token::Public => KTokenClass::Public,
            Token::Receiver => KTokenClass::Receiver,
            Token::Reified => KTokenClass::Reified,
            Token::Return => KTokenClass::Return,
            Token::Sealed => KTokenClass::Sealed,
            Token::Set => KTokenClass::Set,
            Token::Setparam => KTokenClass::Setparam,
            Token::Super => KTokenClass::Super,
            Token::Suspend => KTokenClass::Suspend,
            Token::Tailrec => KTokenClass::Tailrec,
            Token::This => KTokenClass::This,
            Token::Throw => KTokenClass::Throw,
            Token::True => KTokenClass::True,
            Token::Try => KTokenClass::Try,
            Token::TypeAlias => KTokenClass::TypeAlias,
            Token::Val => KTokenClass::Val,
            Token::Value => KTokenClass::Value,
            Token::Var => KTokenClass::Var,
            Token::Vararg => KTokenClass::Vararg,
            Token::When => KTokenClass::When,
            Token::Where => KTokenClass::Where,
            Token::While => KTokenClass::While,
            Token::EOF => KTokenClass::EOF,
        }
    }
}

impl std::fmt::Display for KTokenClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str =
            match self {
                KTokenClass::Id => "Id",
                KTokenClass::Number => "Number",
                KTokenClass::Char => "Char",
                KTokenClass::StringStart => "StringStart",
                KTokenClass::StringEnd => "StringEnd",
                KTokenClass::StringTemplateStart => "StringTemplateStart",
                KTokenClass::StringTemplateEnd => "StringTemplateEnd",
                KTokenClass::StringContent => "StringContent",
                KTokenClass::StringVariable => "StringVariable",
                KTokenClass::Semicolon => "Semicolon",
                KTokenClass::LeftParen => "LeftParen",
                KTokenClass::RightParen => "RightParen",
                KTokenClass::LeftBrace => "LeftBrace",
                KTokenClass::RightBrace => "RightBrace",
                KTokenClass::LeftBracket => "LeftBracket",
                KTokenClass::RightBracket => "RightBracket",
                KTokenClass::LeftAngleBracket => "LeftAngleBracket",
                KTokenClass::RightAngleBracket => "RightAngleBracket",
                KTokenClass::LessEquals => "LessEquals",
                KTokenClass::GreaterEquals => "GreaterEquals",
                KTokenClass::At => "At",
                KTokenClass::Colon => "Colon",
                KTokenClass::DoubleColon => "DoubleColon",
                KTokenClass::Dollar => "Dollar",
                KTokenClass::Dot => "Dot",
                KTokenClass::DoubleDot => "DoubleDot",
                KTokenClass::Comma => "Comma",
                KTokenClass::QuestionMark => "QuestionMark",
                KTokenClass::ExclamationMark => "ExclamationMark",
                KTokenClass::DoubleExclamationMark => "DoubleExclamationMark",
                KTokenClass::NotEquals => "NotEquals",
                KTokenClass::NotDoubleEquals => "NotDoubleEquals",
                KTokenClass::RightArrow => "RightArrow",
                KTokenClass::Elvis => "Elvis",
                KTokenClass::Plus => "Plus",
                KTokenClass::DoublePlus => "DoublePlus",
                KTokenClass::Minus => "Minus",
                KTokenClass::DoubleMinus => "DoubleMinus",
                KTokenClass::Asterisk => "Asterisk",
                KTokenClass::Slash => "Slash",
                KTokenClass::Percent => "Percent",
                KTokenClass::Equals => "Equals",
                KTokenClass::DoubleEquals => "DoubleEquals",
                KTokenClass::TripleEquals => "TripleEquals",
                KTokenClass::PlusEquals => "PlusEquals",
                KTokenClass::MinusEquals => "MinusEquals",
                KTokenClass::TimesEquals => "TimesEquals",
                KTokenClass::DivEquals => "DivEquals",
                KTokenClass::ModEquals => "ModEquals",
                KTokenClass::Ampersand => "Ampersand",
                KTokenClass::DoubleAmpersand => "DoubleAmpersand",
                KTokenClass::Pipe => "Pipe",
                KTokenClass::DoublePipe => "DoublePipe",
                KTokenClass::Underscore => "Underscore",
                KTokenClass::Abstract => "Abstract",
                KTokenClass::Actual => "Actual",
                KTokenClass::Annotation => "Annotation",
                KTokenClass::As => "As",
                KTokenClass::AsQuestionMark => "AsQuestionMark",
                KTokenClass::Break => "Break",
                KTokenClass::By => "By",
                KTokenClass::Catch => "Catch",
                KTokenClass::Class => "Class",
                KTokenClass::Companion => "Companion",
                KTokenClass::Constructor => "Constructor",
                KTokenClass::Continue => "Continue",
                KTokenClass::Const => "Const",
                KTokenClass::Crossinline => "Crossinline",
                KTokenClass::Data => "Data",
                KTokenClass::Delegate => "Delegate",
                KTokenClass::Dynamic => "Dynamic",
                KTokenClass::Do => "Do",
                KTokenClass::Else => "Else",
                KTokenClass::Enum => "Enum",
                KTokenClass::External => "External",
                KTokenClass::Expect => "Expect",
                KTokenClass::False => "False",
                KTokenClass::Field => "Field",
                KTokenClass::File => "File",
                KTokenClass::Final => "Final",
                KTokenClass::Finally => "Finally",
                KTokenClass::For => "For",
                KTokenClass::Fun => "Fun",
                KTokenClass::Get => "Get",
                KTokenClass::If => "If",
                KTokenClass::Import => "Import",
                KTokenClass::In => "In",
                KTokenClass::Infix => "Infix",
                KTokenClass::Init => "Init",
                KTokenClass::Inline => "Inline",
                KTokenClass::Inner => "Inner",
                KTokenClass::Internal => "Internal",
                KTokenClass::Lateinit => "Lateinit",
                KTokenClass::Noinline => "Noinline",
                KTokenClass::NotIn => "NotIn",
                KTokenClass::Is => "Is",
                KTokenClass::NotIs => "NotIs",
                KTokenClass::Interface => "Interface",
                KTokenClass::Null => "Null",
                KTokenClass::Object => "Object",
                KTokenClass::Open => "Open",
                KTokenClass::Operator => "Operator",
                KTokenClass::Out => "Out",
                KTokenClass::Override => "Override",
                KTokenClass::Package => "Package",
                KTokenClass::Param => "Param",
                KTokenClass::Private => "Private",
                KTokenClass::Protected => "Protected",
                KTokenClass::Property => "Property",
                KTokenClass::Public => "Public",
                KTokenClass::Receiver => "Receiver",
                KTokenClass::Reified => "Reified",
                KTokenClass::Return => "Return",
                KTokenClass::Sealed => "Sealed",
                KTokenClass::Set => "Set",
                KTokenClass::Setparam => "Setparam",
                KTokenClass::Super => "Super",
                KTokenClass::Suspend => "Suspend",
                KTokenClass::Tailrec => "Tailrec",
                KTokenClass::This => "This",
                KTokenClass::Throw => "Throw",
                KTokenClass::True => "True",
                KTokenClass::Try => "Try",
                KTokenClass::TypeAlias => "TypeAlias",
                KTokenClass::Val => "Val",
                KTokenClass::Value => "Value",
                KTokenClass::Var => "Var",
                KTokenClass::Vararg => "Vararg",
                KTokenClass::When => "When",
                KTokenClass::Where => "Where",
                KTokenClass::While => "While",
                KTokenClass::EOF => "EOF",
            };
        write!(f, "{}", str)?;
        Ok(())
    }
}
