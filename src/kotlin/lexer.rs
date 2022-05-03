// https://kotlinlang.org/docs/reference/grammar.html
use super::super::choice_lazy;
use super::super::kotlin;
use super::super::Parser;
use super::ShebangLine;

pub fn shebang_line() -> Parser<String, char, ShebangLine> {
    Parser::match_string("#!")
        .seq_right(
            &Parser::satisfy(|t| *t != '\r' && *t != '\n')
                .zero_or_more_vec()
                .return_string(),
        )
        .map(|content| ShebangLine { content })
}

pub fn delimited_comment() -> Parser<String, char, ()> {
    Parser::match_string("/*")
        .seq2(&choice_lazy!(delimited_comment(), Parser::any().map_to(()),))
        .seq2(&Parser::match_string("*/"))
        .map_to(())
}

pub fn line_comment() -> Parser<String, char, ()> {
    Parser::match_string("//")
        .seq2(&Parser::satisfy(|t| *t != '\r' && *t != '\n').zero_or_more_vec())
        .map_to(())
}

pub fn ws() -> Parser<String, char, char> {
    Parser::satisfy(|t| *t == '\u{0020}' || *t == '\u{0009}' || *t == '\u{000C}')
}

pub fn nl() -> Parser<String, char, ()> {
    choice_lazy!(
        Parser::match_('\n').map_to(()),
        Parser::match_('\r')
            .seq2(&Parser::match_('\n').optional())
            .map_to(())
    )
}

pub fn hidden() -> Parser<String, char, ()> {
    choice_lazy!(delimited_comment(), line_comment(), ws().map_to(()),)
}

pub fn seq2_skip<A: Clone + 'static, B: 'static, Skip: Clone + 'static>(
    parser1: &Parser<String, char, A>,
    parser2: &Parser<String, char, B>,
    skip_p: &Parser<String, char, Skip>,
) -> Parser<String, char, (A, B)> {
    let skip_p = skip_p.clone();
    parser1.seq_left(&skip_p.zero_or_more_vec()).seq2(parser2)
}

pub fn seq2_skip_ws<A: Clone + 'static, B: 'static>(
    parser1: &Parser<String, char, A>,
    parser2: &Parser<String, char, B>,
) -> Parser<String, char, (A, B)> {
    seq2_skip(parser1, parser2, &hidden())
}

pub fn seq_left_skip_ws<A: Clone + 'static, B: 'static>(
    parser1: &Parser<String, char, A>,
    parser2: &Parser<String, char, B>,
) -> Parser<String, char, A> {
    seq2_skip_ws(parser1, parser2).map(|(a, _b)| a)
}

pub fn seq_right_skip_ws<A: Clone + 'static, B: 'static>(
    parser1: &Parser<String, char, A>,
    parser2: &Parser<String, char, B>,
) -> Parser<String, char, B> {
    seq2_skip_ws(parser1, parser2).map(|(_a, b)| b)
}

pub fn seq2_skip_ws_nl<A: Clone + 'static, B: 'static>(
    parser1: &Parser<String, char, A>,
    parser2: &Parser<String, char, B>,
) -> Parser<String, char, (A, B)> {
    seq2_skip(parser1, parser2, &choice_lazy!(hidden(), nl()))
}

pub fn seq_left_skip_ws_nl<A: Clone + 'static, B: 'static>(
    parser1: &Parser<String, char, A>,
    parser2: &Parser<String, char, B>,
) -> Parser<String, char, A> {
    seq2_skip_ws_nl(parser1, parser2).map(|(a, _b)| a)
}

pub fn seq_right_skip_ws_nl<A: Clone + 'static, B: 'static>(
    parser1: &Parser<String, char, A>,
    parser2: &Parser<String, char, B>,
) -> Parser<String, char, B> {
    seq2_skip_ws_nl(parser1, parser2).map(|(_a, b)| b)
}

pub fn zero_or_more_vec_skip<A: Clone + 'static, Skip: Clone + 'static>(
    parser: &Parser<String, char, A>,
    skip_parser: &Parser<String, char, Skip>,
) -> Parser<String, char, Vec<A>> {
    let parser = parser.clone();
    let skip_parser = skip_parser.clone();
    Parser::choice(vec![
        Parser::lazy(move || one_or_more_vec_skip(&parser, &skip_parser)),
        Parser::empty().map(|_| Vec::new()),
    ])
}

pub fn one_or_more_vec_skip<A: Clone + 'static, Skip: Clone + 'static>(
    parser: &Parser<String, char, A>,
    skip_parser: &Parser<String, char, Skip>,
) -> Parser<String, char, Vec<A>> {
    let skip_parser = skip_parser.clone();
    let parser2 = parser.clone();
    parser
        .seq2(
            &skip_parser
                .zero_or_more_vec()
                .seq_right(&parser2)
                .zero_or_more_vec(),
        )
        .map(|(x, mut xs): (A, Vec<A>)| {
            xs.insert(0, x);
            xs
        })
}

pub fn zero_or_more_vec_skip_ws<
    A: Clone + 'static,
    MakeP: FnMut() -> Parser<String, char, A> + 'static,
>(
    mut make_parser: MakeP,
) -> Parser<String, char, Vec<A>> {
    zero_or_more_vec_skip(&make_parser(), &hidden())
}

pub fn one_or_more_vec_skip_ws<
    A: Clone + 'static,
    MakeP: FnMut() -> Parser<String, char, A> + 'static,
>(
    mut make_parser: MakeP,
) -> Parser<String, char, Vec<A>> {
    one_or_more_vec_skip(&make_parser(), &hidden())
}

pub fn zero_or_more_vec_skip_ws_nl<
    A: Clone + 'static,
    MakeP: FnMut() -> Parser<String, char, A> + 'static,
>(
    mut make_parser: MakeP,
) -> Parser<String, char, Vec<A>> {
    zero_or_more_vec_skip(&make_parser(), &choice_lazy!(hidden(), nl()))
}

pub fn one_or_more_vec_skip_ws_nl<
    A: Clone + 'static,
    MakeP: FnMut() -> Parser<String, char, A> + 'static,
>(
    mut make_parser: MakeP,
) -> Parser<String, char, Vec<A>> {
    one_or_more_vec_skip(&make_parser(), &choice_lazy!(hidden(), nl()))
}

pub fn zero_or_more_sep_by_skip<A: Clone + 'static, Sep: Clone + 'static, Skip: Clone + 'static>(
    parser: &Parser<String, char, A>,
    sep_parser: &Parser<String, char, Sep>,
    skip_parser: &Parser<String, char, Skip>,
) -> Parser<String, char, Vec<A>> {
    let parser = parser.clone();
    let sep_parser = sep_parser.clone();
    let skip_parser = skip_parser.clone();
    Parser::choice(vec![
        Parser::lazy(move || one_or_more_sep_by_skip(&parser, &sep_parser, &skip_parser)),
        Parser::empty().map(|_| Vec::new()),
    ])
}

pub fn one_or_more_sep_by_skip<A: Clone + 'static, Sep: Clone + 'static, Skip: Clone + 'static>(
    parser: &Parser<String, char, A>,
    sep_parser: &Parser<String, char, Sep>,
    skip_parser: &Parser<String, char, Skip>,
) -> Parser<String, char, Vec<A>> {
    parser
        .seq2(
            &skip_parser
                .zero_or_more_vec()
                .seq2(&sep_parser)
                .seq2(&skip_parser.zero_or_more_vec())
                .seq_right(&parser)
                .zero_or_more_vec(),
        )
        .map(|(x, mut xs): (A, Vec<A>)| {
            xs.insert(0, x);
            xs
        })
}

pub fn zero_or_more_sep_by_skip_ws<A: Clone + 'static, Sep: Clone + 'static>(
    parser: &Parser<String, char, A>,
    sep_parser: &Parser<String, char, Sep>,
) -> Parser<String, char, Vec<A>> {
    zero_or_more_sep_by_skip(parser, sep_parser, &hidden())
}

pub fn one_or_more_sep_by_skip_ws<A: Clone + 'static, Sep: Clone + 'static>(
    parser: &Parser<String, char, A>,
    sep_parser: &Parser<String, char, Sep>,
) -> Parser<String, char, Vec<A>> {
    one_or_more_sep_by_skip(parser, sep_parser, &hidden())
}

pub fn zero_or_more_sep_by_skip_ws_nl<A: Clone + 'static, Sep: Clone + 'static>(
    parser: &Parser<String, char, A>,
    sep_parser: &Parser<String, char, Sep>,
) -> Parser<String, char, Vec<A>> {
    zero_or_more_sep_by_skip(parser, sep_parser, &choice_lazy!(hidden(), nl()))
}

pub fn one_or_more_sep_by_skip_ws_nl<A: Clone + 'static, Sep: Clone + 'static>(
    parser: &Parser<String, char, A>,
    sep_parser: &Parser<String, char, Sep>,
) -> Parser<String, char, Vec<A>> {
    one_or_more_sep_by_skip(parser, sep_parser, &choice_lazy!(hidden(), nl()))
}

pub fn reserved() -> Parser<String, char, ()> {
    Parser::match_string("...")
}

pub fn dot() -> Parser<String, char, char> {
    Parser::match_('.')
}

pub fn comma() -> Parser<String, char, char> {
    Parser::match_(',')
}

pub fn lparen() -> Parser<String, char, char> {
    Parser::match_('(')
}

pub fn rparen() -> Parser<String, char, char> {
    Parser::match_(')')
}

pub fn lsquare() -> Parser<String, char, char> {
    Parser::match_('[')
}

pub fn rsquare() -> Parser<String, char, char> {
    Parser::match_(']')
}

pub fn lcurl() -> Parser<String, char, char> {
    Parser::match_('{')
}

pub fn rcurl() -> Parser<String, char, char> {
    Parser::match_('}')
}

pub fn mult() -> Parser<String, char, char> {
    Parser::match_('*')
}

pub fn mod_() -> Parser<String, char, char> {
    Parser::match_('%')
}

pub fn div() -> Parser<String, char, char> {
    Parser::match_('/')
}

pub fn add() -> Parser<String, char, char> {
    Parser::match_('+')
}

pub fn sub() -> Parser<String, char, char> {
    Parser::match_('-')
}

pub fn incr() -> Parser<String, char, ()> {
    Parser::match_string("++")
}

pub fn decr() -> Parser<String, char, ()> {
    Parser::match_string("--")
}

pub fn conj() -> Parser<String, char, ()> {
    Parser::match_string("&&")
}

pub fn disj() -> Parser<String, char, ()> {
    Parser::match_string("||")
}

pub fn excl_ws() -> Parser<String, char, ()> {
    Parser::match_('!').seq2(&hidden()).map_to(())
}

pub fn excl_no_ws() -> Parser<String, char, char> {
    Parser::match_('!')
}

pub fn colon() -> Parser<String, char, char> {
    Parser::match_(':')
}

pub fn semicolon() -> Parser<String, char, char> {
    Parser::match_(';')
}

pub fn assignment() -> Parser<String, char, char> {
    Parser::match_('=')
}

pub fn add_assignment() -> Parser<String, char, ()> {
    Parser::match_string("+=")
}

pub fn sub_assignment() -> Parser<String, char, ()> {
    Parser::match_string("-=")
}

pub fn mult_assignment() -> Parser<String, char, ()> {
    Parser::match_string("*=")
}

pub fn div_assignment() -> Parser<String, char, ()> {
    Parser::match_string("/=")
}

pub fn mod_assignment() -> Parser<String, char, ()> {
    Parser::match_string("%=")
}

pub fn arrow() -> Parser<String, char, ()> {
    Parser::match_string("->")
}

pub fn double_arrow() -> Parser<String, char, ()> {
    Parser::match_string("=>")
}

pub fn range() -> Parser<String, char, ()> {
    Parser::match_string("..")
}

pub fn coloncolon() -> Parser<String, char, ()> {
    Parser::match_string("::")
}

pub fn double_semicolon() -> Parser<String, char, ()> {
    Parser::match_string(";;")
}

pub fn hash() -> Parser<String, char, char> {
    Parser::match_('#')
}

pub fn at_no_ws() -> Parser<String, char, char> {
    Parser::match_('@')
}

pub fn at_post_ws() -> Parser<String, char, ()> {
    Parser::match_('@')
        .seq2(&choice_lazy!(hidden(), nl()))
        .map_to(())
}

pub fn at_pre_ws() -> Parser<String, char, ()> {
    choice_lazy!(hidden(), nl())
        .seq2(&Parser::match_('@'))
        .map_to(())
}

pub fn at_both_ws() -> Parser<String, char, ()> {
    choice_lazy!(hidden(), nl())
        .seq2(&Parser::match_('@'))
        .seq2(&choice_lazy!(hidden(), nl()))
        .map_to(())
}

pub fn quest_ws() -> Parser<String, char, ()> {
    Parser::match_('?').seq2(&hidden()).map_to(())
}

pub fn quest_no_ws() -> Parser<String, char, char> {
    Parser::match_('?')
}

pub fn langle() -> Parser<String, char, char> {
    Parser::match_('<')
}

pub fn rangle() -> Parser<String, char, char> {
    Parser::match_('>')
}

pub fn le() -> Parser<String, char, ()> {
    Parser::match_string("<=")
}

pub fn ge() -> Parser<String, char, ()> {
    Parser::match_string(">=")
}

pub fn excl_eq() -> Parser<String, char, ()> {
    Parser::match_string("!=")
}

pub fn excl_eqeq() -> Parser<String, char, ()> {
    Parser::match_string("!==")
}

pub fn as_safe() -> Parser<String, char, ()> {
    Parser::match_string("as?")
}

pub fn eqeq() -> Parser<String, char, ()> {
    Parser::match_string("==")
}

pub fn eqeqeq() -> Parser<String, char, ()> {
    Parser::match_string("===")
}

pub fn single_quote() -> Parser<String, char, ()> {
    Parser::match_('\'').map_to(())
}

pub fn return_at() -> Parser<String, char, ()> {
    Parser::match_string("return@")
}

pub fn continue_at() -> Parser<String, char, ()> {
    Parser::match_string("continue@")
}

pub fn break_at() -> Parser<String, char, ()> {
    Parser::match_string("break@")
}

pub fn this_at() -> Parser<String, char, ()> {
    Parser::match_string("this@")
}

pub fn super_at() -> Parser<String, char, ()> {
    Parser::match_string("super@")
}

pub fn file() -> Parser<String, char, ()> {
    Parser::match_string("file")
}

pub fn field() -> Parser<String, char, ()> {
    Parser::match_string("field")
}

pub fn property() -> Parser<String, char, ()> {
    Parser::match_string("property")
}

pub fn get() -> Parser<String, char, ()> {
    Parser::match_string("get")
}

pub fn set() -> Parser<String, char, ()> {
    Parser::match_string("set")
}

pub fn receiver() -> Parser<String, char, ()> {
    Parser::match_string("receiver")
}

pub fn param() -> Parser<String, char, ()> {
    Parser::match_string("param")
}

pub fn setparam() -> Parser<String, char, ()> {
    Parser::match_string("setparam")
}

pub fn delegate() -> Parser<String, char, ()> {
    Parser::match_string("delegate")
}

pub fn package() -> Parser<String, char, ()> {
    Parser::match_string("package")
}

pub fn import() -> Parser<String, char, ()> {
    Parser::match_string("import")
}

pub fn class() -> Parser<String, char, ()> {
    Parser::match_string("class")
}

pub fn interface() -> Parser<String, char, ()> {
    Parser::match_string("interface")
}

pub fn fun() -> Parser<String, char, ()> {
    Parser::match_string("fun")
}

pub fn object() -> Parser<String, char, ()> {
    Parser::match_string("object")
}

pub fn val() -> Parser<String, char, ()> {
    Parser::match_string("val")
}

pub fn var() -> Parser<String, char, ()> {
    Parser::match_string("var")
}

pub fn typealias() -> Parser<String, char, ()> {
    Parser::match_string("typealias")
}

pub fn constructor() -> Parser<String, char, ()> {
    Parser::match_string("constructor")
}

pub fn by() -> Parser<String, char, ()> {
    Parser::match_string("by")
}

pub fn companion() -> Parser<String, char, ()> {
    Parser::match_string("companion")
}

pub fn init() -> Parser<String, char, ()> {
    Parser::match_string("init")
}

pub fn this() -> Parser<String, char, ()> {
    Parser::match_string("this")
}

pub fn super_() -> Parser<String, char, ()> {
    Parser::match_string("super")
}

pub fn typeof_() -> Parser<String, char, ()> {
    Parser::match_string("typeof")
}

pub fn where_() -> Parser<String, char, ()> {
    Parser::match_string("where")
}

pub fn if_() -> Parser<String, char, ()> {
    Parser::match_string("if")
}

pub fn else_() -> Parser<String, char, ()> {
    Parser::match_string("else")
}

pub fn when() -> Parser<String, char, ()> {
    Parser::match_string("when")
}

pub fn try_() -> Parser<String, char, ()> {
    Parser::match_string("try")
}

pub fn catch() -> Parser<String, char, ()> {
    Parser::match_string("catch")
}

pub fn finally() -> Parser<String, char, ()> {
    Parser::match_string("finally")
}

pub fn for_() -> Parser<String, char, ()> {
    Parser::match_string("for")
}

pub fn do_() -> Parser<String, char, ()> {
    Parser::match_string("do")
}

pub fn while_() -> Parser<String, char, ()> {
    Parser::match_string("while")
}

pub fn throw() -> Parser<String, char, ()> {
    Parser::match_string("throw")
}

pub fn return_() -> Parser<String, char, ()> {
    Parser::match_string("return")
}

pub fn continue_() -> Parser<String, char, ()> {
    Parser::match_string("continue")
}

pub fn break_() -> Parser<String, char, ()> {
    Parser::match_string("break")
}

pub fn as_() -> Parser<String, char, ()> {
    Parser::match_string("as")
}

pub fn is() -> Parser<String, char, ()> {
    Parser::match_string("is")
}

pub fn in_() -> Parser<String, char, ()> {
    Parser::match_string("in")
}

pub fn not_is() -> Parser<String, char, ()> {
    Parser::match_string("!is")
        .seq2(&choice_lazy!(hidden(), nl()))
        .map_to(())
}

pub fn not_in() -> Parser<String, char, ()> {
    Parser::match_string("!in")
        .seq2(&choice_lazy!(hidden(), nl()))
        .map_to(())
}

pub fn out() -> Parser<String, char, ()> {
    Parser::match_string("out")
}

pub fn dynamic() -> Parser<String, char, ()> {
    Parser::match_string("dynamic")
}

pub fn public() -> Parser<String, char, ()> {
    Parser::match_string("public")
}

pub fn private() -> Parser<String, char, ()> {
    Parser::match_string("private")
}

pub fn protected() -> Parser<String, char, ()> {
    Parser::match_string("protected")
}

pub fn internal() -> Parser<String, char, ()> {
    Parser::match_string("internal")
}

pub fn enum_() -> Parser<String, char, ()> {
    Parser::match_string("enum")
}

pub fn sealed() -> Parser<String, char, ()> {
    Parser::match_string("sealed")
}

pub fn annotation() -> Parser<String, char, ()> {
    Parser::match_string("annotation")
}

pub fn data() -> Parser<String, char, ()> {
    Parser::match_string("data")
}

pub fn inner() -> Parser<String, char, ()> {
    Parser::match_string("inner")
}

pub fn value() -> Parser<String, char, ()> {
    Parser::match_string("value")
}

pub fn tailrec() -> Parser<String, char, ()> {
    Parser::match_string("tailrec")
}

pub fn operator() -> Parser<String, char, ()> {
    Parser::match_string("operator")
}

pub fn inline() -> Parser<String, char, ()> {
    Parser::match_string("inline")
}

pub fn infix() -> Parser<String, char, ()> {
    Parser::match_string("infix")
}

pub fn external() -> Parser<String, char, ()> {
    Parser::match_string("external")
}

pub fn suspend() -> Parser<String, char, ()> {
    Parser::match_string("suspend")
}

pub fn override_() -> Parser<String, char, ()> {
    Parser::match_string("override")
}

pub fn abstract_() -> Parser<String, char, ()> {
    Parser::match_string("abstract")
}

pub fn final_() -> Parser<String, char, ()> {
    Parser::match_string("final")
}

pub fn open() -> Parser<String, char, ()> {
    Parser::match_string("open")
}

pub fn const_() -> Parser<String, char, ()> {
    Parser::match_string("const")
}

pub fn lateinit() -> Parser<String, char, ()> {
    Parser::match_string("lateinit")
}

pub fn vararg() -> Parser<String, char, ()> {
    Parser::match_string("vararg")
}

pub fn noinline() -> Parser<String, char, ()> {
    Parser::match_string("noinline")
}

pub fn crossinline() -> Parser<String, char, ()> {
    Parser::match_string("crossinline")
}

pub fn reified() -> Parser<String, char, ()> {
    Parser::match_string("reified")
}

pub fn expect() -> Parser<String, char, ()> {
    Parser::match_string("expect")
}

pub fn actual() -> Parser<String, char, ()> {
    Parser::match_string("actual")
}

pub fn dec_digit() -> Parser<String, char, char> {
    Parser::satisfy(|t| '0' <= *t && *t <= '9')
}

pub fn dec_digit_no_zero() -> Parser<String, char, char> {
    Parser::satisfy(|t| '1' <= *t && *t <= '9')
}

pub fn dec_digit_or_separator() -> Parser<String, char, char> {
    choice_lazy!(dec_digit(), Parser::match_('_'),)
}

pub fn dec_digits() -> Parser<String, char, String> {
    choice_lazy!(
        dec_digit()
            .seq2(&dec_digit_or_separator().zero_or_more_vec())
            .seq2(&dec_digit())
            .map_to(()),
        dec_digit().map_to(())
    )
    .return_string()
}

pub fn double_exponent() -> Parser<String, char, String> {
    Parser::satisfy(|t| *t == 'e' || *t == 'E')
        .seq2(&Parser::satisfy(|t| *t == '+' || *t == '-').optional())
        .seq2(&dec_digits())
        .return_string()
}

pub fn real_literal() -> Parser<String, char, String> {
    choice_lazy!(float_literal(), double_literal(),)
}

pub fn float_literal() -> Parser<String, char, String> {
    choice_lazy!(double_literal(), dec_digits())
        .seq2(&Parser::satisfy(|t| *t == 'f' || *t == 'F'))
        .return_string()
}

pub fn double_literal() -> Parser<String, char, String> {
    choice_lazy!(
        dec_digits()
            .optional()
            .seq2(&Parser::match_('.'))
            .seq2(&dec_digits())
            .seq2(&double_exponent().optional())
            .map_to(()),
        dec_digits().seq2(&double_exponent()).map_to(())
    )
    .return_string()
}

pub fn integer_literal() -> Parser<String, char, String> {
    choice_lazy!(
        dec_digit_no_zero()
            .seq2(&dec_digit_or_separator().zero_or_more_vec())
            .seq2(&dec_digit())
            .map_to(()),
        dec_digit().map_to(())
    )
    .return_string()
}

pub fn hex_literal() -> Parser<String, char, String> {
    Parser::match_('0')
        .seq2(&Parser::satisfy(|t| *t == 'x' || *t == 'X'))
        .seq2(&choice_lazy!(
            hex_digit()
                .seq2(&hex_digit_or_separator().zero_or_more_vec())
                .seq2(&hex_digit())
                .map_to(()),
            hex_digit().map_to(())
        ))
        .return_string()
}

pub fn bin_digit() -> Parser<String, char, char> {
    choice_lazy!(Parser::match_('0'), Parser::match_('1'),)
}

pub fn bin_digit_or_separator() -> Parser<String, char, char> {
    choice_lazy!(bin_digit(), Parser::match_('_'),)
}

pub fn bin_literal() -> Parser<String, char, String> {
    Parser::match_('0')
        .seq2(&Parser::satisfy(|t| *t == 'b' || *t == 'B'))
        .seq2(&choice_lazy!(
            bin_digit()
                .seq2(&bin_digit_or_separator().zero_or_more_vec())
                .seq2(&bin_digit())
                .map_to(()),
            bin_digit().map_to(())
        ))
        .return_string()
}

pub fn unsigned_literal() -> Parser<String, char, String> {
    choice_lazy!(integer_literal(), hex_literal(), bin_literal())
        .seq2(&Parser::satisfy(|t| *t == 'u' || *t == 'U'))
        .seq2(&Parser::satisfy(|t| *t == 'l' || *t == 'L').optional())
        .return_string()
}

pub fn long_literal() -> Parser<String, char, String> {
    choice_lazy!(integer_literal(), hex_literal(), bin_literal())
        .seq2(&Parser::satisfy(|t| *t == 'u' || *t == 'U'))
        .return_string()
}

pub fn boolean_literal() -> Parser<String, char, bool> {
    choice_lazy!(
        Parser::match_string("true").map_to(true),
        Parser::match_string("false").map_to(false),
    )
}

pub fn null_literal() -> Parser<String, char, ()> {
    Parser::match_string("null")
}

pub fn character_literal() -> Parser<String, char, char> {
    Parser::match_('\'')
        .seq_right(
            &choice_lazy!(
                escape_seq().map_to(()),
                Parser::satisfy(|t| *t != '\n' && *t != '\r' && *t != '\\').map_to(())
            )
            .return_string()
            .map(|x: String| x.chars().nth(0).unwrap()),
        )
        .seq_left(&Parser::match_('\''))
}

pub fn escape_seq() -> Parser<String, char, char> {
    choice_lazy!(uni_character_literal(), escaped_identifier(),)
}

pub fn uni_character_literal() -> Parser<String, char, char> {
    Parser::match_('\\')
        .seq_right(&Parser::match_('u'))
        .seq_right(
            &hex_digit()
                .seq2(&hex_digit())
                .seq2(&hex_digit())
                .seq2(&hex_digit())
                .map(|(((d1, d2), d3), d4)| {
                    char::from_u32(
                        u32::from_str_radix(&format!("{}{}{}{}", d1, d2, d3, d4), 16).unwrap(),
                    )
                    .unwrap()
                }),
        )
}

pub fn hex_digit() -> Parser<String, char, char> {
    Parser::satisfy(|t| '0' <= *t && *t <= '9' || 'a' <= *t && *t <= 'f' || 'A' <= *t && *t <= 'F')
}

pub fn hex_digit_or_separator() -> Parser<String, char, char> {
    choice_lazy!(hex_digit(), Parser::match_('_'),)
}

pub fn escaped_identifier() -> Parser<String, char, char> {
    Parser::match_('\\').seq_right(&choice_lazy!(
        Parser::match_('t').map_to('\t'),
        Parser::match_('b').map_to('\u{0008}'),
        Parser::match_('r').map_to('\r'),
        Parser::match_('n').map_to('\n'),
        Parser::match_('\'').map_to('\''),
        Parser::match_('"').map_to('"'),
        Parser::match_('\\').map_to('\\'),
        Parser::match_('$').map_to('$')
    ))
}

pub fn identifier() -> Parser<String, char, String> {
    choice_lazy!(
        choice_lazy!(letter().map_to(()), Parser::match_('_').map_to(()))
            .seq2(
                &choice_lazy!(
                    letter().map_to(()),
                    Parser::match_('_').map_to(()),
                    unicode_digit().map_to(())
                )
                .zero_or_more_vec()
            )
            .return_string(),
        Parser::match_('`')
            .seq_right(
                &Parser::satisfy(|t| { *t != '\r' && *t != '\n' && *t != '`' })
                    .zero_or_more_vec()
                    .return_string()
            )
            .seq_left(&Parser::match_('`'))
    )
}

pub fn identifier_or_soft_key() -> Parser<String, char, String> {
    choice_lazy!(
        identifier(),
        abstract_().map_to("abstract".to_owned()),
        annotation().map_to("annotation".to_owned()),
        by().map_to("by".to_owned()),
        catch().map_to("catch".to_owned()),
        companion().map_to("companion".to_owned()),
        constructor().map_to("constructor".to_owned()),
        crossinline().map_to("crossinline".to_owned()),
        data().map_to("data".to_owned()),
        dynamic().map_to("dynamic".to_owned()),
        enum_().map_to("enum".to_owned()),
        external().map_to("external".to_owned()),
        final_().map_to("final".to_owned()),
        finally().map_to("finally".to_owned()),
        import().map_to("import".to_owned()),
        infix().map_to("infix".to_owned()),
        init().map_to("init".to_owned()),
        inline().map_to("inline".to_owned()),
        inner().map_to("inner".to_owned()),
        internal().map_to("internal".to_owned()),
        lateinit().map_to("lateinit".to_owned()),
        noinline().map_to("noinline".to_owned()),
        open().map_to("open".to_owned()),
        operator().map_to("operator".to_owned()),
        out().map_to("out".to_owned()),
        override_().map_to("override".to_owned()),
        private().map_to("private".to_owned()),
        protected().map_to("protected".to_owned()),
        public().map_to("public".to_owned()),
        reified().map_to("reified".to_owned()),
        sealed().map_to("sealed".to_owned()),
        tailrec().map_to("tailrec".to_owned()),
        vararg().map_to("vararg".to_owned()),
        where_().map_to("where".to_owned()),
        get().map_to("get".to_owned()),
        set().map_to("set".to_owned()),
        field().map_to("field".to_owned()),
        property().map_to("property".to_owned()),
        receiver().map_to("receiver".to_owned()),
        param().map_to("param".to_owned()),
        setparam().map_to("setparam".to_owned()),
        delegate().map_to("delegate".to_owned()),
        file().map_to("file".to_owned()),
        expect().map_to("expect".to_owned()),
        actual().map_to("actual".to_owned()),
        value().map_to("value".to_owned()),
        const_().map_to("const".to_owned()),
        suspend().map_to("suspend".to_owned()),
    )
}

pub fn field_identifier() -> Parser<String, char, String> {
    Parser::match_('$').seq_right(&identifier_or_soft_key())
}

pub fn letter() -> Parser<String, char, char> {
    choice_lazy!(
        kotlin::unicode_classes::unicode_class_lu(),
        kotlin::unicode_classes::unicode_class_ll(),
        kotlin::unicode_classes::unicode_class_lt(),
        kotlin::unicode_classes::unicode_class_lm(),
        kotlin::unicode_classes::unicode_class_lo(),
    )
    .return_string()
    .map(|x: String| x.chars().nth(0).unwrap())
}

pub fn unicode_digit() -> Parser<String, char, char> {
    super::unicode_classes::unicode_class_nd()
        .return_string()
        .map(|x: String| x.chars().nth(0).unwrap())
}

pub fn quote() -> Parser<String, char, char> {
    Parser::match_('"')
}

pub fn triple_quote() -> Parser<String, char, ()> {
    Parser::match_string("\"\"\"").map_to(())
}

pub fn line_str_ref() -> Parser<String, char, String> {
    field_identifier()
}

pub fn line_str_text() -> Parser<String, char, String> {
    Parser::choice(vec![
        Parser::satisfy(|t| *t != '\\' && *t != '"' && *t != '$')
            .map_to(())
            .one_or_more_vec()
            .map_to(()),
        Parser::match_('$').map_to(()),
    ])
    .return_string()
}

pub fn line_str_escaped_char() -> Parser<String, char, char> {
    Parser::choice(vec![escaped_identifier(), uni_character_literal()])
}

pub fn line_str_expr_start() -> Parser<String, char, ()> {
    Parser::match_string("${")
}

pub fn multi_line_string_quote() -> Parser<String, char, usize> {
    Parser::match_('"')
        .map_to(())
        .zero_or_more_vec()
        .map(|xs| xs.len())
}

pub fn multi_line_str_ref() -> Parser<String, char, String> {
    field_identifier()
}

pub fn multi_line_str_text() -> Parser<String, char, String> {
    Parser::choice(vec![
        Parser::satisfy(|t| *t != '"' && *t != '$')
            .map_to(())
            .one_or_more_vec()
            .map_to(()),
        Parser::match_('$').map_to(()),
    ])
    .return_string()
}

pub fn multi_line_str_expr_start() -> Parser<String, char, ()> {
    Parser::match_string("${")
}
