// WIP
// https://docs.python.org/3/reference/grammar.html
// version 3.10.2

use std::cell::RefCell;
use std::rc::Rc;

use super::Parser;

const KEYWORDS: [&'static str; 5] = ["else", "if", "lambda", "True", "False"];

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Lambdef {
    params: Vec<LambdaParam>,
    body: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LambdaParam {
    name: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
    IfElse {
        then: Disjunction,
        cond: Disjunction,
        else_: Box<Expression>,
    },
    Disjunction(Disjunction),
    Lambdef(Lambdef),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Disjunction {
    pub conjunctions: Vec<Conjunction>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Conjunction {
    pub inversions: Vec<Inversion>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Inversion {
    pub not_count: usize,
    pub comparison: Comparison,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Comparison {
    pub bitwise_or: BitwiseOr,
    pub operations: Vec<(CompareOp, BitwiseOr)>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CompareOp {
    Eq,
    NotEq,
    Lte,
    Lt,
    Gte,
    Gt,
    NotIn,
    In,
    IsNot,
    Is,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BitwiseOr {
    pub bitwise_xors: Vec<BitwiseXor>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BitwiseXor {
    pub bitwise_ands: Vec<BitwiseAnd>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BitwiseAnd {
    pub shift_exprs: Vec<ShiftExpr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ShiftExpr {
    pub sum: Sum,
    pub operations: Vec<(ShiftOperator, Sum)>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ShiftOperator {
    ShiftLeft,
    ShiftRight,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Sum {
    pub term: Term,
    pub operations: Vec<(SumOperator, Term)>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SumOperator {
    Add,
    Subtract,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Term {
    pub factor: Factor,
    pub operations: Vec<(TermOperator, Factor)>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TermOperator {
    Multiply,
    Divide,
    DivideThenRoundInt,
    Modulus,
    MatrixMultiply,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Factor {
    Plus(Box<Factor>),
    Minus(Box<Factor>),
    Tilda(Box<Factor>),
    Power(Box<Power>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Power {
    pub a: AwaitPrimary,
    pub b_op: Option<Factor>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AwaitPrimary {
    pub is_await: bool,
    pub primary: Primary,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Primary {
    //DotName(..),
    //GenExp(..),
    //Call(..),
    //Index(..),
    Atom(Atom),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Atom {
    Name(String),
    True,
    False,
    None,
    //Strings(..),
    Number(String),
    // Tuple,
    // Group,
    // GenExp,
    // List,
    // ListComp,
    // Dict,
    // Set,
    // DictComp,
    // SetComp(..),
    TripleDot,
}

pub fn indented_block<A: Clone + 'static>(
    after_ident_line_parser: Parser<String, char, A>,
) -> Parser<String, char, Vec<A>> {
    let ws2: Parser<String, char, char> =
        Parser::unordered_choice(vec![Parser::match_(' '), Parser::match_('\t')]);
    let ws_count = Rc::new(RefCell::new(0));
    let count_ws;
    {
        let ws_count = ws_count.clone();
        count_ws = ws2.one_or_more_vec().map(|x| x.len()).map(move |x| {
            *ws_count.borrow_mut() = x;
            return ();
        });
    }
    count_ws
        .seq_right(&after_ident_line_parser.seq_left(&newline()))
        .seq2(
            &Parser::lazy(move || ws2.exactly_vec(*ws_count.borrow()).map_to(()))
                .seq_right(&after_ident_line_parser.seq_left(&newline()))
                .zero_or_more_vec(),
        )
        .map(|(x, mut xs)| {
            xs.insert(0, x);
            return xs;
        })
}

pub fn expression() -> Parser<String, char, Expression> {
    Parser::unordered_choice(vec![
        disjunction()
            .seq2(
                &ws_one_or_more()
                    .seq2(&Parser::match_string("if"))
                    .seq2(&ws_one_or_more())
                    .seq_right(&disjunction())
                    .seq_left(&ws_one_or_more())
                    .seq_left(&Parser::match_string("else"))
                    .seq_left(&ws_one_or_more())
                    .seq2(&Parser::lazy(expression))
                    .optional(),
            )
            .map(|(disjunction, if_op)| {
                if let Some((cond, else_)) = if_op {
                    Expression::IfElse {
                        then: disjunction,
                        cond,
                        else_: Box::new(else_),
                    }
                } else {
                    Expression::Disjunction(disjunction)
                }
            }),
        lambdef().map(Expression::Lambdef),
    ])
}

pub fn lambdef() -> Parser<String, char, Lambdef> {
    Parser::match_string("lambda")
        .seq_right(
            &ws_one_or_more()
                .seq_right(&lambda_param())
                .zero_or_more_vec(),
        )
        .seq2(&annotation())
        .map(|(params, body)| Lambdef {
            params,
            body: Box::new(body),
        })
}

pub fn lambda_param() -> Parser<String, char, LambdaParam> {
    name().map(|name| LambdaParam { name })
}

pub fn annotation() -> Parser<String, char, Expression> {
    Parser::match_(':')
        .seq2(&ws_zero_or_more())
        .seq_right(&Parser::lazy(expression))
}

pub fn default() -> Parser<String, char, Expression> {
    Parser::match_('=')
        .seq2(&ws_zero_or_more())
        .seq_right(&Parser::lazy(expression))
}

pub fn disjunction() -> Parser<String, char, Disjunction> {
    // XXX: Hack to reduce stack depth
    let conjunction2 = Parser::lazy(conjunction);
    //
    conjunction2
        .seq2(
            &ws_one_or_more()
                .seq2(&Parser::match_string("or"))
                .seq2(&ws_one_or_more())
                .seq_right(&conjunction2)
                .zero_or_more_vec(),
        )
        .map(|(x, mut xs)| {
            xs.insert(0, x);
            Disjunction { conjunctions: xs }
        })
}

pub fn conjunction() -> Parser<String, char, Conjunction> {
    inversion()
        .seq2(
            &ws_one_or_more()
                .seq2(&Parser::match_string("and"))
                .seq2(&ws_one_or_more())
                .seq_right(&inversion())
                .zero_or_more_vec(),
        )
        .map(|(x, mut xs)| {
            xs.insert(0, x);
            Conjunction { inversions: xs }
        })
}

pub fn inversion() -> Parser<String, char, Inversion> {
    Parser::match_string("not")
        .seq_right(
            &ws_one_or_more()
                .seq2(&Parser::match_string("not"))
                .zero_or_more_vec()
                .map(|xs| xs.len()),
        )
        .seq_left(&ws())
        .map(|count| count + 1)
        .optional()
        .seq2(&comparison())
        .map(|(count_op, comparison)| Inversion {
            not_count: count_op.unwrap_or(0),
            comparison,
        })
}

pub fn comparison() -> Parser<String, char, Comparison> {
    bitwise_or()
        .seq2(
            &compare_op()
                .seq_left(&ws_zero_or_more())
                .seq2(&bitwise_or())
                .zero_or_more_vec(),
        )
        .map(|(bitwise_or, operations)| Comparison {
            bitwise_or,
            operations,
        })
}

pub fn compare_op() -> Parser<String, char, CompareOp> {
    Parser::unordered_choice(vec![
        ws_zero_or_more()
            .seq2(&Parser::match_string("=="))
            .map(|_| CompareOp::Eq),
        ws_zero_or_more()
            .seq2(&Parser::match_string("!="))
            .map(|_| CompareOp::NotEq),
        ws_zero_or_more()
            .seq2(&Parser::match_string("<="))
            .map(|_| CompareOp::Lte),
        ws_zero_or_more()
            .seq2(&Parser::match_string("<"))
            .map(|_| CompareOp::Lt),
        ws_zero_or_more()
            .seq2(&Parser::match_string(">="))
            .map(|_| CompareOp::Gte),
        ws_zero_or_more()
            .seq2(&Parser::match_string(">"))
            .map(|_| CompareOp::Gt),
        ws_one_or_more()
            .seq2(&Parser::match_string("not"))
            .seq2(&ws_one_or_more())
            .seq2(&Parser::match_string("in"))
            .seq2(&ws())
            .map(|_| CompareOp::NotIn),
        ws_one_or_more()
            .seq2(&Parser::match_string("in"))
            .seq2(&ws())
            .map(|_| CompareOp::In),
        ws_one_or_more()
            .seq2(&Parser::match_string("is"))
            .seq2(&ws_one_or_more())
            .seq2(&Parser::match_string("not"))
            .seq2(&ws())
            .map(|_| CompareOp::IsNot),
        ws_zero_or_more()
            .seq2(&Parser::match_string(">"))
            .map(|_| CompareOp::Gt),
        ws_one_or_more()
            .seq2(&Parser::match_string("not"))
            .seq2(&ws_one_or_more())
            .seq2(&Parser::match_string("in"))
            .seq2(&ws())
            .map(|_| CompareOp::NotIn),
        ws_one_or_more()
            .seq2(&Parser::match_string("in"))
            .seq2(&ws())
            .map(|_| CompareOp::In),
        ws_one_or_more()
            .seq2(&Parser::match_string("is"))
            .seq2(&ws_one_or_more())
            .seq2(&Parser::match_string("not"))
            .seq2(&ws())
            .map(|_| CompareOp::IsNot),
        ws_one_or_more()
            .seq2(&Parser::match_string("is"))
            .seq2(&ws())
            .map(|_| CompareOp::Is),
    ])
}

pub fn bitwise_or() -> Parser<String, char, BitwiseOr> {
    bitwise_xor()
        .seq2(
            &ws_zero_or_more()
                .seq2(&Parser::match_('|'))
                .seq_right(&bitwise_xor())
                .zero_or_more_vec(),
        )
        .map(|(x, mut xs)| {
            xs.insert(0, x);
            BitwiseOr { bitwise_xors: xs }
        })
}

pub fn bitwise_xor() -> Parser<String, char, BitwiseXor> {
    bitwise_and()
        .seq2(
            &ws_zero_or_more()
                .seq2(&Parser::match_('^'))
                .seq_right(&bitwise_and())
                .zero_or_more_vec(),
        )
        .map(|(x, mut xs)| {
            xs.insert(0, x);
            BitwiseXor { bitwise_ands: xs }
        })
}

pub fn bitwise_and() -> Parser<String, char, BitwiseAnd> {
    shift_expr()
        .seq2(
            &ws_zero_or_more()
                .seq2(&Parser::match_('&'))
                .seq_right(&shift_expr())
                .zero_or_more_vec(),
        )
        .map(|(x, mut xs)| {
            xs.insert(0, x);
            BitwiseAnd { shift_exprs: xs }
        })
}

pub fn shift_expr() -> Parser<String, char, ShiftExpr> {
    sum()
        .seq2(
            &ws_zero_or_more()
                .seq_right(&shift_operator())
                .seq_left(&ws_zero_or_more())
                .seq2(&sum())
                .zero_or_more_vec(),
        )
        .map(|(sum, operations)| ShiftExpr { sum, operations })
}

pub fn shift_operator() -> Parser<String, char, ShiftOperator> {
    Parser::unordered_choice(vec![
        Parser::match_string("<<").map(|_| ShiftOperator::ShiftLeft),
        Parser::match_string(">>").map(|_| ShiftOperator::ShiftRight),
    ])
}

pub fn sum() -> Parser<String, char, Sum> {
    term()
        .seq2(
            &ws_zero_or_more()
                .seq_right(&sum_operator())
                .seq_left(&ws_zero_or_more())
                .seq2(&term())
                .zero_or_more_vec(),
        )
        .map(|(term, operations)| Sum { term, operations })
}

pub fn sum_operator() -> Parser<String, char, SumOperator> {
    Parser::unordered_choice(vec![
        Parser::match_('+').map(|_| SumOperator::Add),
        Parser::match_('-').map(|_| SumOperator::Subtract),
    ])
}

pub fn term() -> Parser<String, char, Term> {
    factor()
        .seq2(
            &ws_zero_or_more()
                .seq_right(&term_operator())
                .seq_left(&ws_zero_or_more())
                .seq2(&factor())
                .zero_or_more_vec(),
        )
        .map(|(factor, operations)| Term { factor, operations })
}

pub fn term_operator() -> Parser<String, char, TermOperator> {
    Parser::unordered_choice(vec![
        Parser::match_('*').map(|_| TermOperator::Multiply),
        Parser::match_string("//").map(|_| TermOperator::DivideThenRoundInt),
        Parser::match_('/').map(|_| TermOperator::Divide),
        Parser::match_('%').map(|_| TermOperator::Modulus),
        Parser::match_('@').map(|_| TermOperator::MatrixMultiply),
    ])
}

pub fn factor() -> Parser<String, char, Factor> {
    Parser::unordered_choice(vec![
        Parser::match_('+')
            .seq_right(&Parser::lazy(factor))
            .map(|factor| Factor::Plus(Box::new(factor))),
        Parser::match_('-')
            .seq_right(&Parser::lazy(factor))
            .map(|factor| Factor::Minus(Box::new(factor))),
        Parser::match_('~')
            .seq_right(&Parser::lazy(factor))
            .map(|factor| Factor::Tilda(Box::new(factor))),
        Parser::lazy(power).map(|power| Factor::Power(Box::new(power))),
    ])
}

pub fn power() -> Parser<String, char, Power> {
    Parser::unordered_choice(vec![
        await_primary()
            .seq_left(&ws_zero_or_more().seq2(&Parser::match_string("**").seq2(&ws_zero_or_more())))
            .seq2(&factor())
            .map(|(await_primary, factor)| Power {
                a: await_primary,
                b_op: Some(factor),
            }),
        await_primary().map(|await_primary| Power {
            a: await_primary,
            b_op: None,
        }),
    ])
}

pub fn await_primary() -> Parser<String, char, AwaitPrimary> {
    Parser::unordered_choice(vec![
        await_().seq_right(&primary()).map(|primary| AwaitPrimary {
            is_await: true,
            primary,
        }),
        primary().map(|primary| AwaitPrimary {
            is_await: false,
            primary,
        }),
    ])
}

pub fn primary() -> Parser<String, char, Primary> {
    atom().map(Primary::Atom)
}

pub fn atom() -> Parser<String, char, Atom> {
    Parser::unordered_choice(vec![
        true_().map(|_| Atom::True),
        false_().map(|_| Atom::False),
        none().map(|_| Atom::None),
        number().map(Atom::Number),
        triple_dot().map(|_| Atom::TripleDot),
        name().map(Atom::Name),
    ])
}

pub fn name() -> Parser<String, char, String> {
    Parser::satisfy(|t| 'a' <= *t && *t <= 'z' || 'A' <= *t && *t <= 'Z' || *t == '_')
        .seq2(
            &Parser::satisfy(|t| {
                'a' <= *t && *t <= 'z'
                    || 'A' <= *t && *t <= 'Z'
                    || *t == '_'
                    || '0' <= *t && *t <= '9'
            })
            .zero_or_more_vec(),
        )
        .map(|(c, mut chars)| {
            chars.insert(0, c);
            chars.iter().cloned().collect::<String>()
        })
        .filter(
            |name| KEYWORDS.iter().all(|keyword| name != *keyword),
            "Keywored used as name.".into(),
        )
}

pub fn await_() -> Parser<String, char, ()> {
    Parser::match_string("await")
}

pub fn true_() -> Parser<String, char, ()> {
    Parser::match_string("True")
}

pub fn false_() -> Parser<String, char, ()> {
    Parser::match_string("False")
}

pub fn none() -> Parser<String, char, ()> {
    Parser::match_string("None")
}

pub fn complex_number() -> Parser<String, char, (String, String)> {
    signed_real_number().seq2(&ws_zero_or_more().seq_right(&Parser::unordered_choice(vec![
                Parser::match_('+')
                    .seq2(&ws_zero_or_more())
                    .seq_right(&imaginary_number()),
                Parser::match_('-')
                    .seq2(&ws_zero_or_more())
                    .seq_right(&imaginary_number())
                    .map(|x| format!("-{}", x)),
            ])))
}

pub fn signed_number() -> Parser<String, char, String> {
    Parser::unordered_choice(vec![
        Parser::match_('-')
            .seq2(&number())
            .map(|(_, x)| format!("-{}", x)),
        number(),
    ])
}

pub fn signed_real_number() -> Parser<String, char, String> {
    Parser::unordered_choice(vec![
        Parser::match_('-')
            .seq2(&real_number())
            .map(|(_, x)| format!("-{}", x)),
        real_number(),
    ])
}

pub fn real_number() -> Parser<String, char, String> {
    number()
}

pub fn imaginary_number() -> Parser<String, char, String> {
    number()
        .seq2(&Parser::unordered_choice(vec![
            Parser::match_('j'),
            Parser::match_('J'),
        ]))
        .map(|(x, _)| x)
}

pub fn number() -> Parser<String, char, String> {
    Parser::unordered_choice(vec![
        Parser::satisfy(|t| '0' <= *t && *t <= '9')
            .one_or_more_vec()
            .seq2(
                &Parser::match_('.')
                    .seq2(&Parser::satisfy(|t| '0' <= *t && *t <= '9').zero_or_more_vec())
                    .optional(),
            )
            .map(|(a, b)| {
                let x: String;
                if let Some((_, c)) = b {
                    x = a.iter().chain(['.'].iter()).chain(c.iter()).collect();
                } else {
                    x = a.iter().collect();
                }
                return x;
            }),
        Parser::match_('.')
            .seq2(&Parser::satisfy(|t| '0' <= *t && *t <= '9').one_or_more_vec())
            .map(|(_, a)| {
                let x: String = ['.'].iter().chain(a.iter()).collect();
                return x;
            }),
    ])
}

pub fn triple_dot() -> Parser<String, char, ()> {
    Parser::match_string("...")
}

pub fn ws_zero_or_more() -> Parser<String, char, ()> {
    ws().zero_or_more_vec().map_to(())
}

pub fn ws_one_or_more() -> Parser<String, char, ()> {
    ws().one_or_more_vec().map_to(())
}

pub fn newline() -> Parser<String, char, ()> {
    Parser::unordered_choice(vec![
        Parser::match_string("\r\n"),    // <-- Windows
        Parser::match_('\n').map_to(()), // <--Linux
        Parser::match_('\r').map_to(()), // <-- Mac
    ])
}

pub fn ws() -> Parser<String, char, ()> {
    Parser::unordered_choice(vec![
        Parser::match_(' ').map_to(()),
        Parser::match_('\t').map_to(()),
        Parser::match_string("\\\n"),
    ])
}
