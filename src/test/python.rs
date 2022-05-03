#[cfg(test)]
use super::super::python;
#[cfg(test)]
use super::super::Parser;

#[test]
fn python_idented_block_test() {
    let parser: Parser<String, char, Vec<String>> = Parser::match_string("Foo:")
        .seq2(&python::newline())
        .seq_right(&python::indented_block(python::number()))
        .seq_left(&Parser::eof());
    let input = r#"Foo:
    1
    2
    3
"#;
    let r = parser.run_str(input);
    assert_eq!(r, Ok(vec!["1".to_owned(), "2".to_owned(), "3".to_owned()]))
}

#[test]
fn python_expression_test() {
    let parser = python::expression().seq_left(&Parser::eof());
    {
        let input = "42 if 5 > 4 else 7";
        let r = parser.run_str(input);
        assert!(match r {
            Ok(_) => true,
            _ => false,
        });
    }
    {
        let input = "lambda a b: a+b";
        let r = parser.run_str(input);
        assert!(match r {
            Ok(_) => true,
            _ => false,
        });
    }
}

#[test]
fn python_inversion_test() {
    let parser = python::inversion().seq_left(&Parser::eof());
    let input = "not not not True";
    let r = parser.run_str(input);
    println!("{:?}", r);
    assert_eq!(
        r,
        Ok(python::Inversion {
            not_count: 3,
            comparison: python::Comparison {
                bitwise_or: python::BitwiseOr {
                    bitwise_xors: vec![python::BitwiseXor {
                        bitwise_ands: vec![python::BitwiseAnd {
                            shift_exprs: vec![python::ShiftExpr {
                                sum: python::Sum {
                                    term: python::Term {
                                        factor: python::Factor::Power(Box::new(python::Power {
                                            a: python::AwaitPrimary {
                                                is_await: false,
                                                primary: python::Primary::Atom(python::Atom::True)
                                            },
                                            b_op: None
                                        })),
                                        operations: vec![]
                                    },
                                    operations: vec![]
                                },
                                operations: vec![]
                            }]
                        }]
                    }]
                },
                operations: vec![]
            }
        })
    )
}

#[test]
fn python_shift_expr_test() {
    let parser = python::shift_expr().seq_left(&Parser::eof());
    let input = "2 << 3 >> 1";
    let r = parser.run_str(input);
    assert_eq!(
        r,
        Ok(python::ShiftExpr {
            sum: python::Sum {
                term: python::Term {
                    factor: python::Factor::Power(Box::new(python::Power {
                        a: python::AwaitPrimary {
                            is_await: false,
                            primary: python::Primary::Atom(python::Atom::Number("2".into()))
                        },
                        b_op: None,
                    })),
                    operations: Vec::new(),
                },
                operations: Vec::new(),
            },
            operations: vec![
                (
                    python::ShiftOperator::ShiftLeft,
                    python::Sum {
                        term: python::Term {
                            factor: python::Factor::Power(Box::new(python::Power {
                                a: python::AwaitPrimary {
                                    is_await: false,
                                    primary: python::Primary::Atom(python::Atom::Number(
                                        "3".into()
                                    ))
                                },
                                b_op: None,
                            })),
                            operations: Vec::new(),
                        },
                        operations: Vec::new(),
                    }
                ),
                (
                    python::ShiftOperator::ShiftRight,
                    python::Sum {
                        term: python::Term {
                            factor: python::Factor::Power(Box::new(python::Power {
                                a: python::AwaitPrimary {
                                    is_await: false,
                                    primary: python::Primary::Atom(python::Atom::Number(
                                        "1".into()
                                    ))
                                },
                                b_op: None,
                            })),
                            operations: Vec::new(),
                        },
                        operations: Vec::new(),
                    }
                )
            ]
        })
    )
}

#[test]
fn python_term_test() {
    let parser = python::term().seq_left(&Parser::eof());
    let input = "2 * 3";
    let r = parser.run_str(input);
    assert_eq!(
        r,
        Ok(python::Term {
            factor: python::Factor::Power(Box::new(python::Power {
                a: python::AwaitPrimary {
                    is_await: false,
                    primary: python::Primary::Atom(python::Atom::Number("2".into()))
                },
                b_op: None,
            })),
            operations: vec![(
                python::TermOperator::Multiply,
                python::Factor::Power(Box::new(python::Power {
                    a: python::AwaitPrimary {
                        is_await: false,
                        primary: python::Primary::Atom(python::Atom::Number("3".into()))
                    },
                    b_op: None,
                }))
            )]
        })
    );
}

#[test]
fn python_power_test() {
    let parser = python::power().seq_left(&Parser::eof());
    let input = "12 ** 12";
    let r = parser.run_str(input);
    assert_eq!(
        r,
        Ok(python::Power {
            a: python::AwaitPrimary {
                is_await: false,
                primary: python::Primary::Atom(python::Atom::Number("12".into()))
            },
            b_op: Some(python::Factor::Power(Box::new(python::Power {
                a: python::AwaitPrimary {
                    is_await: false,
                    primary: python::Primary::Atom(python::Atom::Number("12".into()))
                },
                b_op: None
            })))
        })
    );
}

#[test]
fn python_atom_name_test() {
    let parser = python::atom().seq_left(&Parser::eof());
    let input = "var_1";
    let r = parser.run_str(input);
    assert_eq!(r, Ok(python::Atom::Name("var_1".into())));
}

#[test]
fn python_atom_number_test() {
    let parser = python::atom().seq_left(&Parser::eof());
    let input = "12";
    let r = parser.run_str(input);
    assert_eq!(r, Ok(python::Atom::Number("12".into())));
}

#[test]
fn python_parse_name_test() {
    let parser = python::name().seq_left(&Parser::eof());
    let input = "variable_1";
    let r = parser.run_str(input);
    assert_eq!(r, Ok("variable_1".to_owned()));
}

#[test]
fn python_parse_number_test() {
    let parser = python::number().seq_left(&Parser::eof());
    let input = "123.456";
    let r = parser.run_str(input);
    assert_eq!(r, Ok("123.456".to_owned()));
}

#[test]
fn python_parse_complex_number_test() {
    let parser = python::complex_number().seq_left(&Parser::eof());
    let input = "3 + 4j";
    let r = parser.run_str(input);
    assert_eq!(r, Ok(("3".to_owned(), "4".to_owned())));
}
